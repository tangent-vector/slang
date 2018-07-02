// main.cpp

// The code for this example is similar to the earlier `01-hello-world`
// example, and as such the comments will gloss over parts of the code
// that were covered previously.
//

// We still need to include the Slang header to use the Slang API
#include <slang.h>

// We will again make use of a simple graphics API abstraction
// layer, just to keep the examples short and to the point.
#include "slang-graphics/model.h"
#include "slang-graphics/render.h"
#include "slang-graphics/render-d3d11.h"
#include "slang-graphics/vector-math.h"
#include "slang-graphics/window.h"
using namespace slang_graphics;

#include <vector>

// The steps for loading a shader program are the same as in the
// previous example.
//
ShaderProgram* loadShaderProgram(Renderer* renderer)
{
    SlangSession* slangSession = spCreateSession(NULL);
    SlangCompileRequest* slangRequest = spCreateCompileRequest(slangSession);

    int targetIndex = spAddCodeGenTarget(slangRequest, SLANG_DXBC);
    spSetTargetProfile(slangRequest, targetIndex, spFindProfile(slangSession, "sm_4_0"));

    int translationUnitIndex = spAddTranslationUnit(slangRequest, SLANG_SOURCE_LANGUAGE_SLANG, nullptr);
    spAddTranslationUnitSourceFile(slangRequest, translationUnitIndex, "shaders.slang");

    char const* vertexEntryPointName    = "vertexMain";
    char const* fragmentEntryPointName  = "fragmentMain";
    int vertexIndex   = spAddEntryPoint(slangRequest, translationUnitIndex, vertexEntryPointName,   SLANG_STAGE_VERTEX);
    int fragmentIndex = spAddEntryPoint(slangRequest, translationUnitIndex, fragmentEntryPointName, SLANG_STAGE_FRAGMENT);

    int compileErr = spCompile(slangRequest);
    if(auto diagnostics = spGetDiagnosticOutput(slangRequest))
    {
        reportError("%s", diagnostics);
    }
    if(compileErr)
    {
        spDestroyCompileRequest(slangRequest);
        spDestroySession(slangSession);
        return nullptr;
    }

    ISlangBlob* vertexShaderBlob = nullptr;
    spGetEntryPointCodeBlob(slangRequest, vertexIndex, 0, &vertexShaderBlob);

    ISlangBlob* fragmentShaderBlob = nullptr;
    spGetEntryPointCodeBlob(slangRequest, fragmentIndex, 0, &fragmentShaderBlob);

    char const* vertexCode = (char const*) vertexShaderBlob->getBufferPointer();
    char const* vertexCodeEnd = vertexCode + vertexShaderBlob->getBufferSize();

    char const* fragmentCode = (char const*) fragmentShaderBlob->getBufferPointer();
    char const* fragmentCodeEnd = fragmentCode + fragmentShaderBlob->getBufferSize();

    spDestroyCompileRequest(slangRequest);
    spDestroySession(slangSession);

    ShaderProgram::KernelDesc kernelDescs[] =
    {
        { StageType::Vertex,    vertexCode,     vertexCodeEnd},
        { StageType::Fragment,  fragmentCode,   fragmentCodeEnd},
    };

    ShaderProgram::Desc programDesc;
    programDesc.pipelineType = PipelineType::Graphics;
    programDesc.kernels = &kernelDescs[0];
    programDesc.kernelCount = 2;

    ShaderProgram* shaderProgram = renderer->createProgram(programDesc);

    vertexShaderBlob->release();
    fragmentShaderBlob->release();

    return shaderProgram;
}

static int gWindowWidth = 1024;
static int gWindowHeight = 768;

// We will need to pass in both camera/transformation information and
// lighting information, so we will declare a constant buffer type here.
//
// Note: this type must match the layout of the `Uniform` struct that
// is declared in `shaders.slang`.
//
struct Uniforms
{
    glm::mat4x4 viewProjection;

    glm::vec3   lightDir;
    float       pad0;

    glm::vec3   lightColor;
    float       pad1;

    glm::vec3   materialDiffuseColor;
    float       pad2;

    glm::mat4x4 modelTransform;
    glm::mat4x4 inverseTransposeModelTransform;
};

enum Status
{
    OKAY,
    FAILURE,
};


// Global variables for the various platform and graphics API objects
// that our application needs:
//
ApplicationContext* gAppContext;
Window* gWindow;
Renderer* gRenderer;
InputLayout* gInputLayout;
BufferResource* gConstantBuffer;
TextureResource* gDepthBuffer;
ShaderProgram* gShaderProgram;
BindingState* gBindingState;
GraphicsPipelineState*  gPSO;

std::vector<Model*> gModels;

void loadModel(
    Renderer*           renderer,
    char const*         inputPath,
    Model::LoadFlags    loadFlags = 0,
    float               scale = 1.0f)
{
    auto model = new Model();

    if(model->initialize(renderer, inputPath, loadFlags, scale) != OKAY)
    {
        return;
    }

    gModels.emplace_back(model);
}

int initialize()
{
    // Create a window for our application to render into.
    WindowDesc windowDesc;
    windowDesc.title = "Model Viewer";
    windowDesc.width = gWindowWidth;
    windowDesc.height = gWindowHeight;
    gWindow = createWindow(windowDesc);

    // Initialize the rendering layer.
    //
    // Note: for now we are hard-coding logic to use the
    // Direct3D11 back-end for the graphics API abstraction.
    // A future version of this example may support multiple
    // platforms/APIs.
    //
    gRenderer = createD3D11Renderer();
    Renderer::Desc rendererDesc;
    rendererDesc.width = gWindowWidth;
    rendererDesc.height = gWindowHeight;
    gRenderer->initialize(rendererDesc, getPlatformWindowHandle(gWindow));

    // Create a constant buffer for passing the model-view-projection matrix.
    //
    // TODO: A future version of this example will show how to
    // use the Slang reflection API to query the required size
    // for the data in this constant buffer.
    //
    int constantBufferSize = 16 * sizeof(float);

    BufferResource::Desc constantBufferDesc;
    constantBufferDesc.init(constantBufferSize);
    constantBufferDesc.setDefaults(Resource::Usage::ConstantBuffer);
    constantBufferDesc.cpuAccessFlags = Resource::AccessFlag::Write;

    gConstantBuffer = gRenderer->createBufferResource(
        Resource::Usage::ConstantBuffer,
        constantBufferDesc);
    if(!gConstantBuffer) return FAILURE;

    // Input Assembler (IA)

    // Input Layout

    InputElementDesc inputElements[] = {
        {"POSITION", 0, Format::RGB_Float32, offsetof(Model::Vertex, position) },
        {"NORMAL",   0, Format::RGB_Float32, offsetof(Model::Vertex, normal) },
        {"UV",       0, Format::RG_Float32,  offsetof(Model::Vertex, uv) },
    };
    gInputLayout = gRenderer->createInputLayout(
        &inputElements[0],
        3);
    if(!gInputLayout) return FAILURE;

    // Depth-Stencil Test (DS)
    {
        TextureResource::Desc depthBufferDesc;
        depthBufferDesc.init2D(Resource::Type::Texture2D, Format::D_Float32, gWindowWidth, gWindowHeight, 1);
        depthBufferDesc.setDefaults(Resource::Usage::DepthWrite);
        gDepthBuffer = gRenderer->createTextureResource(
            Resource::Usage::DepthWrite,
            depthBufferDesc);
    }

    // Shaders (VS, PS, ...)

    gShaderProgram = loadShaderProgram(gRenderer);
    if(!gShaderProgram) return FAILURE;

    // Fixed function state
    {
        GraphicsPipelineState::Desc psoDesc;

        // Fill in what we need

        gPSO = gRenderer->createGraphicsPipelineState(psoDesc);
    }

    // Resource binding state

    BindingState::Desc bindingStateDesc;
    bindingStateDesc.addBufferResource(gConstantBuffer, BindingState::RegisterRange::makeSingle(0));
    gBindingState = gRenderer->createBindingState(bindingStateDesc);

    // Load model(s)

    loadModel(gRenderer, "CornellBox-Original.obj", Model::LoadFlag::FlipWinding);
//    loadModel(gRenderer, "cube.obj");
//    loadModel(gRenderer, "teapot.obj");
    loadModel(gRenderer, "bumpy-teapot.obj", Model::LoadFlag::FlipWinding, 0.01f);

    // Once we've initialized all the graphics API objects,
    // it is time to show our application window and start rendering.

    showWindow(gWindow);

    return OKAY;
}

void renderFrame()
{
    static uint64_t lastTime = getCurrentTime();
    uint64_t currentTime = getCurrentTime();

    float deltaTime = float(currentTime - lastTime) / float(getTimerFrequency());

    lastTime = currentTime;

    // Clear our framebuffer (color target only)
    //
    static const float kClearColor[] = { 0.25, 0.25, 0.25, 1.0 };
    gRenderer->setClearColor(kClearColor);
    gRenderer->clearFrame();

    glm::mat4x4 projection = glm::perspective(
        glm::radians(60.0f),
        float(gWindowWidth) / float(gWindowHeight),
        0.1f,
        1000.0f);

    glm::mat4x4 identity = glm::mat4x4(1.0f);

    glm::mat4x4 view = identity;
    view = translate(view, glm::vec3(0, 0, -5));

    glm::mat4x4 viewProjection = projection * view;

    glm::vec3 lightDir = normalize(glm::vec3(10, 10, -10));
    glm::vec3 lightColor = glm::vec3(1, 1, 1);


    // We will animate the light source a bit in order to make the scene dynamic
    static float angle = 0.0f;
    angle += 0.5f * deltaTime;

    glm::mat4x4 lightTransform = identity;
    lightTransform = rotate(lightTransform, angle, glm::vec3(0, 1, 0));

    lightDir = glm::vec3(lightTransform * glm::vec4(lightDir, 0));

    glm::vec3 materialDiffuseColor = glm::vec3(1, 0.5f, 0.5f);

    glm::mat4x4 modelTransform = identity;
    glm::mat4x4 inverseTransposeModelTransform = inverse(transpose(modelTransform));

    // Input Assembler (IA)

    gRenderer->setInputLayout(gInputLayout);
    gRenderer->setPrimitiveTopology(PrimitiveTopology::TriangleList);

    // Depth-Stencil Test (DS)

    gRenderer->setDepthStencilTarget(gDepthBuffer);

    // General fixed-function state

    gRenderer->setGraphicsPipelineState(gPSO);

    for(auto& model : gModels)
    {
        UInt vertexStride = sizeof(Model::Vertex);
        UInt vertexBufferOffset = 0;
        gRenderer->setVertexBuffers(0, 1, &model->vertexBuffer, &vertexStride, &vertexBufferOffset);

        gRenderer->setIndexBuffer(model->indexBuffer, Format::R_UInt32);

        // Vertex Shader (VS)
        // Pixel Shader (PS)

        gRenderer->setShaderProgram(gShaderProgram);
        gRenderer->setBindingState(gBindingState);

        //

        for(auto& mesh : model->meshes)
        {
            // For this example, we are updating our main constant buffer
            // for every single mesh that we draw, because there might
            // be a change in material color. A more optimized renderer
            // will group shader parameters according to the rates at
            // which they change.
            //
            if(Uniforms* data = (Uniforms*) gRenderer->map(gConstantBuffer, MapFlavor::WriteDiscard))
            {
                data->viewProjection = viewProjection;
                data->modelTransform = modelTransform;
                data->inverseTransposeModelTransform = inverseTransposeModelTransform;

                data->lightDir = lightDir;
                data->lightColor = lightColor;

                data->materialDiffuseColor = mesh->material->diffuseColor;

                gRenderer->unmap(gConstantBuffer);
            }
            //
            // TODO: does changing the contents of the constant buffer
            // mean we need to re-bind it?

            gRenderer->drawIndexed(mesh->indexCount, mesh->firstIndex);
        }
    }

    gRenderer->presentFrame();
}

void finalize()
{
    // TODO: Proper cleanup.
}

// This "inner" main function is used by the platform abstraction
// layer to deal with differences in how an entry point needs
// to be defined for different platforms.
//
void innerMain(ApplicationContext* context)
{
    if(initialize() != OKAY)
    {
        exitApplication(context, 1);
    }

    while(dispatchEvents(context))
    {
        renderFrame();
    }

    finalize();
}

// This macro instantiates an appropriate main function to
// invoke the `innerMain` above.
//
SG_UI_MAIN(innerMain)
