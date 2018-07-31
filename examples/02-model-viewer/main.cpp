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

#include <memory>
#include <vector>

// For this example we will be using the Slang reflection API
// to help us allocate parameter blocks required by the shader
// code, and also to generate specialized variants of the
// shaders as needed.
//

struct ShaderLibrary
{
    std::string                 inputPath;
    RefPtr<Renderer>            renderer;
    SlangCompileRequest*        slangRequest;
    slang::ShaderReflection*    slangReflection;
};

struct EntryPointX
{
    std::string name;
    SlangStage  slangStage;
    StageType   apiStage;
};

struct GenericParam
{
    int parameterBlockIndex;
};

struct Program
{
    ShaderLibrary*              shaderLibrary;
    int                         parameterBlockCount;

    std::vector<EntryPointX*>   entryPoints;
    std::vector<GenericParam*>  genericParams;
};

SlangSession* getSlangSession()
{
    static SlangSession* slangSession = spCreateSession(NULL);
    return slangSession;
}

ShaderLibrary* loadShaderLibrary(Renderer* renderer, char const* inputPath)
{
    auto slangSession = getSlangSession();

    SlangCompileRequest* slangRequest = spCreateCompileRequest(slangSession);

    // When *loading* the shader library, we will not request concrete
    // code to be generated, and will instead generate code for kernels
    // on-demand as we see specialized kernels referenced.
    spSetCompileFlags(
        slangRequest,
        SLANG_COMPILE_FLAG_NO_CODEGEN);

    int targetIndex = spAddCodeGenTarget(slangRequest, SLANG_DXBC);
    spSetTargetProfile(slangRequest, targetIndex, spFindProfile(slangSession, "sm_4_0"));

    int translationUnitIndex = spAddTranslationUnit(slangRequest, SLANG_SOURCE_LANGUAGE_SLANG, nullptr);
    spAddTranslationUnitSourceFile(slangRequest, translationUnitIndex, inputPath);

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

    auto slangReflection = (slang::ShaderReflection*) spGetReflection(slangRequest);

    ShaderLibrary* library = new ShaderLibrary();
    library->renderer = renderer;
    library->inputPath = inputPath;
    library->slangRequest = slangRequest;
    library->slangReflection = slangReflection;
    return library;
}

EntryPointX* loadEntryPointX(
    ShaderLibrary*  library,
    char const*     name)
{
    auto slangReflection = library->slangReflection;

    auto slangEntryPoint = slangReflection->findEntryPointByName(name);
    if(!slangEntryPoint) return nullptr;

    auto slangStage = slangEntryPoint->getStage();
    StageType apiStage = StageType::Unknown;
    switch(slangStage)
    {
    default:
        return nullptr;

    case SLANG_STAGE_VERTEX:    apiStage = StageType::Vertex;   break;
    case SLANG_STAGE_FRAGMENT:  apiStage = StageType::Fragment; break;
    }

    EntryPointX* entryPoint = new EntryPointX();
    entryPoint->name = name;
    entryPoint->slangStage = slangEntryPoint->getStage();
    entryPoint->apiStage = apiStage;
    return entryPoint;
}

Program* loadProgram(
    ShaderLibrary*      library,
    int                 entryPointCount,
    const char* const*  entryPointNames)
{
    auto slangReflection = library->slangReflection;

    Program* program = new Program();
    program->shaderLibrary = library;

    for(int ee = 0; ee < entryPointCount; ++ee)
    {
        auto entryPoint = loadEntryPointX(library, entryPointNames[ee]);
        if(!entryPoint)
            return nullptr;
        program->entryPoints.push_back(entryPoint);
    }

    auto genericParamCount = slangReflection->getTypeParameterCount();
    for(int pp = 0; pp < genericParamCount; ++pp)
    {
        auto slangGenericParam = slangReflection->getTypeParameterByIndex(pp);

        auto genericParam = new GenericParam();
        program->genericParams.push_back(genericParam);
    }

    // We want to specialize our shaders based on what gets bound
    // in parameter blocks, so we will scan the shader parameters
    // looking for `ParameterBlock<G>` where `G` is one of our
    // generic type parameters.
    auto paramCount = slangReflection->getParameterCount();
    int parameterBlockCounter = 0;
    for(int pp = 0; pp < paramCount; ++pp)
    {
        auto slangParam = slangReflection->getParameterByIndex(pp);
        if(slangParam->getType()->getKind() == slang::TypeReflection::Kind::ParameterBlock)
        {
            int parameterBlockIndex = parameterBlockCounter++;

            auto slangElementTypeLayout = slangParam->getTypeLayout()->getElementTypeLayout();
            if(slangElementTypeLayout->getKind() == slang::TypeReflection::Kind::GenericTypeParameter)
            {
                auto genericParamIndex = slangElementTypeLayout->getGenericParamIndex();
                program->genericParams[genericParamIndex]->parameterBlockIndex = parameterBlockIndex;
            }
        }
    }
    program->parameterBlockCount = parameterBlockCounter;

    return program;
}

Program* loadProgram(ShaderLibrary* library, char const* entryPoint0, char const* entryPoint1)
{
    char const* entryPointNames[] = { entryPoint0, entryPoint1 };
    return loadProgram(library, 2, entryPointNames);
}

static int gWindowWidth = 1024;
static int gWindowHeight = 768;

// For this more complex example we will be passing multiple
// parameter blocks into the shader code, and each will
// need its own `struct` type the define the layout of the
// uniform data.
//
struct PerView
{
    glm::mat4x4 viewProjection;

    glm::vec3   lightDir;
    float       pad0;

    glm::vec3   lightColor;
    float       pad1;
};
struct PerModel
{
    glm::mat4x4 modelTransform;
    glm::mat4x4 inverseTransposeModelTransform;
};

enum Status
{
    OKAY,
    FAILURE,
};

//

// The Slang API does not implement the host side version
// of a parameter block, and instead leaves it to the application
// to implement this abstraction with the aid of Slang's reflection
// information.
//
struct ParameterBlock;

struct ParameterBlockLayout
{
    Renderer*                       renderer;
    slang::TypeLayoutReflection*    slangTypeLayout;
    size_t                          primaryConstantBufferSize;
    DescriptorSetLayout*            descriptorSetLayout;
};

struct ParameterBlock
{
    Renderer*               renderer;
    ParameterBlockLayout*   layout;
    BufferResource*         primaryConstantBuffer;
    DescriptorSet*          descriptorSet;

    void* map();
    void unmap();

    template<typename T>
    T* mapAs() { return (T*)map(); }
};

ParameterBlockLayout* getParameterBlockLayout(
    ShaderLibrary*  library,
    char const*     name)
{
    auto slangReflection = library->slangReflection;
    auto renderer = library->renderer;

    auto type = slangReflection->findTypeByName(name);
    if(!type) return nullptr;

    auto typeLayout = slangReflection->getTypeLayout(type);
    if(!typeLayout) return nullptr;

    // If the type that is going in the parameter block has
    // any uniform data in it (as opposed to resources), then
    // a constant buffer will be needed to hold that data.
    //
    // In turn any resource parameters would need to go into
    // the descriptor set *after* this constant buffer.
    //
    size_t primaryConstantBufferSize = typeLayout->getSize(SLANG_PARAMETER_CATEGORY_UNIFORM);

    // We need to use the Slang reflection information to
    // create an API-level descriptor-set layout that
    // is compatible with the original declaration.
    //
    std::vector<DescriptorSetLayout::SlotRangeDesc> slotRanges;

    // If the type has any uniform data, then the descriptor set
    // will need a constant buffer to be the first thing it stores.
    //
    // Note: for a renderer only targetting D3D12, it might make
    // sense to allocate this "primary" constant buffer as a root
    // descriptor instead of inside the descriptor set (or at least
    // do this *if* there are no non-uniform parameters). Policy
    // decisions like that are up to the application, not Slang.
    // This example application just does something simple.
    //
    if(primaryConstantBufferSize)
    {
        slotRanges.push_back(
            DescriptorSetLayout::SlotRangeDesc(
                DescriptorSlotType::UniformBuffer));
    }

    DescriptorSetLayout::Desc descriptorSetLayoutDesc;
    descriptorSetLayoutDesc.slotRangeCount = slotRanges.size();
    descriptorSetLayoutDesc.slotRanges = slotRanges.data();

    DescriptorSetLayout* descriptorSetLayout = renderer->createDescriptorSetLayout(descriptorSetLayoutDesc);

    auto parameterBlockLayout = new ParameterBlockLayout();
    parameterBlockLayout->renderer = renderer;
    parameterBlockLayout->primaryConstantBufferSize = primaryConstantBufferSize;
    parameterBlockLayout->slangTypeLayout = typeLayout;
    parameterBlockLayout->descriptorSetLayout = descriptorSetLayout;
    return parameterBlockLayout;
}

ParameterBlock* allocatePersistentParameterBlock(
    ParameterBlockLayout*   layout);

ParameterBlock* allocateTransientParameterBlock(
    ParameterBlockLayout*   layout);

//
struct Effect
{
    // The shader program entry point(s) to execute
    Program* program;

    // We store basic state in the form of a desc
    GraphicsPipelineStateDesc desc;
};

// Global variables for the various platform and graphics API objects
// that our application needs:
//
ApplicationContext* gAppContext;
Window* gWindow;
Renderer* gRenderer;
ResourceView* gDepthTarget;

Effect* gEffect;

ParameterBlockLayout* gPerViewParameterBlockLayout;
ParameterBlockLayout* gPerModelParameterBlockLayout;

struct Material
{
    virtual ParameterBlock* createParameterBlock() = 0;
    ParameterBlock* parameterBlock;
};

struct SimpleMaterial : Material
{
    struct Uniforms
    {
        glm::vec3   diffuseColor;
        float       pad;
    };

    Uniforms uniforms;

    static ParameterBlockLayout* gParameterBlockLayout;

    ParameterBlock* createParameterBlock() override
    {
        auto parameterBlockLayout = gParameterBlockLayout;
        auto parameterBlock = allocatePersistentParameterBlock(
            parameterBlockLayout);

        if(auto u = parameterBlock->mapAs<Uniforms>())
        {
            *u = uniforms;
            parameterBlock->unmap();
        }

        return parameterBlock;
    }
};

ParameterBlockLayout* SimpleMaterial::gParameterBlockLayout = nullptr;

struct Mesh
{
    Material* material;
    int firstIndex;
    int indexCount;
};

struct Model
{
    typedef ModelLoader::Vertex Vertex;

    BufferResource*     vertexBuffer;
    BufferResource*     indexBuffer;
    PrimitiveTopology   primitiveTopology;
    int                 vertexCount;
    int                 indexCount;
    std::vector<Mesh*>  meshes;
};

std::vector<Model*> gModels;

void loadModel(
    Renderer*               renderer,
    char const*             inputPath,
    ModelLoader::LoadFlags  loadFlags = 0,
    float                   scale = 1.0f)
{
    struct Callbacks : ModelLoader::ICallbacks
    {
        void* createMaterial(MaterialData const& data) override
        {
            SimpleMaterial* material = new SimpleMaterial();
            material->uniforms.diffuseColor = data.diffuseColor;

            material->parameterBlock = material->createParameterBlock();

            return material;
        }

        void* createMesh(MeshData const& data) override
        {
            Mesh* mesh = new Mesh();
            mesh->firstIndex = data.firstIndex;
            mesh->indexCount = data.indexCount;
            mesh->material = (Material*)data.material;
            return mesh;
        }

        void* createModel(ModelData const& data) override
        {
            Model* model = new Model();
            model->vertexBuffer = data.vertexBuffer;
            model->indexBuffer = data.indexBuffer;
            model->primitiveTopology = data.primitiveTopology;
            model->vertexCount = data.vertexCount;
            model->indexCount = data.indexCount;

            int meshCount = data.meshCount;
            for(int ii = 0; ii < meshCount; ++ii)
                model->meshes.push_back((Mesh*)data.meshes[ii]);

            return model;
        }
    };

    Callbacks callbacks;

    ModelLoader loader;
    loader.renderer = renderer;
    loader.loadFlags = loadFlags;
    loader.scale = scale;
    loader.callbacks = &callbacks;

    Model* model = nullptr;
    if(SLANG_FAILED(loader.load(inputPath, (void**)&model)))
    {
        log("failed to load '%s'\n", inputPath);
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

    // Input Assembler (IA)

    // Input Layout

    InputElementDesc inputElements[] = {
        {"POSITION", 0, Format::RGB_Float32, offsetof(Model::Vertex, position) },
        {"NORMAL",   0, Format::RGB_Float32, offsetof(Model::Vertex, normal) },
        {"UV",       0, Format::RG_Float32,  offsetof(Model::Vertex, uv) },
    };
    InputLayout* inputLayout = gRenderer->createInputLayout(
        &inputElements[0],
        3);
    if(!inputLayout) return FAILURE;

    // Depth-Stencil Test (DS)
    {
        TextureResource::Desc depthBufferDesc = gRenderer->getSwapChainTextureDesc();
        depthBufferDesc.format = Format::D_Float32;
        depthBufferDesc.setDefaults(Resource::Usage::DepthWrite);

        TextureResource* depthTexture = gRenderer->createTextureResource(
            Resource::Usage::DepthWrite,
            depthBufferDesc);
        if(!depthTexture) return FAILURE;

        ResourceView::Desc textureViewDesc;
        textureViewDesc.type = ResourceView::Type::DepthStencil;
        ResourceView* depthTarget = gRenderer->createTextureView(depthTexture, textureViewDesc);
        if (!depthTarget) return FAILURE;

        gDepthTarget = depthTarget;
    }

    // Shaders (VS, PS, ...)

    ShaderLibrary* shaderLibrary = loadShaderLibrary(gRenderer, "shaders.slang");
    if(!shaderLibrary) return FAILURE;

    Program* program = loadProgram(shaderLibrary, "vertexMain", "fragmentMain");
    if(!program) return FAILURE;

//    ShaderProgram* shaderProgram = loadShaderProgram(gRenderer);
//    if(!shaderProgram) return FAILURE;

    // We need to load reflection information for the types
    // tha are used inside of our Slang parameter blocks,
    // so lets do that here.

    gPerViewParameterBlockLayout = getParameterBlockLayout(
        shaderLibrary, "PerView");
    SimpleMaterial::gParameterBlockLayout = getParameterBlockLayout(
        shaderLibrary, "SimpleMaterial");
    gPerModelParameterBlockLayout = getParameterBlockLayout(
        shaderLibrary, "PerModel");


    {
        Effect* effect = new Effect();

        auto& desc = effect->desc;
        desc.inputLayout = inputLayout;
        desc.renderTargetCount = 1;

        effect->program = program;

        gEffect = effect;
    }

    // Load model(s)

    loadModel(gRenderer, "CornellBox-Original.obj", ModelLoader::LoadFlag::FlipWinding);
//    loadModel(gRenderer, "cube.obj");
//    loadModel(gRenderer, "teapot.obj");
    loadModel(gRenderer, "bumpy-teapot.obj", ModelLoader::LoadFlag::FlipWinding, 0.01f);

    // Once we've initialized all the graphics API objects,
    // it is time to show our application window and start rendering.

    showWindow(gWindow);

    return OKAY;
}

ParameterBlock* allocateParameterBlockImpl(
    ParameterBlockLayout*   layout)
{
    auto renderer = layout->renderer;

    // TODO: if there is a primary constant buffer, allocate it
    BufferResource* primaryConstantBuffer = nullptr;
    if(auto primaryConstantBufferSize = layout->primaryConstantBufferSize)
    {
        BufferResource::Desc bufferDesc;
        bufferDesc.init(primaryConstantBufferSize);
        bufferDesc.setDefaults(Resource::Usage::ConstantBuffer);
        bufferDesc.cpuAccessFlags = Resource::AccessFlag::Write;
        primaryConstantBuffer = renderer->createBufferResource(
            Resource::Usage::ConstantBuffer,
            bufferDesc);
    }

    // TODO: if there is a descriptor set, allocate it
    auto descriptorSet = renderer->createDescriptorSet(
        layout->descriptorSetLayout);

    if(primaryConstantBuffer)
    {
        descriptorSet->setConstantBuffer(0, 0, primaryConstantBuffer);
    }

    auto parameterBlock = new ParameterBlock();
    parameterBlock->renderer = renderer;
    parameterBlock->layout = layout;
    parameterBlock->primaryConstantBuffer = primaryConstantBuffer;
    parameterBlock->descriptorSet = descriptorSet;

    return parameterBlock;
}

ParameterBlock* allocatePersistentParameterBlock(
    ParameterBlockLayout*   layout)
{
    return allocateParameterBlockImpl(layout);
}

ParameterBlock* allocateTransientParameterBlock(
    ParameterBlockLayout*   layout)
{
    return allocateParameterBlockImpl(layout);
}

void* ParameterBlock::map()
{
    return renderer->map(
        primaryConstantBuffer,
        MapFlavor::WriteDiscard);
}

void ParameterBlock::unmap()
{
    renderer->unmap(primaryConstantBuffer);
}

// Application's rendering state, which
// layers on top of the underlying graphics API
struct RenderContext
{
private:
    enum { kMaxParameterBlocks = 8 };

    Renderer*               renderer = nullptr;
    Effect*                 effect = nullptr;
    ParameterBlock*         parameterBlocks[kMaxParameterBlocks];
    ParameterBlockLayout*   parameterBlockLayouts[kMaxParameterBlocks];

    bool                    pipelineStateDirty = true;
    int                     minDirtyBlockBinding = 0;

    PipelineLayout*         currentPipelineLayout = nullptr;

public:
    RenderContext(Renderer* renderer)
        : renderer(renderer)
    {}

    void setEffect(
        Effect* effect)
    {
        this->effect = effect;
    }

    void setParameterBlock(
        int             index,
        ParameterBlock* parameterBlock)
    {
        if(parameterBlock != parameterBlocks[index])
        {
            parameterBlocks[index] = parameterBlock;
            minDirtyBlockBinding = std::min(index, minDirtyBlockBinding);

            auto layout = parameterBlock->layout;
            if(layout != parameterBlockLayouts[index])
            {
                parameterBlockLayouts[index] = layout;
                pipelineStateDirty = true;
            }
        }
    }

    void flushState()
    {
        auto program = effect->program;
        auto parameterBlockCount = program->parameterBlockCount;

        if(pipelineStateDirty)
        {
            pipelineStateDirty = false;

            // We will compute the desired pipeline layout (aka,
            // D3D12 "root signature") based on the parameter blocks
            // that have been bound.
            //
            std::vector<PipelineLayout::DescriptorSetDesc> descriptorSets;
            for(int pp = 0; pp < parameterBlockCount; ++pp)
            {
                descriptorSets.push_back(
                    parameterBlockLayouts[pp]->descriptorSetLayout);
            }
            PipelineLayout::Desc pipelineLayoutDesc;
            pipelineLayoutDesc.renderTargetCount = 1;
            pipelineLayoutDesc.descriptorSetCount = descriptorSets.size();
            pipelineLayoutDesc.descriptorSets = descriptorSets.data();
            auto pipelineLayout = renderer->createPipelineLayout(pipelineLayoutDesc);

            // The final shader kernels to bind will be computed
            // from the kernels we extracted into an application `EntryPoint`
            // plus the types of the bound paramter blocks, as needed.
            //

            // We will "infer" a type argument for each of the generic
            // parameters of our shader program by looking for a
            // parameter block that is declared using that generic
            // type.
            //
            std::vector<const char*> genericArgs;
            for(auto gp : program->genericParams)
            {
                int parameterBlockIndex = gp->parameterBlockIndex;
                auto typeName = parameterBlockLayouts[parameterBlockIndex]->slangTypeLayout->getName();
                genericArgs.push_back(typeName);
            }

            auto slangSession = getSlangSession();
            SlangCompileRequest* slangRequest = spCreateCompileRequest(slangSession);

            int targetIndex = spAddCodeGenTarget(slangRequest, SLANG_DXBC);
            spSetTargetProfile(slangRequest, targetIndex, spFindProfile(slangSession, "sm_4_0"));

            int translationUnitIndex = spAddTranslationUnit(slangRequest, SLANG_SOURCE_LANGUAGE_SLANG, nullptr);
            spAddTranslationUnitSourceFile(slangRequest, translationUnitIndex, program->shaderLibrary->inputPath.c_str());

            int entryPointCont = program->entryPoints.size();
            for(int ii = 0; ii < entryPointCont; ++ii)
            {
                auto entryPoint = program->entryPoints[ii];
                spAddEntryPointEx(
                    slangRequest,
                    translationUnitIndex,
                    entryPoint->name.c_str(),
                    entryPoint->slangStage,
                    genericArgs.size(),
                    genericArgs.data());
            }

            int compileErr = spCompile(slangRequest);
            if(auto diagnostics = spGetDiagnosticOutput(slangRequest))
            {
                reportError("%s", diagnostics);
            }
            if(compileErr)
            {
                spDestroyCompileRequest(slangRequest);
                throw 99;
            }

            std::vector<ISlangBlob*> kernelBlobs;
            std::vector<ShaderProgram::KernelDesc> kernelDescs;
            for(int ii = 0; ii < entryPointCont; ++ii)
            {
                auto entryPoint = program->entryPoints[ii];

                ISlangBlob* blob = nullptr;
                spGetEntryPointCodeBlob(slangRequest, ii, 0, &blob);

                kernelBlobs.push_back(blob);

                ShaderProgram::KernelDesc kernelDesc;

                char const* codeBegin = (char const*) blob->getBufferPointer();
                char const* codeEnd = codeBegin + blob->getBufferSize();

                kernelDesc.stage = entryPoint->apiStage;
                kernelDesc.codeBegin = codeBegin;
                kernelDesc.codeEnd = codeEnd;

                kernelDescs.push_back(kernelDesc);
            }

            spDestroyCompileRequest(slangRequest);

            ShaderProgram::Desc programDesc;
            programDesc.pipelineType = PipelineType::Graphics;
            programDesc.kernels = kernelDescs.data();
            programDesc.kernelCount = kernelDescs.size();

            ShaderProgram* specializedProgram = renderer->createProgram(programDesc);

            for(auto blob : kernelBlobs)
            {
                blob->release();
            }

            GraphicsPipelineStateDesc pipelineStateDesc = effect->desc;
            pipelineStateDesc.program = specializedProgram;
            pipelineStateDesc.pipelineLayout = pipelineLayout;

            auto pipelineState = renderer->createGraphicsPipelineState(pipelineStateDesc);

            renderer->setPipelineState(PipelineType::Graphics, pipelineState);

            currentPipelineLayout = pipelineLayout;
        }

        for(int ii = minDirtyBlockBinding; ii < parameterBlockCount; ++ii)
        {
            renderer->setDescriptorSet(
                PipelineType::Graphics,
                currentPipelineLayout,
                ii,
                parameterBlocks[ii]->descriptorSet);
        }
        minDirtyBlockBinding = parameterBlockCount;
    }
};

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

    RenderContext context(gRenderer);

    gRenderer->setDepthStencilTarget(gDepthTarget);

    // TODO: set basic state on context...
//    gRenderer->setPipelineState(
//        PipelineType::Graphics,
//        gPipelineState);

    context.setEffect(gEffect);

    gRenderer->setPrimitiveTopology(PrimitiveTopology::TriangleList);

    // We allocate and fill in the parameter block with the
    // per-view parameters once outside of the loop over models.
    //
    {
        auto viewParameterBlock = allocateTransientParameterBlock(
            gPerViewParameterBlockLayout);
        if(auto perView = viewParameterBlock->mapAs<PerView>())
        {
            perView->viewProjection = viewProjection;
            perView->lightDir = lightDir;
            perView->lightColor = lightColor;

            viewParameterBlock->unmap();
        }
        context.setParameterBlock(0, viewParameterBlock);
    }

    for(auto& model : gModels)
    {
        UInt vertexStride = sizeof(Model::Vertex);
        UInt vertexBufferOffset = 0;
        gRenderer->setVertexBuffers(0, 1, &model->vertexBuffer, &vertexStride, &vertexBufferOffset);

        gRenderer->setIndexBuffer(model->indexBuffer, Format::R_UInt32);

        // For each model we provide the parameter
        // block that holds the per-model transformation
        // parameters.
        //
        // Like the view parameter block, it makes sense
        // to allocate this block as a transient allocation,
        // since its contents would be different on the next
        // frame anyway.
        //
        {
            auto modelParameterBlock = allocateTransientParameterBlock(
                gPerModelParameterBlockLayout);
            if(auto perModel = modelParameterBlock->mapAs<PerModel>())
            {
                perModel->modelTransform = modelTransform;
                perModel->inverseTransposeModelTransform = inverseTransposeModelTransform;

                modelParameterBlock->unmap();
            }
            context.setParameterBlock(1, modelParameterBlock);
        }

        //

        for(auto& mesh : model->meshes)
        {
            // Each material will have its own parameter block
            // that we can use to bind both the material parameters
            // and the material *code* to the pipeline.
            //
            context.setParameterBlock(
                2,
                mesh->material->parameterBlock);

            // Once we've set up all the parameter blocks needed
            // for a given drawing operation, we need to flush
            // any pending state changes (e.g., if the type of
            // material changed, a shader switch might be
            // required).
            context.flushState();

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
