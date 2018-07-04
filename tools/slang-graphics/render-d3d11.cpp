// render-d3d11.cpp

#define _CRT_SECURE_NO_WARNINGS

#include "render-d3d11.h"

//WORKING: #include "options.h"
#include "render.h"
#include "d3d-util.h"

#include "surface.h"

// In order to use the Slang API, we need to include its header

//#include <slang.h>

#include "../../slang-com-ptr.h"

// We will be rendering with Direct3D 11, so we need to include
// the Windows and D3D11 headers

#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
#undef WIN32_LEAN_AND_MEAN
#undef NOMINMAX

#include <d3d11_2.h>
#include <d3dcompiler.h>

// We will use the C standard library just for printing error messages.
#include <stdio.h>

#ifdef _MSC_VER
#include <stddef.h>
#if (_MSC_VER < 1900)
#define snprintf sprintf_s
#endif
#endif
//
using namespace Slang;

namespace slang_graphics {

class D3D11Renderer : public Renderer
{
public:
    enum
    {
        kMaxUAVs = 64,
        kMaxRTVs = 8,
    };

    // Renderer    implementation
    virtual SlangResult initialize(const Desc& desc, void* inWindowHandle) override;
    virtual void setClearColor(const float color[4]) override;
    virtual void clearFrame() override;
    virtual void presentFrame() override;
    virtual TextureResource* createTextureResource(Resource::Usage initialUsage, const TextureResource::Desc& desc, const TextureResource::Data* initData) override;
    virtual BufferResource* createBufferResource(Resource::Usage initialUsage, const BufferResource::Desc& bufferDesc, const void* initData) override;
    virtual SlangResult captureScreenSurface(Surface& surfaceOut) override;
    virtual InputLayout* createInputLayout( const InputElementDesc* inputElements, UInt inputElementCount) override;

//    virtual BindingState* createBindingState(const BindingState::Desc& bindingStateDesc) override;
    virtual DescriptorSetLayout* createDescriptorSetLayout(const DescriptorSetLayout::Desc& desc) override;
    virtual PipelineLayout* createPipelineLayout(const PipelineLayout::Desc& desc) override;
    virtual DescriptorSet* createDescriptorSet(DescriptorSetLayout* layout) override;

    virtual ShaderProgram* createProgram(const ShaderProgram::Desc& desc) override;
    virtual PipelineState* createGraphicsPipelineState(const GraphicsPipelineStateDesc& desc) override;
    virtual PipelineState* createComputePipelineState(const ComputePipelineStateDesc& desc) override;
    virtual void* map(BufferResource* buffer, MapFlavor flavor) override;
    virtual void unmap(BufferResource* buffer) override;
    virtual void setPrimitiveTopology(PrimitiveTopology topology) override;

    virtual void setDescriptorSet(PipelineType pipelineType, PipelineLayout* layout, UInt index, DescriptorSet* descriptorSet) override;

    virtual void setVertexBuffers(UInt startSlot, UInt slotCount, BufferResource*const* buffers, const UInt* strides,  const UInt* offsets) override;
    virtual void setIndexBuffer(BufferResource* buffer, Format indexFormat, UInt offset) override;
    virtual void setDepthStencilTarget(TextureView* depthStencilView) override;
    virtual void setPipelineState(PipelineType pipelineType, PipelineState* state) override;
    virtual void draw(UInt vertexCount, UInt startVertex) override;
    virtual void drawIndexed(UInt indexCount, UInt startIndex, UInt baseVertex) override;
    virtual void dispatchCompute(int x, int y, int z) override;
    virtual void submitGpuWork() override {}
    virtual void waitForGpu() override {}
    virtual RendererType getRendererType() const override { return RendererType::DirectX11; }

    protected:

#if 0
    struct BindingDetail
    {
        ComPtr<ID3D11ShaderResourceView>    m_srv;
        ComPtr<ID3D11UnorderedAccessView>   m_uav;
        ComPtr<ID3D11SamplerState>          m_samplerState;
    };

    class BindingStateImpl: public BindingState
    {
		public:
        typedef BindingState Parent;

            /// Ctor
        BindingStateImpl(const Desc& desc):
            Parent(desc)
        {}

        List<BindingDetail> m_bindingDetails;
    };
#endif

    enum class D3D11DescriptorSlotType
    {
        ConstantBuffer,
        ShaderResourceView,
        UnorderedAccessView,
        Sampler,

        CountOf,
    };

    class DescriptorSetLayoutImpl : public DescriptorSetLayout
    {
    public:
        struct RangeInfo
        {
            D3D11DescriptorSlotType type;
            UInt                    arrayIndex;
        };
        List<RangeInfo> m_ranges;

        struct BindingRangeInfo
        {
            UInt count;
            UInt bindingIndex;
            UInt arrayIndex;
        };

        List<BindingRangeInfo>  m_bindingRanges[int(D3D11DescriptorSlotType::CountOf)];
    };

    class PipelineLayoutImpl : public PipelineLayout
    {
    public:
        struct DescriptorSetInfo
        {
            RefPtr<DescriptorSetLayoutImpl> layout;
            UInt                            baseIndices[int(D3D11DescriptorSlotType::CountOf)];
        };

        List<DescriptorSetInfo>     m_descriptorSets;
        UINT                        m_uavCount;
    };

    class DescriptorSetImpl : public DescriptorSet
    {
    public:
        virtual void setTexture(UInt range, UInt index, TextureView* texture) override;
        virtual void setBuffer(UInt range, UInt index, BufferResource* buffer) override;

        RefPtr<DescriptorSetLayoutImpl>         m_layout;

        List<ComPtr<ID3D11Buffer>>              m_cbs;
        List<ComPtr<ID3D11ShaderResourceView>>  m_srvs;
        List<ComPtr<ID3D11UnorderedAccessView>> m_uavs;
        List<ComPtr<ID3D11SamplerState>>        m_samplers;
    };

    class ShaderProgramImpl: public ShaderProgram
    {
    public:
        ComPtr<ID3D11VertexShader> m_vertexShader;
        ComPtr<ID3D11PixelShader> m_pixelShader;
        ComPtr<ID3D11ComputeShader> m_computeShader;
    };

    class BufferResourceImpl: public BufferResource
    {
		public:
        typedef BufferResource Parent;

        BufferResourceImpl(const Desc& desc, Usage initialUsage):
            Parent(desc),
            m_initialUsage(initialUsage)
        {
        }

        MapFlavor m_mapFlavor;
        Usage m_initialUsage;
        ComPtr<ID3D11Buffer> m_buffer;
        ComPtr<ID3D11Buffer> m_staging;
    };
    class TextureResourceImpl : public TextureResource
    {
    public:
        typedef TextureResource Parent;

        TextureResourceImpl(const Desc& desc, Usage initialUsage) :
            Parent(desc),
            m_initialUsage(initialUsage)
        {
        }
        Usage m_initialUsage;
        ComPtr<ID3D11Resource> m_resource;

    };

    class TextureViewImpl : public TextureView
    {
    public:

        // TODO: we should be able to store just one pointer,
        // and cast as appropriate based on the type of view.

        ComPtr<ID3D11ShaderResourceView>    m_srv;
        ComPtr<ID3D11UnorderedAccessView>   m_uav;
        ComPtr<ID3D11DepthStencilView>      m_dsv;
        ComPtr<ID3D11RenderTargetView>      m_rtv;
    };

	class InputLayoutImpl: public InputLayout
	{
		public:
		ComPtr<ID3D11InputLayout> m_layout;
	};

    class PipelineStateImpl : public PipelineState
    {
    public:
        RefPtr<ShaderProgramImpl>   m_program;
        RefPtr<PipelineLayoutImpl>  m_pipelineLayout;
    };


    class GraphicsPipelineStateImpl : public PipelineStateImpl
    {
    public:
        UINT                            m_rtvCount;

        RefPtr<InputLayoutImpl>         m_inputLayout;
        ComPtr<ID3D11DepthStencilState> m_depthStencilState;
        ComPtr<ID3D11RasterizerState>   m_rasterizerState;

        UINT                            m_stencilRef;
    };

    class ComputePipelineStateImpl : public PipelineStateImpl
    {
    public:
    };

        /// Capture a texture to a file
    static HRESULT captureTextureToSurface(ID3D11Device* device, ID3D11DeviceContext* context, ID3D11Texture2D* texture, Surface& surfaceOut);

    void _flushGraphicsState();
    void _flushComputeState();

    ComPtr<IDXGISwapChain> m_swapChain;
    ComPtr<ID3D11Device> m_device;
    ComPtr<ID3D11DeviceContext> m_immediateContext;
    ComPtr<ID3D11Texture2D> m_backBufferTexture;

    List<ComPtr<ID3D11RenderTargetView> > m_renderTargetViews;
    List<ComPtr<ID3D11Texture2D> > m_renderTargetTextures;

    bool m_renderTargetBindingsDirty = false;

    RefPtr<GraphicsPipelineStateImpl>   m_currentGraphicsState;
    RefPtr<ComputePipelineStateImpl>    m_currentComputeState;

    ComPtr<ID3D11RenderTargetView>      m_rtvBindings[kMaxRTVs];
    ComPtr<ID3D11DepthStencilView>      m_dsvBinding;
    ComPtr<ID3D11UnorderedAccessView>   m_uavBindings[int(PipelineType::CountOf)][kMaxUAVs];
    bool m_targetBindingsDirty[int(PipelineType::CountOf)];

    Desc m_desc;

    float m_clearColor[4] = { 0, 0, 0, 0 };
};

Renderer* createD3D11Renderer()
{
    return new D3D11Renderer();
}

/* static */HRESULT D3D11Renderer::captureTextureToSurface(ID3D11Device* device, ID3D11DeviceContext* context, ID3D11Texture2D* texture, Surface& surfaceOut)
{
    if (!context) return E_INVALIDARG;
    if (!texture) return E_INVALIDARG;

    D3D11_TEXTURE2D_DESC textureDesc;
    texture->GetDesc(&textureDesc);

    // Don't bother supporting MSAA for right now
    if (textureDesc.SampleDesc.Count > 1)
    {
        fprintf(stderr, "ERROR: cannot capture multi-sample texture\n");
        return E_INVALIDARG;
    }

    HRESULT hr = S_OK;
    ComPtr<ID3D11Texture2D> stagingTexture;

    if (textureDesc.Usage == D3D11_USAGE_STAGING && (textureDesc.CPUAccessFlags & D3D11_CPU_ACCESS_READ))
    {
        stagingTexture = texture;
    }
    else
    {
        // Modify the descriptor to give us a staging texture
        textureDesc.BindFlags = 0;
        textureDesc.MiscFlags &= ~D3D11_RESOURCE_MISC_TEXTURECUBE;
        textureDesc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
        textureDesc.Usage = D3D11_USAGE_STAGING;

        hr = device->CreateTexture2D(&textureDesc, 0, stagingTexture.writeRef());
        if (FAILED(hr))
        {
            fprintf(stderr, "ERROR: failed to create staging texture\n");
            return hr;
        }

        context->CopyResource(stagingTexture, texture);
    }

    // Now just read back texels from the staging textures
    {
        D3D11_MAPPED_SUBRESOURCE mappedResource;
        SLANG_RETURN_ON_FAIL(context->Map(stagingTexture, 0, D3D11_MAP_READ, 0, &mappedResource));

        Result res = surfaceOut.set(textureDesc.Width, textureDesc.Height, Format::RGBA_Unorm_UInt8, mappedResource.RowPitch, mappedResource.pData, SurfaceAllocator::getMallocAllocator());

        // Make sure to unmap
        context->Unmap(stagingTexture, 0);
        return res;
    }
}

// !!!!!!!!!!!!!!!!!!!!!!!!!!!! Renderer interface !!!!!!!!!!!!!!!!!!!!!!!!!!

SlangResult D3D11Renderer::initialize(const Desc& desc, void* inWindowHandle)
{
    auto windowHandle = (HWND)inWindowHandle;
    m_desc = desc;

    // Rather than statically link against D3D, we load it dynamically.
    HMODULE d3dModule = LoadLibraryA("d3d11.dll");
    if (!d3dModule)
    {
        fprintf(stderr, "error: failed load 'd3d11.dll'\n");
        return SLANG_FAIL;
    }

    PFN_D3D11_CREATE_DEVICE_AND_SWAP_CHAIN D3D11CreateDeviceAndSwapChain_ =
        (PFN_D3D11_CREATE_DEVICE_AND_SWAP_CHAIN)GetProcAddress(d3dModule, "D3D11CreateDeviceAndSwapChain");
    if (!D3D11CreateDeviceAndSwapChain_)
    {
        fprintf(stderr,
            "error: failed load symbol 'D3D11CreateDeviceAndSwapChain'\n");
        return SLANG_FAIL;
    }

    // We create our device in debug mode, just so that we can check that the
    // example doesn't trigger warnings.
    UINT deviceFlags = 0;
    deviceFlags |= D3D11_CREATE_DEVICE_DEBUG;

    // Our swap chain uses RGBA8 with sRGB, with double buffering.
    DXGI_SWAP_CHAIN_DESC swapChainDesc = { 0 };
    swapChainDesc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;

    // Note(tfoley): Disabling sRGB for DX back buffer for now, so that we
    // can get consistent output with OpenGL, where setting up sRGB will
    // probably be more involved.
    // swapChainDesc.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM_SRGB;
    swapChainDesc.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;

    swapChainDesc.SampleDesc.Count = 1;
    swapChainDesc.SampleDesc.Quality = 0;
    swapChainDesc.BufferCount = 2;
    swapChainDesc.OutputWindow = windowHandle;
    swapChainDesc.Windowed = TRUE;
    swapChainDesc.SwapEffect = DXGI_SWAP_EFFECT_DISCARD;
    swapChainDesc.Flags = 0;

    // We will ask for the highest feature level that can be supported.
    const D3D_FEATURE_LEVEL featureLevels[] = {
        D3D_FEATURE_LEVEL_11_1,
        D3D_FEATURE_LEVEL_11_0,
        D3D_FEATURE_LEVEL_10_1,
        D3D_FEATURE_LEVEL_10_0,
        D3D_FEATURE_LEVEL_9_3,
        D3D_FEATURE_LEVEL_9_2,
        D3D_FEATURE_LEVEL_9_1,
    };
    D3D_FEATURE_LEVEL featureLevel = D3D_FEATURE_LEVEL_9_1;
    const int totalNumFeatureLevels = SLANG_COUNT_OF(featureLevels);

    // On a machine that does not have an up-to-date version of D3D installed,
    // the `D3D11CreateDeviceAndSwapChain` call will fail with `E_INVALIDARG`
    // if you ask for featuer level 11_1. The workaround is to call
    // `D3D11CreateDeviceAndSwapChain` up to twice: the first time with 11_1
    // at the start of the list of requested feature levels, and the second
    // time without it.

    for (int ii = 0; ii < 2; ++ii)
    {
        const HRESULT hr = D3D11CreateDeviceAndSwapChain_(
            nullptr,                    // adapter (use default)
//            D3D_DRIVER_TYPE_REFERENCE,
            D3D_DRIVER_TYPE_HARDWARE,
            nullptr,                    // software
            deviceFlags,
            &featureLevels[ii],
            totalNumFeatureLevels - ii,
            D3D11_SDK_VERSION,
            &swapChainDesc,
            m_swapChain.writeRef(),
            m_device.writeRef(),
            &featureLevel,
            m_immediateContext.writeRef());

        // Failures with `E_INVALIDARG` might be due to feature level 11_1
        // not being supported.
        if (hr == E_INVALIDARG)
        {
            continue;
        }

        // Other failures are real, though.
        SLANG_RETURN_ON_FAIL(hr);
        // We must have a swap chain
        break;
    }

    // After we've created the swap chain, we can request a pointer to the
    // back buffer as a D3D11 texture, and create a render-target view from it.

    static const IID kIID_ID3D11Texture2D = {
        0x6f15aaf2, 0xd208, 0x4e89, 0x9a, 0xb4, 0x48,
        0x95, 0x35, 0xd3, 0x4f, 0x9c };

    SLANG_RETURN_ON_FAIL(m_swapChain->GetBuffer(0, kIID_ID3D11Texture2D, (void**)m_backBufferTexture.writeRef()));

    for (int i = 0; i < 8; i++)
    {
        ComPtr<ID3D11Texture2D> texture;
        D3D11_TEXTURE2D_DESC textureDesc;
        m_backBufferTexture->GetDesc(&textureDesc);
        SLANG_RETURN_ON_FAIL(m_device->CreateTexture2D(&textureDesc, nullptr, texture.writeRef()));

        ComPtr<ID3D11RenderTargetView> rtv;
        D3D11_RENDER_TARGET_VIEW_DESC rtvDesc;
        rtvDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
        rtvDesc.Texture2D.MipSlice = 0;
        rtvDesc.ViewDimension = D3D11_RTV_DIMENSION_TEXTURE2D;
        SLANG_RETURN_ON_FAIL(m_device->CreateRenderTargetView(texture, &rtvDesc, rtv.writeRef()));

        m_renderTargetViews.Add(rtv);
        m_renderTargetTextures.Add(texture);
    }

    m_immediateContext->OMSetRenderTargets((UINT)m_renderTargetViews.Count(), m_renderTargetViews.Buffer()->readRef(), nullptr);

    // Similarly, we are going to set up a viewport once, and then never
    // switch, since this is a simple test app.
    D3D11_VIEWPORT viewport;
    viewport.TopLeftX = 0;
    viewport.TopLeftY = 0;
    viewport.Width = (float)desc.width;
    viewport.Height = (float)desc.height;
    viewport.MaxDepth = 1; // TODO(tfoley): use reversed depth
    viewport.MinDepth = 0;
    m_immediateContext->RSSetViewports(1, &viewport);

    return SLANG_OK;
}

void D3D11Renderer::setClearColor(const float color[4])
{
    memcpy(m_clearColor, color, sizeof(m_clearColor));
}

void D3D11Renderer::clearFrame()
{
    for (auto i = 0u; i < m_renderTargetViews.Count(); i++)
	{
        m_immediateContext->ClearRenderTargetView(m_renderTargetViews[i], m_clearColor);
	}
}

void D3D11Renderer::presentFrame()
{
    m_immediateContext->CopyResource(m_backBufferTexture, m_renderTargetTextures[0]);
    m_swapChain->Present(0, 0);
}

SlangResult D3D11Renderer::captureScreenSurface(Surface& surfaceOut)
{
    return captureTextureToSurface(m_device, m_immediateContext, m_renderTargetTextures[0], surfaceOut);
}

static D3D11_BIND_FLAG _calcResourceFlag(Resource::BindFlag::Enum bindFlag)
{
    typedef Resource::BindFlag BindFlag;
    switch (bindFlag)
    {
        case BindFlag::VertexBuffer:            return D3D11_BIND_VERTEX_BUFFER;
        case BindFlag::IndexBuffer:             return D3D11_BIND_INDEX_BUFFER;
        case BindFlag::ConstantBuffer:          return D3D11_BIND_CONSTANT_BUFFER;
        case BindFlag::StreamOutput:            return D3D11_BIND_STREAM_OUTPUT;
        case BindFlag::RenderTarget:            return D3D11_BIND_RENDER_TARGET;
        case BindFlag::DepthStencil:            return D3D11_BIND_DEPTH_STENCIL;
        case BindFlag::UnorderedAccess:         return D3D11_BIND_UNORDERED_ACCESS;
        case BindFlag::PixelShaderResource:     return D3D11_BIND_SHADER_RESOURCE;
        case BindFlag::NonPixelShaderResource:  return D3D11_BIND_SHADER_RESOURCE;
        default:                                return D3D11_BIND_FLAG(0);
    }
}

static int _calcResourceBindFlags(int bindFlags)
{
    int dstFlags = 0;
    while (bindFlags)
    {
        int lsb = bindFlags & -bindFlags;

        dstFlags |= _calcResourceFlag(Resource::BindFlag::Enum(lsb));
        bindFlags &= ~lsb;
    }
    return dstFlags;
}

static int _calcResourceAccessFlags(int accessFlags)
{
    switch (accessFlags)
    {
        case 0:         return 0;
        case Resource::AccessFlag::Read:            return D3D11_CPU_ACCESS_READ;
        case Resource::AccessFlag::Write:           return D3D11_CPU_ACCESS_WRITE;
        case Resource::AccessFlag::Read |
             Resource::AccessFlag::Write:           return D3D11_CPU_ACCESS_READ | D3D11_CPU_ACCESS_WRITE;
        default: assert(!"Invalid flags"); return 0;
    }
}

TextureResource* D3D11Renderer::createTextureResource(Resource::Usage initialUsage, const TextureResource::Desc& descIn, const TextureResource::Data* initData)
{
    TextureResource::Desc srcDesc(descIn);
    srcDesc.setDefaults(initialUsage);

    const int effectiveArraySize = srcDesc.calcEffectiveArraySize();

    if(initData)
    {
        assert(initData->numSubResources == srcDesc.numMipLevels * effectiveArraySize * srcDesc.size.depth);
    }

    const DXGI_FORMAT format = D3DUtil::getMapFormat(srcDesc.format);
    if (format == DXGI_FORMAT_UNKNOWN)
    {
        return nullptr;
    }

    const int bindFlags = _calcResourceBindFlags(srcDesc.bindFlags);

    // Set up the initialize data
    List<D3D11_SUBRESOURCE_DATA> subRes;
    D3D11_SUBRESOURCE_DATA* subResourcesPtr = nullptr;
    if(initData)
    {
        subRes.SetSize(srcDesc.numMipLevels * effectiveArraySize);
        {
            int subResourceIndex = 0;
            for (int i = 0; i < effectiveArraySize; i++)
            {
                for (int j = 0; j < srcDesc.numMipLevels; j++)
                {
                    const int mipHeight = TextureResource::calcMipSize(srcDesc.size.height, j);

                    D3D11_SUBRESOURCE_DATA& data = subRes[subResourceIndex];

                    data.pSysMem = initData->subResources[subResourceIndex];

                    data.SysMemPitch = UINT(initData->mipRowStrides[j]);
                    data.SysMemSlicePitch = UINT(initData->mipRowStrides[j] * mipHeight);

                    subResourceIndex++;
                }
            }
        }
        subResourcesPtr = subRes.Buffer();
    }

    const int accessFlags = _calcResourceAccessFlags(srcDesc.cpuAccessFlags);

    RefPtr<TextureResourceImpl> texture(new TextureResourceImpl(srcDesc, initialUsage));

    switch (srcDesc.type)
    {
        case Resource::Type::Texture1D:
        {
            D3D11_TEXTURE1D_DESC desc = { 0 };
            desc.BindFlags = bindFlags;
            desc.CPUAccessFlags = accessFlags;
            desc.Format = format;
            desc.MiscFlags = 0;
            desc.MipLevels = srcDesc.numMipLevels;
            desc.ArraySize = effectiveArraySize;
            desc.Width = srcDesc.size.width;
            desc.Usage = D3D11_USAGE_DEFAULT;

            ComPtr<ID3D11Texture1D> texture1D;
            SLANG_RETURN_NULL_ON_FAIL(m_device->CreateTexture1D(&desc, subResourcesPtr, texture1D.writeRef()));

            texture->m_resource = texture1D;
            break;
        }
        case Resource::Type::TextureCube:
        case Resource::Type::Texture2D:
        {
            D3D11_TEXTURE2D_DESC desc = { 0 };
            desc.BindFlags = bindFlags;
            desc.CPUAccessFlags = accessFlags;
            desc.Format = format;
            desc.MiscFlags = 0;
            desc.MipLevels = srcDesc.numMipLevels;
            desc.ArraySize = effectiveArraySize;

            desc.Width = srcDesc.size.width;
            desc.Height = srcDesc.size.height;
            desc.Usage = D3D11_USAGE_DEFAULT;
            desc.SampleDesc.Count = srcDesc.sampleDesc.numSamples;
            desc.SampleDesc.Quality = srcDesc.sampleDesc.quality;

            if (srcDesc.type == Resource::Type::TextureCube)
            {
                desc.MiscFlags |= D3D11_RESOURCE_MISC_TEXTURECUBE;
            }

            ComPtr<ID3D11Texture2D> texture2D;
            SLANG_RETURN_NULL_ON_FAIL(m_device->CreateTexture2D(&desc, subResourcesPtr, texture2D.writeRef()));

            texture->m_resource = texture2D;
            break;
        }
        case Resource::Type::Texture3D:
        {
            D3D11_TEXTURE3D_DESC desc = { 0 };
            desc.BindFlags = bindFlags;
            desc.CPUAccessFlags = accessFlags;
            desc.Format = format;
            desc.MiscFlags = 0;
            desc.MipLevels = srcDesc.numMipLevels;
            desc.Width = srcDesc.size.width;
            desc.Height = srcDesc.size.height;
            desc.Depth = srcDesc.size.depth;
            desc.Usage = D3D11_USAGE_DEFAULT;

            ComPtr<ID3D11Texture3D> texture3D;
            SLANG_RETURN_NULL_ON_FAIL(m_device->CreateTexture3D(&desc, subResourcesPtr, texture3D.writeRef()));

            texture->m_resource = texture3D;
            break;
        }
        default: return nullptr;
    }

#if 0
    if(srcDesc.bindFlags & Resource::BindFlag::DepthStencil)
    {
        SLANG_RETURN_NULL_ON_FAIL(m_device->CreateDepthStencilView(
            texture->m_resource,
            nullptr,
            texture->m_dsv.writeRef()));
    }
#endif

    return texture.detach();
}

BufferResource* D3D11Renderer::createBufferResource(Resource::Usage initialUsage, const BufferResource::Desc& descIn, const void* initData)
{
    BufferResource::Desc srcDesc(descIn);
    srcDesc.setDefaults(initialUsage);

    // Make aligned to 256 bytes... not sure why, but if you remove this the tests do fail.
    const size_t alignedSizeInBytes = D3DUtil::calcAligned(srcDesc.sizeInBytes, 256);

    // Hack to make the initialization never read from out of bounds memory, by copying into a buffer
    List<uint8_t> initDataBuffer;
    if (initData && alignedSizeInBytes > srcDesc.sizeInBytes)
    {
        initDataBuffer.SetSize(alignedSizeInBytes);
        ::memcpy(initDataBuffer.Buffer(), initData, srcDesc.sizeInBytes);
        initData = initDataBuffer.Buffer();
    }

    D3D11_BUFFER_DESC bufferDesc = { 0 };
    bufferDesc.ByteWidth = UINT(alignedSizeInBytes);
    bufferDesc.BindFlags = _calcResourceBindFlags(srcDesc.bindFlags);
    // For read we'll need to do some staging
    bufferDesc.CPUAccessFlags = _calcResourceAccessFlags(descIn.cpuAccessFlags & Resource::AccessFlag::Write);
    bufferDesc.Usage = D3D11_USAGE_DEFAULT;

    // If written by CPU, make it dynamic
    if (descIn.cpuAccessFlags & Resource::AccessFlag::Write)
    {
        bufferDesc.Usage = D3D11_USAGE_DYNAMIC;
    }

    switch (initialUsage)
    {
        case Resource::Usage::ConstantBuffer:
        {
            // We'll just assume ConstantBuffers are dynamic for now
            bufferDesc.Usage = D3D11_USAGE_DYNAMIC;
            break;
        }
        default: break;
    }

    if (bufferDesc.BindFlags & (D3D11_BIND_UNORDERED_ACCESS | D3D11_BIND_SHADER_RESOURCE))
    {
        //desc.BindFlags = D3D11_BIND_UNORDERED_ACCESS | D3D11_BIND_SHADER_RESOURCE;
        if (srcDesc.elementSize != 0)
        {
            bufferDesc.StructureByteStride = srcDesc.elementSize;
            bufferDesc.MiscFlags = D3D11_RESOURCE_MISC_BUFFER_STRUCTURED;
        }
        else
        {
            bufferDesc.MiscFlags = D3D11_RESOURCE_MISC_BUFFER_ALLOW_RAW_VIEWS;
        }
    }

    D3D11_SUBRESOURCE_DATA subResourceData = { 0 };
    subResourceData.pSysMem = initData;

    RefPtr<BufferResourceImpl> buffer(new BufferResourceImpl(srcDesc, initialUsage));

	SLANG_RETURN_NULL_ON_FAIL(m_device->CreateBuffer(&bufferDesc, initData ? &subResourceData : nullptr, buffer->m_buffer.writeRef()));

    if (srcDesc.cpuAccessFlags & Resource::AccessFlag::Read)
    {
        D3D11_BUFFER_DESC bufDesc = {};
        bufDesc.BindFlags = 0;
        bufDesc.ByteWidth = (UINT)alignedSizeInBytes;
        bufDesc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
        bufDesc.Usage = D3D11_USAGE_STAGING;

        SLANG_RETURN_NULL_ON_FAIL(m_device->CreateBuffer(&bufDesc, nullptr, buffer->m_staging.writeRef()));
    }

    return buffer.detach();
}

InputLayout* D3D11Renderer::createInputLayout(const InputElementDesc* inputElementsIn, UInt inputElementCount)
{
    D3D11_INPUT_ELEMENT_DESC inputElements[16] = {};

    char hlslBuffer[1024];
    char* hlslCursor = &hlslBuffer[0];

    hlslCursor += sprintf(hlslCursor, "float4 main(\n");

    for (UInt ii = 0; ii < inputElementCount; ++ii)
    {
        inputElements[ii].SemanticName = inputElementsIn[ii].semanticName;
        inputElements[ii].SemanticIndex = (UINT)inputElementsIn[ii].semanticIndex;
        inputElements[ii].Format = D3DUtil::getMapFormat(inputElementsIn[ii].format);
        inputElements[ii].InputSlot = 0;
        inputElements[ii].AlignedByteOffset = (UINT)inputElementsIn[ii].offset;
        inputElements[ii].InputSlotClass = D3D11_INPUT_PER_VERTEX_DATA;
        inputElements[ii].InstanceDataStepRate = 0;

        if (ii != 0)
        {
            hlslCursor += sprintf(hlslCursor, ",\n");
        }

        char const* typeName = "Unknown";
        switch (inputElementsIn[ii].format)
        {
            case Format::RGBA_Float32:
                typeName = "float4";
                break;
            case Format::RGB_Float32:
                typeName = "float3";
                break;
            case Format::RG_Float32:
                typeName = "float2";
                break;
            case Format::R_Float32:
                typeName = "float";
                break;
            default:
                return nullptr;
        }

        hlslCursor += sprintf(hlslCursor, "%s a%d : %s%d",
            typeName,
            (int)ii,
            inputElementsIn[ii].semanticName,
            (int)inputElementsIn[ii].semanticIndex);
    }

    hlslCursor += sprintf(hlslCursor, "\n) : SV_Position { return 0; }");

	ComPtr<ID3DBlob> vertexShaderBlob;
	SLANG_RETURN_NULL_ON_FAIL(D3DUtil::compileHLSLShader("inputLayout", hlslBuffer, "main", "vs_5_0", vertexShaderBlob));

    ComPtr<ID3D11InputLayout> inputLayout;
	SLANG_RETURN_NULL_ON_FAIL(m_device->CreateInputLayout(&inputElements[0], (UINT)inputElementCount, vertexShaderBlob->GetBufferPointer(), vertexShaderBlob->GetBufferSize(),
        inputLayout.writeRef()));

	InputLayoutImpl* impl = new InputLayoutImpl;
	impl->m_layout.swap(inputLayout);

	return impl;
}

void* D3D11Renderer::map(BufferResource* bufferIn, MapFlavor flavor)
{
    BufferResourceImpl* bufferResource = static_cast<BufferResourceImpl*>(bufferIn);

    D3D11_MAP mapType;
    ID3D11Buffer* buffer = bufferResource->m_buffer;

    switch (flavor)
    {
        case MapFlavor::WriteDiscard:
            mapType = D3D11_MAP_WRITE_DISCARD;
            break;
        case MapFlavor::HostWrite:
            mapType = D3D11_MAP_WRITE;
            break;
        case MapFlavor::HostRead:
            mapType = D3D11_MAP_READ;

            buffer = bufferResource->m_staging;
            if (!buffer)
            {
                return nullptr;
            }

            // Okay copy the data over
            m_immediateContext->CopyResource(buffer, bufferResource->m_buffer);

            break;
        default:
            return nullptr;
    }

    // We update our constant buffer per-frame, just for the purposes
    // of the example, but we don't actually load different data
    // per-frame (we always use an identity projection).
    D3D11_MAPPED_SUBRESOURCE mappedSub;
    SLANG_RETURN_NULL_ON_FAIL(m_immediateContext->Map(buffer, 0, mapType, 0, &mappedSub));

    bufferResource->m_mapFlavor = flavor;

    return mappedSub.pData;
}

void D3D11Renderer::unmap(BufferResource* bufferIn)
{
    BufferResourceImpl* bufferResource = static_cast<BufferResourceImpl*>(bufferIn);
    ID3D11Buffer* buffer = (bufferResource->m_mapFlavor == MapFlavor::HostRead) ? bufferResource->m_staging : bufferResource->m_buffer;
    m_immediateContext->Unmap(buffer, 0);
}

#if 0
void D3D11Renderer::setInputLayout(InputLayout* inputLayoutIn)
{
    auto inputLayout = static_cast<InputLayoutImpl*>(inputLayoutIn);
    m_immediateContext->IASetInputLayout(inputLayout->m_layout);
}
#endif

void D3D11Renderer::setPrimitiveTopology(PrimitiveTopology topology)
{
    m_immediateContext->IASetPrimitiveTopology(D3DUtil::getPrimitiveTopology(topology));
}

void D3D11Renderer::setVertexBuffers(UInt startSlot, UInt slotCount, BufferResource*const* buffersIn, const UInt* stridesIn, const UInt* offsetsIn)
{
    static const int kMaxVertexBuffers = 16;
	assert(slotCount <= kMaxVertexBuffers);

    UINT vertexStrides[kMaxVertexBuffers];
    UINT vertexOffsets[kMaxVertexBuffers];
	ID3D11Buffer* dxBuffers[kMaxVertexBuffers];

	auto buffers = (BufferResourceImpl*const*)buffersIn;

    for (UInt ii = 0; ii < slotCount; ++ii)
    {
        vertexStrides[ii] = (UINT)stridesIn[ii];
        vertexOffsets[ii] = (UINT)offsetsIn[ii];
		dxBuffers[ii] = buffers[ii]->m_buffer;
	}

    m_immediateContext->IASetVertexBuffers((UINT)startSlot, (UINT)slotCount, dxBuffers, &vertexStrides[0], &vertexOffsets[0]);
}

void D3D11Renderer::setIndexBuffer(BufferResource* buffer, Format indexFormat, UInt offset)
{
    DXGI_FORMAT dxFormat = D3DUtil::getMapFormat(indexFormat);
    m_immediateContext->IASetIndexBuffer(((BufferResourceImpl*)buffer)->m_buffer, dxFormat, offset);
}

void D3D11Renderer::setDepthStencilTarget(TextureView* depthStencilView)
{
    m_dsvBinding = ((TextureViewImpl*) depthStencilView)->m_dsv;
    m_targetBindingsDirty[int(PipelineType::Graphics)] = true;
}

void D3D11Renderer::setPipelineState(PipelineType pipelineType, PipelineState* state)
{
    switch(pipelineType)
    {
    default:
        break;

    case PipelineType::Graphics:
        {
            auto stateImpl = (GraphicsPipelineStateImpl*) state;
            auto programImpl = stateImpl->m_program;

            // TODO: We could conceivably do some lightweight state
            // differencing here (e.g., check if `programImpl` is the
            // same as the program that is currently bound).
            //
            // It isn't clear how much that would pay off given that
            // the D3D11 runtime seems to do its own state diffing.

            // IA

            m_immediateContext->IASetInputLayout(stateImpl->m_inputLayout->m_layout);

            // VS

            m_immediateContext->VSSetShader(programImpl->m_vertexShader, nullptr, 0);

            // HS

            // DS

            // GS

            // RS

            m_immediateContext->RSSetState(stateImpl->m_rasterizerState);

            // PS

            m_immediateContext->PSSetShader(programImpl->m_pixelShader, nullptr, 0);

            // OM

            m_immediateContext->OMSetDepthStencilState(stateImpl->m_depthStencilState, stateImpl->m_stencilRef);
        }
        break;

    case PipelineType::Compute:
        {
            auto stateImpl = (ComputePipelineStateImpl*) state;
            auto programImpl = stateImpl->m_program;

            // CS

            m_immediateContext->CSSetShader(programImpl->m_computeShader, nullptr, 0);
        }
        break;
    }

    /// ...
}

void D3D11Renderer::draw(UInt vertexCount, UInt startVertex)
{
    _flushGraphicsState();
    m_immediateContext->Draw((UINT)vertexCount, (UINT)startVertex);
}

void D3D11Renderer::drawIndexed(UInt indexCount, UInt startIndex, UInt baseVertex)
{
    _flushGraphicsState();
    m_immediateContext->DrawIndexed((UINT)indexCount, (UINT)startIndex, (UInt)baseVertex);
}

ShaderProgram* D3D11Renderer::createProgram(const ShaderProgram::Desc& desc)
{
    if (desc.pipelineType == PipelineType::Compute)
    {
        auto computeKernel = desc.findKernel(StageType::Compute);

        ComPtr<ID3D11ComputeShader> computeShader;
        SLANG_RETURN_NULL_ON_FAIL(m_device->CreateComputeShader(computeKernel->codeBegin, computeKernel->getCodeSize(), nullptr, computeShader.writeRef()));

        ShaderProgramImpl* shaderProgram = new ShaderProgramImpl();
        shaderProgram->m_computeShader.swap(computeShader);
        return shaderProgram;
    }
    else
    {
        auto vertexKernel = desc.findKernel(StageType::Vertex);
        auto fragmentKernel = desc.findKernel(StageType::Fragment);

        ComPtr<ID3D11VertexShader> vertexShader;
        ComPtr<ID3D11PixelShader> pixelShader;

        SLANG_RETURN_NULL_ON_FAIL(m_device->CreateVertexShader(vertexKernel->codeBegin, vertexKernel->getCodeSize(), nullptr, vertexShader.writeRef()));
        SLANG_RETURN_NULL_ON_FAIL(m_device->CreatePixelShader(fragmentKernel->codeBegin, fragmentKernel->getCodeSize(), nullptr, pixelShader.writeRef()));

        ShaderProgramImpl* shaderProgram = new ShaderProgramImpl();
        shaderProgram->m_vertexShader.swap(vertexShader);
        shaderProgram->m_pixelShader.swap(pixelShader);
        return shaderProgram;
    }
}

static D3D11_COMPARISON_FUNC translateComparisonFunc(ComparisonFunc func)
{
    switch(func)
    {
    default:
        // TODO: need to report failures
        return D3D11_COMPARISON_ALWAYS;

#define CASE(FROM, TO) \
    case ComparisonFunc::FROM: return D3D11_COMPARISON_##TO

    CASE(Never,         NEVER);
    CASE(Less,          LESS);
    CASE(Equal,         EQUAL);
    CASE(LessEqual,     LESS_EQUAL);
    CASE(Greater,       GREATER);
    CASE(NotEqual,      NOT_EQUAL);
    CASE(GreaterEqual,  GREATER_EQUAL);
    CASE(Always,        ALWAYS);
#undef CASE
    }
}

static D3D11_STENCIL_OP translateStencilOp(StencilOp op)
{
    switch(op)
    {
    default:
        // TODO: need to report failures
        return D3D11_STENCIL_OP_KEEP;

#define CASE(FROM, TO) \
    case StencilOp::FROM: return D3D11_STENCIL_OP_##TO

    CASE(Keep,              KEEP);
    CASE(Zero,              ZERO);
    CASE(Replace,           REPLACE);
    CASE(IncrementSaturate, INCR_SAT);
    CASE(DecrementSaturate, DECR_SAT);
    CASE(Invert,            INVERT);
    CASE(IncrementWrap,     INCR);
    CASE(DecrementWrap,     DECR);
#undef CASE

    }
}

static D3D11_FILL_MODE translateFillMode(FillMode mode)
{
    switch(mode)
    {
    default:
        // TODO: need to report failures
        return D3D11_FILL_SOLID;

    case FillMode::Solid:       return D3D11_FILL_SOLID;
    case FillMode::Wireframe:   return D3D11_FILL_WIREFRAME;
    }
}

static D3D11_CULL_MODE translateCullMode(CullMode mode)
{
    switch(mode)
    {
    default:
        // TODO: need to report failures
        return D3D11_CULL_NONE;

    case CullMode::None:    return D3D11_CULL_NONE;
    case CullMode::Back:    return D3D11_CULL_BACK;
    case CullMode::Front:   return D3D11_CULL_FRONT;
    }
}

PipelineState* D3D11Renderer::createGraphicsPipelineState(const GraphicsPipelineStateDesc& desc)
{
    auto programImpl = (ShaderProgramImpl*) desc.program;

    ComPtr<ID3D11DepthStencilState> depthStencilState;
    {
        D3D11_DEPTH_STENCIL_DESC dsDesc;
        dsDesc.DepthEnable      = desc.depthStencil.depthTestEnable;
        dsDesc.DepthWriteMask   = desc.depthStencil.depthWriteEnable ? D3D11_DEPTH_WRITE_MASK_ALL : D3D11_DEPTH_WRITE_MASK_ZERO;
        dsDesc.DepthFunc        = translateComparisonFunc(desc.depthStencil.depthFunc);
        dsDesc.StencilEnable    = desc.depthStencil.stencilEnable;
        dsDesc.StencilReadMask  = desc.depthStencil.stencilReadMask;
        dsDesc.StencilWriteMask = desc.depthStencil.stencilWriteMask;

    #define FACE(DST, SRC) \
        dsDesc.DST.StencilFailOp      = translateStencilOp(     desc.depthStencil.SRC.stencilFailOp);       \
        dsDesc.DST.StencilDepthFailOp = translateStencilOp(     desc.depthStencil.SRC.stencilDepthFailOp);  \
        dsDesc.DST.StencilPassOp      = translateStencilOp(     desc.depthStencil.SRC.stencilPassOp);       \
        dsDesc.DST.StencilFunc        = translateComparisonFunc(desc.depthStencil.SRC.stencilFunc);         \
        /* end */

        FACE(FrontFace, frontFace);
        FACE(BackFace,  backFace);

        SLANG_RETURN_NULL_ON_FAIL(m_device->CreateDepthStencilState(
            &dsDesc,
            depthStencilState.writeRef()));
    }

    ComPtr<ID3D11RasterizerState> rasterizerState;
    {
        D3D11_RASTERIZER_DESC rsDesc;
        rsDesc.FillMode                 = translateFillMode(desc.rasterizer.fillMode);
        rsDesc.CullMode                 = translateCullMode(desc.rasterizer.cullMode);
        rsDesc.FrontCounterClockwise    = desc.rasterizer.frontFace == FrontFaceMode::Clockwise;
        rsDesc.DepthBias                = desc.rasterizer.depthBias;
        rsDesc.DepthBiasClamp           = desc.rasterizer.depthBiasClamp;
        rsDesc.SlopeScaledDepthBias     = desc.rasterizer.slopeScaledDepthBias;
        rsDesc.DepthClipEnable          = desc.rasterizer.depthClipEnable;
        rsDesc.ScissorEnable            = desc.rasterizer.scissorEnable;
        rsDesc.MultisampleEnable        = desc.rasterizer.multisampleEnable;
        rsDesc.AntialiasedLineEnable    = desc.rasterizer.antialiasedLineEnable;

        SLANG_RETURN_NULL_ON_FAIL(m_device->CreateRasterizerState(
            &rsDesc,
            rasterizerState.writeRef()));

    }

    GraphicsPipelineStateImpl* state = new GraphicsPipelineStateImpl();
    state->m_program = programImpl;
    state->m_stencilRef = desc.depthStencil.stencilRef;
    state->m_depthStencilState = depthStencilState;
    state->m_rasterizerState = rasterizerState;
    return state;
}

PipelineState* D3D11Renderer::createComputePipelineState(const ComputePipelineStateDesc& desc)
{
    auto programImpl = (ShaderProgramImpl*) desc.program;

    ComputePipelineStateImpl* state = new ComputePipelineStateImpl();
    state->m_program = programImpl;
    return state;
}

void D3D11Renderer::dispatchCompute(int x, int y, int z)
{
    _flushComputeState();
    m_immediateContext->Dispatch(x, y, z);
}

DescriptorSetLayout* D3D11Renderer::createDescriptorSetLayout(const DescriptorSetLayout::Desc& desc)
{
    RefPtr<DescriptorSetLayoutImpl> descriptorSetLayoutImpl = new DescriptorSetLayoutImpl();
    return descriptorSetLayoutImpl.detach();
}

PipelineLayout* D3D11Renderer::createPipelineLayout(const PipelineLayout::Desc& desc)
{
    RefPtr<PipelineLayoutImpl> pipelineLayoutImpl = new PipelineLayoutImpl();
    return pipelineLayoutImpl.detach();
}

DescriptorSet* D3D11Renderer::createDescriptorSet(DescriptorSetLayout* layout)
{
    RefPtr<DescriptorSetImpl> descriptorSetImpl = new DescriptorSetImpl();
    return descriptorSetImpl.detach();
}


#if 0
BindingState* D3D11Renderer::createBindingState(const BindingState::Desc& bindingStateDesc)
{
    RefPtr<BindingStateImpl> bindingState(new BindingStateImpl(bindingStateDesc));

    const auto& srcBindings = bindingStateDesc.m_bindings;
    const int numBindings = int(srcBindings.Count());

    auto& dstDetails = bindingState->m_bindingDetails;
    dstDetails.SetSize(numBindings);

    for (int i = 0; i < numBindings; ++i)
    {
        auto& dstDetail = dstDetails[i];
        const auto& srcBinding = srcBindings[i];

        assert(srcBinding.registerRange.isSingle());

        switch (srcBinding.bindingType)
        {
            case BindingType::Buffer:
            {
                assert(srcBinding.resource && srcBinding.resource->isBuffer());

                BufferResourceImpl* buffer = static_cast<BufferResourceImpl*>(srcBinding.resource.Ptr());
                const BufferResource::Desc& bufferDesc = buffer->getDesc();

                const int elemSize = bufferDesc.elementSize <= 0 ? 1 : bufferDesc.elementSize;

                if (bufferDesc.bindFlags & Resource::BindFlag::UnorderedAccess)
                {
                    D3D11_UNORDERED_ACCESS_VIEW_DESC viewDesc;
                    memset(&viewDesc, 0, sizeof(viewDesc));
                    viewDesc.Buffer.FirstElement = 0;
                    viewDesc.Buffer.NumElements = (UINT)(bufferDesc.sizeInBytes / elemSize);
                    viewDesc.Buffer.Flags = 0;
                    viewDesc.ViewDimension = D3D11_UAV_DIMENSION_BUFFER;
                    viewDesc.Format = D3DUtil::getMapFormat(bufferDesc.format);

                    if (bufferDesc.elementSize == 0 && bufferDesc.format == Format::Unknown)
                    {
                        viewDesc.Buffer.Flags |= D3D11_BUFFER_UAV_FLAG_RAW;
                        viewDesc.Format = DXGI_FORMAT_R32_TYPELESS;
                    }

                    SLANG_RETURN_NULL_ON_FAIL(m_device->CreateUnorderedAccessView(buffer->m_buffer, &viewDesc, dstDetail.m_uav.writeRef()));
                }
                if (bufferDesc.bindFlags & (Resource::BindFlag::NonPixelShaderResource | Resource::BindFlag::PixelShaderResource))
                {
                    D3D11_SHADER_RESOURCE_VIEW_DESC viewDesc;
                    memset(&viewDesc, 0, sizeof(viewDesc));
                    viewDesc.Buffer.FirstElement = 0;
                    viewDesc.Buffer.ElementWidth = elemSize;
                    viewDesc.Buffer.NumElements = (UINT)(bufferDesc.sizeInBytes / elemSize);
                    viewDesc.Buffer.ElementOffset = 0;
                    viewDesc.ViewDimension = D3D11_SRV_DIMENSION_BUFFER;
                    viewDesc.Format = DXGI_FORMAT_UNKNOWN;

                    if (bufferDesc.elementSize == 0)
                    {
                        viewDesc.Format = DXGI_FORMAT_R32_FLOAT;
                    }

                    SLANG_RETURN_NULL_ON_FAIL(m_device->CreateShaderResourceView(buffer->m_buffer, &viewDesc, dstDetail.m_srv.writeRef()));
                }
                break;
            }
            case BindingType::Texture:
            case BindingType::CombinedTextureSampler:
            {
                assert(srcBinding.resource && srcBinding.resource->isTexture());

                TextureResourceImpl* texture = static_cast<TextureResourceImpl*>(srcBinding.resource.Ptr());

                const TextureResource::Desc& textureDesc = texture->getDesc();

                D3D11_SHADER_RESOURCE_VIEW_DESC viewDesc;
                viewDesc.Format = D3DUtil::getMapFormat(textureDesc.format);

                switch (texture->getType())
                {
                    case Resource::Type::Texture1D:
                    {
                        if (textureDesc.arraySize <= 0)
                        {
                            viewDesc.ViewDimension =  D3D11_SRV_DIMENSION_TEXTURE1D;
                            viewDesc.Texture1D.MipLevels = textureDesc.numMipLevels;
                            viewDesc.Texture1D.MostDetailedMip = 0;
                        }
                        else
                        {
                            viewDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE1DARRAY;
                            viewDesc.Texture1DArray.ArraySize = textureDesc.arraySize;
                            viewDesc.Texture1DArray.FirstArraySlice = 0;
                            viewDesc.Texture1DArray.MipLevels = textureDesc.numMipLevels;
                            viewDesc.Texture1DArray.MostDetailedMip = 0;
                        }
                        break;
                    }
                    case Resource::Type::Texture2D:
                    {
                        if (textureDesc.arraySize <= 0)
                        {
                            viewDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2D;
                            viewDesc.Texture2D.MipLevels = textureDesc.numMipLevels;
                            viewDesc.Texture2D.MostDetailedMip = 0;
                        }
                        else
                        {
                            viewDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2DARRAY;
                            viewDesc.Texture2DArray.ArraySize = textureDesc.arraySize;
                            viewDesc.Texture2DArray.FirstArraySlice = 0;
                            viewDesc.Texture2DArray.MipLevels = textureDesc.numMipLevels;
                            viewDesc.Texture2DArray.MostDetailedMip = 0;
                        }
                        break;
                    }
                    case Resource::Type::TextureCube:
                    {
                        if (textureDesc.arraySize <= 0)
                        {
                            viewDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURECUBE;
                            viewDesc.TextureCube.MipLevels = textureDesc.numMipLevels;
                            viewDesc.TextureCube.MostDetailedMip = 0;
                        }
                        else
                        {
                            viewDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURECUBEARRAY;
                            viewDesc.TextureCubeArray.MipLevels = textureDesc.numMipLevels;
                            viewDesc.TextureCubeArray.MostDetailedMip = 0;
                            viewDesc.TextureCubeArray.First2DArrayFace = 0;
                            viewDesc.TextureCubeArray.NumCubes = textureDesc.arraySize;
                        }
                        break;
                    }
                    case Resource::Type::Texture3D:
                    {
                        viewDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE3D;
                        viewDesc.Texture3D.MipLevels = textureDesc.numMipLevels;            // Old code fixed as one
                        viewDesc.Texture3D.MostDetailedMip = 0;
                        break;
                    }
                    default:
                    {
                        assert(!"Unhandled type");
                        return nullptr;
                    }
                }

                SLANG_RETURN_NULL_ON_FAIL(m_device->CreateShaderResourceView(texture->m_resource, &viewDesc, dstDetail.m_srv.writeRef()));
                break;
            }
            case BindingType::Sampler:
            {
                const BindingState::SamplerDesc& samplerDesc = bindingStateDesc.m_samplerDescs[srcBinding.descIndex];

                D3D11_SAMPLER_DESC desc = {};
                desc.AddressU = desc.AddressV = desc.AddressW = D3D11_TEXTURE_ADDRESS_WRAP;

                if (samplerDesc.isCompareSampler)
                {
                    desc.ComparisonFunc = D3D11_COMPARISON_LESS_EQUAL;
                    desc.Filter = D3D11_FILTER_MIN_LINEAR_MAG_MIP_POINT;
                    desc.MinLOD = desc.MaxLOD = 0.0f;
                }
                else
                {
                    desc.Filter = D3D11_FILTER_ANISOTROPIC;
                    desc.MaxAnisotropy = 8;
                    desc.MinLOD = 0.0f;
                    desc.MaxLOD = 100.0f;
                }
                SLANG_RETURN_NULL_ON_FAIL(m_device->CreateSamplerState(&desc, dstDetail.m_samplerState.writeRef()));
                break;
            }
            default:
            {
                assert(!"Unhandled type");
                return nullptr;
            }
        }
    }

    // Done
    return bindingState.detach();
}

void D3D11Renderer::_applyBindingState(bool isCompute)
{
    auto context = m_immediateContext.get();

    const auto& details = m_currentBindings->m_bindingDetails;
    const auto& bindings = m_currentBindings->getDesc().m_bindings;

    const int numBindings = int(bindings.Count());

    for (int i = 0; i < numBindings; ++i)
    {
        const auto& binding = bindings[i];
        const auto& detail = details[i];

        const int bindingIndex = binding.registerRange.getSingleIndex();

        switch (binding.bindingType)
        {
            case BindingType::Buffer:
            {
                assert(binding.resource && binding.resource->isBuffer());
                if (binding.resource->canBind(Resource::BindFlag::ConstantBuffer))
                {
                    ID3D11Buffer* buffer = static_cast<BufferResourceImpl*>(binding.resource.Ptr())->m_buffer;
                    if (isCompute)
                        context->CSSetConstantBuffers(bindingIndex, 1, &buffer);
                    else
                    {
                        context->VSSetConstantBuffers(bindingIndex, 1, &buffer);
                        context->PSSetConstantBuffers(bindingIndex, 1, &buffer);
                    }
                }
                else if (detail.m_uav)
                {
                    if (isCompute)
                        context->CSSetUnorderedAccessViews(bindingIndex, 1, detail.m_uav.readRef(), nullptr);
                    else
                        context->OMSetRenderTargetsAndUnorderedAccessViews(
                            m_currentBindings->getDesc().m_numRenderTargets,
                            m_renderTargetViews.Buffer()->readRef(),
                            m_depthStencilView,
                            bindingIndex,
                            1,
                            detail.m_uav.readRef(),
                            nullptr);
                }
                else
                {
                    if (isCompute)
                        context->CSSetShaderResources(bindingIndex, 1, detail.m_srv.readRef());
                    else
                    {
                        context->PSSetShaderResources(bindingIndex, 1, detail.m_srv.readRef());
                        context->VSSetShaderResources(bindingIndex, 1, detail.m_srv.readRef());
                    }
                }
                break;
            }
            case BindingType::Texture:
            {
                if (detail.m_uav)
                {
                    if (isCompute)
                        context->CSSetUnorderedAccessViews(bindingIndex, 1, detail.m_uav.readRef(), nullptr);
                    else
                        context->OMSetRenderTargetsAndUnorderedAccessViews(D3D11_KEEP_RENDER_TARGETS_AND_DEPTH_STENCIL,
                            nullptr, nullptr, bindingIndex, 1, detail.m_uav.readRef(), nullptr);
                }
                else
                {
                    if (isCompute)
                        context->CSSetShaderResources(bindingIndex, 1, detail.m_srv.readRef());
                    else
                    {
                        context->PSSetShaderResources(bindingIndex, 1, detail.m_srv.readRef());
                        context->VSSetShaderResources(bindingIndex, 1, detail.m_srv.readRef());
                    }
                }
                break;
            }
            case BindingType::Sampler:
            {
                if (isCompute)
                    context->CSSetSamplers(bindingIndex, 1, detail.m_samplerState.readRef());
                else
                {
                    context->PSSetSamplers(bindingIndex, 1, detail.m_samplerState.readRef());
                    context->VSSetSamplers(bindingIndex, 1, detail.m_samplerState.readRef());
                }
                break;
            }
            default:
            {
                assert(!"Not implemented");
                return;
            }
        }
    }
}

void D3D11Renderer::setBindingState(BindingState* state)
{
    m_currentBindings = static_cast<BindingStateImpl*>(state);
}
#endif

void D3D11Renderer::_flushGraphicsState()
{
    auto pipelineType = int(PipelineType::Graphics);
    if(m_targetBindingsDirty[pipelineType])
    {
        m_targetBindingsDirty[pipelineType] = false;

        auto pipelineState = m_currentGraphicsState.Ptr();

        auto rtvCount = pipelineState->m_rtvCount;
        auto uavCount = pipelineState->m_pipelineLayout->m_uavCount;

        m_immediateContext->OMSetRenderTargetsAndUnorderedAccessViews(
            rtvCount,
            m_rtvBindings[0].readRef(),
            m_dsvBinding,
            rtvCount,
            uavCount,
            m_uavBindings[pipelineType][rtvCount].readRef(),
            nullptr);
    }
}

void D3D11Renderer::_flushComputeState()
{
    auto pipelineType = int(PipelineType::Graphics);
    if(m_targetBindingsDirty[pipelineType])
    {
        m_targetBindingsDirty[pipelineType] = false;

        auto pipelineState = m_currentComputeState.Ptr();

        auto uavCount = pipelineState->m_pipelineLayout->m_uavCount;

        m_immediateContext->CSSetUnorderedAccessViews(
            0,
            uavCount,
            m_uavBindings[pipelineType][0].readRef(),
            nullptr);
    }}

void D3D11Renderer::DescriptorSetImpl::setTexture(UInt range, UInt index, TextureView* view)
{
    auto viewImpl = (TextureViewImpl*) view;
}

void D3D11Renderer::DescriptorSetImpl::setBuffer(UInt range, UInt index, BufferResource* buffer)
{
    auto bufferImpl = (BufferResourceImpl*) buffer;
    auto& rangeInfo = m_layout->m_ranges[range];

    switch(rangeInfo.type)
    {
    default:
        break;

    case D3D11DescriptorSlotType::ConstantBuffer:
        {
            m_cbs[rangeInfo.arrayIndex + index] = bufferImpl->m_buffer;
        }
        break;
    }
}


void D3D11Renderer::setDescriptorSet(PipelineType pipelineType, PipelineLayout* layout, UInt index, DescriptorSet* descriptorSet)
{
    auto pipelineLayoutImpl = (PipelineLayoutImpl*)layout;
    auto descriptorSetImpl = (DescriptorSetImpl*) descriptorSet;

    auto descriptorSetLayoutImpl = descriptorSetImpl->m_layout;
    auto& setInfo = pipelineLayoutImpl->m_descriptorSets[index];

    // Note: `setInfo->layout` and `descriptorSetLayoutImpl` need to be compatible

    // TODO: If/when we add per-stage visibility masks, it would be best to organize
    // this as a loop over stages, so that we only do the binding that is required
    // for each stage.

    {
        int slotType = int(D3D11DescriptorSlotType::ConstantBuffer);
        UInt baseIndex = setInfo.baseIndices[slotType];

        for(auto range : descriptorSetLayoutImpl->m_bindingRanges[slotType])
        {
            UINT startSlot = baseIndex + range.bindingIndex;
            UINT slotCount = range.count;
            auto cbs = descriptorSetImpl->m_cbs[range.arrayIndex].readRef();

            m_immediateContext->VSSetConstantBuffers(startSlot, slotCount, cbs);
            // ...
            m_immediateContext->PSSetConstantBuffers(startSlot, slotCount, cbs);

            m_immediateContext->CSSetConstantBuffers(startSlot, slotCount, cbs);
        }
    }

    {
        int slotType = int(D3D11DescriptorSlotType::ShaderResourceView);
        UInt baseIndex = setInfo.baseIndices[slotType];

        for(auto range : descriptorSetLayoutImpl->m_bindingRanges[slotType])
        {
            UINT startSlot = baseIndex + range.bindingIndex;
            UINT slotCount = range.count;
            auto srvs = descriptorSetImpl->m_srvs[range.arrayIndex].readRef();

            m_immediateContext->VSSetShaderResources(startSlot, slotCount, srvs);
            // ...
            m_immediateContext->PSSetShaderResources(startSlot, slotCount, srvs);

            m_immediateContext->CSSetShaderResources(startSlot, slotCount, srvs);
        }
    }

    {
        int slotType = int(D3D11DescriptorSlotType::Sampler);
        UInt baseIndex = setInfo.baseIndices[slotType];

        for(auto range : descriptorSetLayoutImpl->m_bindingRanges[slotType])
        {
            UINT startSlot = baseIndex + range.bindingIndex;
            UINT slotCount = range.count;
            auto samplers = descriptorSetImpl->m_samplers[range.arrayIndex].readRef();

            m_immediateContext->VSSetSamplers(startSlot, slotCount, samplers);
            // ...
            m_immediateContext->PSSetSamplers(startSlot, slotCount, samplers);

            m_immediateContext->CSSetSamplers(startSlot, slotCount, samplers);
        }
    }

    {
        int slotType = int(D3D11DescriptorSlotType::UnorderedAccessView);
        UInt baseIndex = setInfo.baseIndices[slotType];

        for(auto range : descriptorSetLayoutImpl->m_bindingRanges[slotType])
        {
            UINT startSlot = baseIndex + range.bindingIndex;
            UINT slotCount = range.count;
            auto uavs = descriptorSetImpl->m_uavs[range.arrayIndex].readRef();

            for(UINT ii = 0; ii < slotCount; ++ii)
            {
                m_uavBindings[int(pipelineType)][startSlot + ii] = uavs[ii];
            }
            m_targetBindingsDirty[int(pipelineType)] = true;
        }
    }


}

} // renderer_test
