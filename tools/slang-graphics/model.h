// model.h
#pragma once

#include "render.h"
#include "vector-math.h"

#include <vector>

namespace slang_graphics {

using Slang::RefObject;
using Slang::RefPtr;

typedef SlangResult Result;

struct Material : RefObject
{
    glm::vec3           diffuseColor;
    TextureResource*    diffuseMap;
};

struct Model : RefObject
{
    struct Vertex
    {
        glm::vec3 position;
        glm::vec3 normal;
        glm::vec2 uv;
    };

    typedef uint32_t Index;

    struct Mesh : RefObject
    {
        int firstIndex;
        int indexCount;

        RefPtr<Material> material;
    };

    BufferResource*     vertexBuffer;
    BufferResource*     indexBuffer;
    PrimitiveTopology   primitiveTopology;
    int                 vertexCount;
    int                 indexCount;

    std::vector<RefPtr<Mesh> >   meshes;

    typedef uint32_t LoadFlags;
    enum LoadFlag : LoadFlags
    {
        FlipWinding = 1 << 0,
    };

    Result initialize(
        Renderer*   renderer,
        char const* inputPath,
        LoadFlags   loadFlags = 0,
        float       scale = 1.0f);
};


} // slang_graphics
