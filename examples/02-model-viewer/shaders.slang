// shaders.slang

// This file provides a simple vertex and fragment shader that can be compiled
// using Slang. This code should also be valid as HLSL, and thus it does not
// use any of the new language features supported by Slang.

cbuffer Uniforms
{
    float4x4    viewProjection;

    float3      lightDir;
    float3      lightColor;

    float3      materialDiffuseColor;

    float4x4    modelTransform;
    float4x4    inverseTransposeModelTransform;
}

struct AssembledVertex
{
    float3 position : POSITION;
    float3 normal   : NORMAL;
    float2 uv       : UV;
};

struct CoarseVertex
{
    float3 worldPosition;
    float3 worldNormal;
    float2 uv;
};

struct Fragment
{
    float4 color;
};


// Vertex  Shader

struct VertexStageInput
{
    AssembledVertex assembledVertex;
};

struct VertexStageOutput
{
    CoarseVertex    coarseVertex    : CoarseVertex;
    float4          sv_position     : SV_Position;
};

VertexStageOutput vertexMain(VertexStageInput input)
{
    VertexStageOutput output;

    float3 position = input.assembledVertex.position;
    float3 normal   = input.assembledVertex.normal;
    float2 uv       = input.assembledVertex.uv;

    float3 worldPosition = mul(modelTransform, float4(position, 1.0)).xyz;
    float3 worldNormal = normal; // mul(inverseTransposeModelTransform, float4(normal, 0.0)).xyz;

    output.coarseVertex.worldPosition = worldPosition;
    output.coarseVertex.worldNormal   = worldNormal;
    output.coarseVertex.uv            = uv;

    output.sv_position = mul(viewProjection, float4(worldPosition, 1.0));

    return output;
}

// Fragment Shader

struct FragmentStageInput
{
    CoarseVertex    coarseVertex    : CoarseVertex;
};

struct FragmentStageOutput
{
    Fragment fragment   : SV_Target;
};

FragmentStageOutput fragmentMain(FragmentStageInput input)
{
    FragmentStageOutput output;

    float3 N = normalize(input.coarseVertex.worldNormal);
    float3 L = normalize(lightDir);

    output.fragment.color.xyz = materialDiffuseColor * max(0, dot(N, L));
    output.fragment.color.w = 1.0f;

    return output;
}
