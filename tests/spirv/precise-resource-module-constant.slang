// A vector computation in a static constant can survive scalar constant folding. Its explicit
// demand belongs to the module expression, while later arithmetic in a function stays ordinary.
//TEST:SIMPLE(filecheck=HLSL): -target hlsl -entry main -stage compute -O0
//TEST:SIMPLE(filecheck=GLSL): -target glsl -entry main -stage compute -O0
//TEST:SIMPLE(filecheck=SPIRV): -target spirv-asm -entry main -stage compute -O0

static const precise float4 product = float4(1, 2, 3, 4) * float4(5, 6, 7, 8);
StructuredBuffer<float4> inputs;
RWStructuredBuffer<float4> outputs;

[numthreads(1, 1, 1)]
void main()
{
    outputs[0] = product + inputs[0];
}

//HLSL: precise static const float4 product
//HLSL: outputs
//GLSL: precise const vec4 product
//GLSL: void main
//SPIRV: OpDecorate %[[PRODUCT:product[A-Za-z0-9_]*]] NoContraction
//SPIRV-NOT: NoContraction
//SPIRV: %[[PRODUCT]] = OpFMul
//SPIRV: OpFAdd
