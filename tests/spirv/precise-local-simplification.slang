// Primitive simplification can remove a dependency before precision propagation. Calls
// still demand all evaluated input arguments, independently of an ordinary callee's body.
//TEST:SIMPLE(filecheck=PRIMITIVE): -target spirv-asm -entry primitiveCancellation -stage compute -O0
//TEST:SIMPLE(filecheck=ZERO): -target spirv-asm -entry primitiveCancellation -stage compute -O0
//TEST:SIMPLE(filecheck=CALL): -target spirv-asm -entry ordinaryConstantCall -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry ordinaryConstantCall -stage compute -O0

StructuredBuffer<float4> inputBuffer;
RWStructuredBuffer<float> outputBuffer;

// The compiler folds `product - product` to zero before solving precision demands.
// The separate output keeps the original arithmetic alive without demanding it.
[numthreads(1, 1, 1)]
void primitiveCancellation()
{
    float4 inputs = inputBuffer[0];
    float product = inputs.x * inputs.y + inputs.z;
    precise float zero = product - product;
    outputBuffer[0] = product;
    outputBuffer[1] = zero;
}

//PRIMITIVE: %[[PRODUCT:[A-Za-z0-9_]+]] = OpFMul %float
//PRIMITIVE: %[[SUM:[A-Za-z0-9_]+]] = OpFAdd %float %[[PRODUCT]]
//PRIMITIVE: OpStore {{.*}} %[[SUM]]
//PRIMITIVE: OpStore {{.*}} %float_0

// Returning a constant does not change the caller's input-argument dependency rule.
[noinline]
float zeroHelper(float ignored)
{
    return 0.0f;
}

// The demand on the call reaches `product` even though `zeroHelper` ignores its input.
[numthreads(1, 1, 1)]
void ordinaryConstantCall()
{
    float4 inputs = inputBuffer[0];
    float product = inputs.x * inputs.y + inputs.z;
    precise float zero = zeroHelper(product);
    outputBuffer[0] = product;
    outputBuffer[1] = zero;
}

//CALL: OpDecorate %[[PRODUCT:[A-Za-z0-9_]+]] NoContraction
//CALL: OpDecorate %[[SUM:[A-Za-z0-9_]+]] NoContraction
//CALL: %[[PRODUCT]] = OpFMul %float
//CALL: %[[SUM]] = OpFAdd %float %[[PRODUCT]]
//CALL: OpFunctionCall %float {{.*}} %[[SUM]]

// The exact counts describe this implementation's selected simplification behavior.
//ZERO-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//TWO-COUNT-2: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//TWO-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
