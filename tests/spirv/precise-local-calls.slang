// Caller-side demands reach input arguments and output writes. Ordinary function bodies
// remain separate, while `__unsafeForceInlineEarly` explicitly requests body semantics.
//TEST:SIMPLE(filecheck=CALL): -target spirv-asm -entry ordinaryCall -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry ordinaryCall -stage compute -O0
//TEST:SIMPLE(filecheck=INLINE): -target spirv-asm -entry forceInlineCall -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry forceInlineCall -stage compute -O0
//TEST:SIMPLE(filecheck=EARLY): -target spirv-asm -entry earlyInlineCall -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry earlyInlineCall -stage compute -O0
//TEST:SIMPLE(filecheck=OUTPUT): -target spirv-asm -entry outputCall -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry outputCall -stage compute -O0
//TEST:SIMPLE(filecheck=DESTINATION): -target spirv-asm -entry outputDestination -stage compute -O0
//TEST:SIMPLE(filecheck=ZERO): -target spirv-asm -entry outputDestination -stage compute -O0
//TEST:SIMPLE(filecheck=INOUT): -target spirv-asm -entry inoutArgument -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry inoutArgument -stage compute -O0

StructuredBuffer<float4> inputBuffer;
RWStructuredBuffer<float> outputBuffer;

// The arithmetic in an ordinary callee has no precise output contract.
[noinline]
float ordinaryAffine(float value)
{
    return value * 2.0f + 3.0f;
}

// Later inlining does not change the precision policy of the callee's arithmetic.
[ForceInline]
float inlineAffine(float value)
{
    return value * 2.0f + 3.0f;
}

// Early inlining gives the body the same precision behavior as arithmetic in the caller.
[__unsafeForceInlineEarly]
float earlyAffine(float value)
{
    float product = value * 2.0f;
    return product + 3.0f;
}

// A demanded return value constrains the arithmetic passed to the input parameter.
[numthreads(1, 1, 1)]
void ordinaryCall()
{
    float4 inputs = inputBuffer[0];
    float product = inputs.x * inputs.y;
    float argument = product + inputs.z;
    precise float result = ordinaryAffine(argument);
    outputBuffer[0] = result;
}

//CALL: OpDecorate %[[PRODUCT:product[A-Za-z0-9_]*]] NoContraction
//CALL: OpDecorate %[[ARGUMENT:argument[A-Za-z0-9_]*]] NoContraction
//CALL: %[[PRODUCT]] = OpFMul %float
//CALL: %[[ARGUMENT]] = OpFAdd %float %[[PRODUCT]]
//CALL: OpFunctionCall %float {{.*}} %[[ARGUMENT]]

// The argument demand remains local even when the callee is forcibly inlined later.
[numthreads(1, 1, 1)]
void forceInlineCall()
{
    float4 inputs = inputBuffer[0];
    float product = inputs.x * inputs.y;
    float argument = product + inputs.z;
    precise float result = inlineAffine(argument);
    outputBuffer[0] = result;
}

//INLINE: OpDecorate %[[PRODUCT:product[A-Za-z0-9_]*]] NoContraction
//INLINE: OpDecorate %[[ARGUMENT:argument[A-Za-z0-9_]*]] NoContraction
//INLINE: %[[PRODUCT]] = OpFMul %float
//INLINE: %[[ARGUMENT]] = OpFAdd %float %[[PRODUCT]]
//INLINE: OpFMul %float %[[ARGUMENT]] %float_2
//INLINE: OpFAdd %float {{.*}} %float_3

// An early-inlined body contributes its multiply and add to `result`.
[numthreads(1, 1, 1)]
void earlyInlineCall()
{
    precise float result = earlyAffine(inputBuffer[0].x);
    outputBuffer[0] = result;
}

//EARLY: OpDecorate %[[PRODUCT:product[A-Za-z0-9_]*]] NoContraction
//EARLY: OpDecorate %[[SUM:[A-Za-z0-9_]+]] NoContraction
//EARLY: %[[PRODUCT]] = OpFMul %float {{.*}} %float_2
//EARLY: %[[SUM]] = OpFAdd %float %[[PRODUCT]] %float_3

// `out` supplies a destination; the input argument still contributes to that output.
[noinline]
void copyValue(float value, out float destination)
{
    destination = value;
}

// A precise output argument demands other inputs even when the call returns `void`.
[numthreads(1, 1, 1)]
void outputCall()
{
    float4 inputs = inputBuffer[0];
    float product = inputs.x * inputs.y;
    float argument = product + inputs.z;
    precise float result;
    copyValue(argument, result);
    outputBuffer[0] = result;
}

//OUTPUT: OpDecorate %[[PRODUCT:product[A-Za-z0-9_]*]] NoContraction
//OUTPUT: OpDecorate %[[ARGUMENT:argument[A-Za-z0-9_]*]] NoContraction
//OUTPUT: %[[PRODUCT]] = OpFMul %float
//OUTPUT: %[[ARGUMENT]] = OpFAdd %float %[[PRODUCT]]
//OUTPUT: OpFunctionCall %void {{.*}} %[[ARGUMENT]]

// Forming an output-only destination does not demand its index calculation.
[numthreads(1, 1, 1)]
void outputDestination()
{
    float4 inputs = inputBuffer[0];
    float indexProduct = inputs.x * inputs.y;
    precise float values[4] = { inputs.w, inputs.w, inputs.w, inputs.w };
    copyValue(inputs.z, values[uint(indexProduct) & 3]);
    outputBuffer[0] = values[0];
}

//DESTINATION: %indexProduct{{[A-Za-z0-9_]*}} = OpFMul %float
//DESTINATION: OpFunctionCall %void

// The helper copies the incoming `value` into `destination`, then replaces `value` with zero.
[noinline]
void copyAndReset(inout float value, out float destination)
{
    destination = value;
    value = 0.0f;
}

// Demanding the extra output also demands the incoming value and index of `values[index]`.
[numthreads(1, 1, 1)]
void inoutArgument()
{
    float4 inputs = inputBuffer[0];
    float indexProduct = inputs.x * inputs.y;
    float initialProduct = inputs.z * inputs.w;
    float values[4] = { initialProduct, initialProduct, initialProduct, initialProduct };
    precise float result;
    copyAndReset(values[uint(indexProduct) & 3], result);
    outputBuffer[0] = result;
}

//INOUT: OpDecorate %[[INDEX:indexProduct[A-Za-z0-9_]*]] NoContraction
//INOUT: OpDecorate %[[INITIAL:initialProduct[A-Za-z0-9_]*]] NoContraction
//INOUT: %[[INDEX]] = OpFMul %float
//INOUT: %[[INITIAL]] = OpFMul %float
//INOUT: OpFunctionCall %void

// Counts ensure that ordinary callee bodies and output-only indices remain unrestricted.
//ZERO-NOT: NoContraction
//TWO-COUNT-2: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//TWO-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
