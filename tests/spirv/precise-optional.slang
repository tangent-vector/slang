// A precise optional must retain its payload demand without preventing an absent payload from
// folding away before the compiler checks whether `.value` can be read.
//TEST:SIMPLE(filecheck=NONE): -target spirv-asm -entry absentValue -stage compute -fp-mode fast
//TEST:SIMPLE(filecheck=VALUE): -target spirv-asm -entry presentValue -stage compute -fp-mode fast

StructuredBuffer<float> inputs;
RWStructuredBuffer<float> outputs;

[numthreads(1, 1, 1)]
void absentValue()
{
    precise Optional<float> value = none;
    if (let payload = value)
        outputs[0] = payload;
    else
        outputs[0] = 1.0f;
}

[numthreads(1, 1, 1)]
void presentValue()
{
    float product = inputs[0] * inputs[1];
    precise Optional<float> value = product;
    if (let payload = value)
        outputs[0] = payload;
    else
        outputs[0] = 1.0f;
}

//NONE: OpEntryPoint GLCompute %absentValue
//VALUE: OpDecorate %[[PRODUCT:[A-Za-z0-9_]+]] NoContraction
//VALUE-NOT: OpDecorate %{{.*}} NoContraction
//VALUE: %[[PRODUCT]] = OpFMul %float
