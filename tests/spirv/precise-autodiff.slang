// Autodiff preserves precision requests on primal values. The markers are runtime identities,
// so their derivatives forward operand zero's derivative and have no extra precision policy.
//TEST:SIMPLE(filecheck=PRECISE): -target spirv-asm -entry forwardPrecise -stage compute -fp-mode fast
//TEST:SIMPLE(filecheck=PRECISE): -target spirv-asm -entry backwardPrecise -stage compute -fp-mode fast
//TEST:SIMPLE(filecheck=SELECT): -target spirv-asm -entry forwardSelect -stage compute -fp-mode fast
//TEST:SIMPLE(filecheck=SELECT): -target spirv-asm -entry backwardSelect -stage compute -fp-mode fast

StructuredBuffer<float> inputs;
RWStructuredBuffer<float> outputs;

[Differentiable]
float calculate(float x)
{
    precise float product = x * x;
    return product * x;
}

[Differentiable]
float choose(float x)
{
    return select(x > 0.0f, x * x, x + 1.0f);
}

[numthreads(1, 1, 1)]
void forwardPrecise()
{
    precise DifferentialPair<float> argument = diffPair(inputs[0], inputs[1]);
    let result = fwd_diff(calculate)(argument);
    outputs[0] = result.p;
    outputs[1] = result.d;
}

[numthreads(1, 1, 1)]
void backwardPrecise()
{
    DifferentialPair<float> argument = diffPair(inputs[0], 0.0f);
    bwd_diff(calculate)(argument, inputs[1]);
    outputs[0] = argument.d;
}

[numthreads(1, 1, 1)]
void forwardSelect()
{
    DifferentialPair<float> argument = diffPair(inputs[0], inputs[1]);
    let result = fwd_diff(choose)(argument);
    outputs[0] = result.p;
    outputs[1] = result.d;
}

[numthreads(1, 1, 1)]
void backwardSelect()
{
    DifferentialPair<float> argument = diffPair(inputs[0], 0.0f);
    bwd_diff(choose)(argument, inputs[1]);
    outputs[0] = argument.d;
}

//PRECISE: OpDecorate %[[PRODUCT:[A-Za-z0-9_]+]] NoContraction
//PRECISE-NOT: OpDecorate %{{.*}} NoContraction
//PRECISE: %[[PRODUCT]] = OpFMul %float
//SELECT: OpEntryPoint
//SELECT-NOT: OpDecorate %{{.*}} NoContraction
