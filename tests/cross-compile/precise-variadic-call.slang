// A variadic interface lookup can retain a concrete type pack in its signature after call
// arguments have expanded. A demanded result must still constrain each input argument.
//TEST:SIMPLE(filecheck=CHECK): -target hlsl -entry main -stage compute -O0 -dump-ir-after propagatePreciseDemands
//TEST:SIMPLE(filecheck=COUNT): -target hlsl -entry main -stage compute -O0

interface IAccumulator<each Ts>
{
    float accumulate(expand each Ts values);
}

struct Sum : IAccumulator<float, float>
{
    float accumulate(float a, float b) { return a + b; }
}

struct Product : IAccumulator<float, float>
{
    float accumulate(float a, float b) { return a * b; }
}

[noinline]
IAccumulator<float, float> chooseAccumulator(bool useSum)
{
    if (useSum)
        return Sum();
    return Product();
}

StructuredBuffer<float4> inputs;
RWStructuredBuffer<float> outputs;

[numthreads(1, 1, 1)]
void main()
{
    float4 data = inputs[0];
    let accumulator = chooseAccumulator(data.x > 0.0f);
    float firstProduct = data.x * data.y;
    float secondProduct = data.z * data.w;
    precise float result = accumulator.accumulate(firstProduct, secondProduct);
    outputs[0] = result;
}

//CHECK: ### AFTER propagatePreciseDemands:
//CHECK: %[[POLICY:[A-Za-z0-9_]+]] {{.*}} = Precise
//CHECK: %[[FIRST:firstProduct[A-Za-z0-9_]*]] {{.*}} = mul({{.*}}, %[[POLICY]])
//CHECK: %[[SECOND:secondProduct[A-Za-z0-9_]*]] {{.*}} = mul({{.*}}, %[[POLICY]])

// The two caller inputs are constrained; arithmetic inside either ordinary implementation stays
// outside the demand even when dynamic dispatch is resolved or the implementation is inlined.
//COUNT-COUNT-2: precise float
//COUNT-NOT: precise float
