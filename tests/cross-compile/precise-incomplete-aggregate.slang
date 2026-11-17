// The static initializer can request `makeValue` while `Value` is still being lowered, before
// its instance fields have been appended. The returned whole value must retain its conditional
// precision demand until the compiler can inspect the complete field list.
//TEST:SIMPLE(filecheck=CHECK): -target cpp -entry main -stage compute -O0 -dump-ir-after propagatePreciseDemands

StructuredBuffer<float4> inputs;
RWStructuredBuffer<float> outputs;

struct Value
{
    static Value initial = makeValue(inputs[0]);
    precise float preciseField;
    float ordinaryField;
};

[noinline]
Value makeValue(float4 data)
{
    float firstProduct = data.x * data.y;
    float secondProduct = data.z * data.w;
    return {firstProduct, secondProduct};
}

[numthreads(1, 1, 1)]
void main()
{
    outputs[0] = Value.initial.preciseField + Value.initial.ordinaryField;
}

//CHECK: ### AFTER propagatePreciseDemands:
//CHECK: %[[POLICY:[A-Za-z0-9_]+]] {{.*}} = Precise
//CHECK: %[[FIRST:firstProduct[A-Za-z0-9_]*]] {{.*}} = mul({{.*}}, %[[POLICY]])
//CHECK: %[[SECOND:secondProduct[A-Za-z0-9_]*]] {{.*}} = mul({{.*}}, %[[POLICY]])
