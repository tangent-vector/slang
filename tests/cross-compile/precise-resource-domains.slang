// CPU parameter collection loads resource descriptors from separate fields of a globals
// aggregate. Their contents remain independent of each other and of ordinary local storage.
//TEST:SIMPLE(filecheck=IR): -target cpp -entry main -stage compute -O0 -dump-ir-after propagatePreciseDemands

StructuredBuffer<float4> inputs;
RWStructuredBuffer<float> memory;
RWStructuredBuffer<float> outputs;

__intrinsic_op(rwstructuredBufferStore)
void nativeStore(RWStructuredBuffer<float> target, uint index, float value);

[numthreads(1, 1, 1)]
void main()
{
    float4 data = inputs[0];
    float values[2] = {0.0f, 0.0f};
    float product = data.x * data.y;
    values[uint(data.w) & 1] = product;
    float ordinaryProduct = data.z * data.w;
    nativeStore(memory, 0, ordinaryProduct);
    precise float result = values[0];
    outputs[0] = result;
    outputs[1] = memory.Load(0);
}

//IR: ### AFTER propagatePreciseDemands:
//IR: %[[POLICY:[A-Za-z0-9_]+]] {{.*}} = Precise
//IR: %[[PRODUCT:product[A-Za-z0-9_]*]] {{.*}} = mul({{.*}}, %[[POLICY]])
//IR: %[[ORDINARY:ordinaryProduct[A-Za-z0-9_]*]] {{.*}} = mul(%{{[A-Za-z0-9_]+}}, %{{[A-Za-z0-9_]+}})
