// Resource contents are identified by the resource value, not by the storage holding a descriptor.
// The CPP path also reloads collected shader bindings through separate field-address instructions.
//TEST:SIMPLE(filecheck=IR): -target cpp -entry sameBinding -stage compute -O0 -dump-ir-after propagatePreciseDemands
//TEST:SIMPLE(filecheck=IR): -target cpp -entry mutableCopies -stage compute -O0 -dump-ir-after propagatePreciseDemands

StructuredBuffer<float4> inputs;
RWStructuredBuffer<float> memory;
RWStructuredBuffer<float> outputs;

__intrinsic_op(rwstructuredBufferStore)
void nativeStore(RWStructuredBuffer<float> target, uint index, float value);

// The two uses become separate loads from the same compiler-collected binding field.
[numthreads(1, 1, 1)]
void sameBinding()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    nativeStore(memory, 0, product);
    precise float result = memory.Load(0);
    outputs[0] = result;
}

// An ordinary call keeps the descriptors in mutable locals while assigning both the same resource.
[noinline]
void initializeCopies(
    out RWStructuredBuffer<float> first,
    out RWStructuredBuffer<float> second,
    RWStructuredBuffer<float> resource)
{
    first = resource;
    second = resource;
}

// Disjoint local descriptor storage does not prove that the described resource contents differ.
[numthreads(1, 1, 1)]
void mutableCopies()
{
    float4 data = inputs[0];
    RWStructuredBuffer<float> first;
    RWStructuredBuffer<float> second;
    initializeCopies(first, second, memory);
    float product = data.x * data.y;
    nativeStore(first, 0, product);
    precise float result = second.Load(0);
    outputs[0] = result;
}

//IR: ### AFTER propagatePreciseDemands:
//IR: %[[POLICY:[A-Za-z0-9_]+]] {{.*}} = Precise
//IR: %[[PRODUCT:product[A-Za-z0-9_]*]] {{.*}} = mul({{.*}}, %[[POLICY]])
