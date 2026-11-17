// A raw pointer derived from one buffer element can address another element. Such an offset
// invalidates the original element-index proof across loads, writes, reference calls, and atomics.
// A descriptor cast can likewise change the element stride associated with a logical index.
//TEST:SIMPLE(filecheck=IR): -target cpp -entry nativeStorePointerRead -stage compute -O0 -dump-ir-after propagatePreciseDemands
//TEST:SIMPLE(filecheck=IR): -target cpp -entry pointerWriteNativeRead -stage compute -O0 -dump-ir-after propagatePreciseDemands
//TEST:SIMPLE(filecheck=IR): -target cpp -entry nativeStoreReferenceRead -stage compute -O0 -dump-ir-after propagatePreciseDemands
//TEST:SIMPLE(filecheck=IR): -target cpp -entry referenceWriteNativeRead -stage compute -O0 -dump-ir-after propagatePreciseDemands
//TEST:SIMPLE(filecheck=IR): -target cpp -entry nativeStoreAtomicRead -stage compute -O0 -dump-ir-after propagatePreciseDemands
//TEST:SIMPLE(filecheck=IR): -target cpp -entry atomicWriteNativeRead -stage compute -O0 -dump-ir-after propagatePreciseDemands
//TEST:SIMPLE(filecheck=IR): -target cpp -entry descriptorView -stage compute -O0 -dump-ir-after propagatePreciseDemands
//TEST:SIMPLE(filecheck=FIELD): -target cpp -entry fieldProjection -stage compute -O0 -dump-ir-after propagatePreciseDemands

StructuredBuffer<float4> inputs;
RWStructuredBuffer<float> memory;
RWStructuredBuffer<uint> uintMemory;
RWStructuredBuffer<float> outputs;

struct Element
{
    float value;
};
RWStructuredBuffer<Element> elements;

__intrinsic_op(rwstructuredBufferStore)
void nativeStore(RWStructuredBuffer<float> target, uint index, float value);
__intrinsic_op(rwstructuredBufferStore)
void nativeStore(RWStructuredBuffer<uint> target, uint index, uint value);
__intrinsic_op(atomicAdd)
uint nativeAtomicAdd(uint* pointer, uint value, int order);

[noinline]
float readValue(__ref float value)
{
    return value;
}

[noinline]
void writeValue(float value, out float destination)
{
    destination = value;
}

[numthreads(1, 1, 1)]
void nativeStorePointerRead()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    nativeStore(memory, 1, product);
    float* pointer = &memory[0];
    precise float result = pointer[1];
    outputs[0] = result;
}

[numthreads(1, 1, 1)]
void pointerWriteNativeRead()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    float* pointer = &memory[0];
    pointer[1] = product;
    precise float result = memory.Load(1);
    outputs[0] = result;
}

[numthreads(1, 1, 1)]
void nativeStoreReferenceRead()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    nativeStore(memory, 1, product);
    float* pointer = &memory[0];
    precise float result = readValue(pointer[1]);
    outputs[0] = result;
}

[numthreads(1, 1, 1)]
void referenceWriteNativeRead()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    float* pointer = &memory[0];
    writeValue(product, pointer[1]);
    precise float result = memory.Load(1);
    outputs[0] = result;
}

// The direct atomic intrinsic isolates memory propagation from the standard atomic API's targets.
[numthreads(1, 1, 1)]
void nativeStoreAtomicRead()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    nativeStore(uintMemory, 1, uint(product));
    uint* pointer = &uintMemory[0];
    precise uint result = nativeAtomicAdd(&pointer[1], 0, 0);
    outputs[0] = float(result);
}

[numthreads(1, 1, 1)]
void atomicWriteNativeRead()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    uint* pointer = &uintMemory[0];
    nativeAtomicAdd(&pointer[1], uint(product), 0);
    precise uint result = uintMemory.Load(1);
    outputs[0] = float(result);
}

// Element zero of the float2 view includes element one of the original float view.
[numthreads(1, 1, 1)]
void descriptorView()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    nativeStore(memory, 1, product);
    RWStructuredBuffer<float2> wider = bit_cast<RWStructuredBuffer<float2>>(memory);
    precise float2 result = wider.Load(0);
    outputs[0] = result.y;
}

// A struct field stays within its containing element, so distinct element indices remain useful.
[numthreads(1, 1, 1)]
void fieldProjection()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    float ordinaryProduct = data.z * data.w;
    elements[0].value = product;
    elements[1].value = ordinaryProduct;
    precise float result = elements[0].value;
    outputs[0] = result;
    outputs[1] = elements[1].value;
}

//IR: ### AFTER propagatePreciseDemands:
//IR: %[[POLICY:[A-Za-z0-9_]+]] {{.*}} = Precise
//IR: %[[PRODUCT:product[A-Za-z0-9_]*]] {{.*}} = mul({{.*}}, %[[POLICY]])
//FIELD: ### AFTER propagatePreciseDemands:
//FIELD: %[[POLICY:[A-Za-z0-9_]+]] {{.*}} = Precise
//FIELD: %[[PRODUCT:product[A-Za-z0-9_]*]] {{.*}} = mul({{.*}}, %[[POLICY]])
//FIELD: %[[ORDINARY:ordinaryProduct[A-Za-z0-9_]*]] {{.*}} = mul(%{{[A-Za-z0-9_]+}}, %{{[A-Za-z0-9_]+}})
