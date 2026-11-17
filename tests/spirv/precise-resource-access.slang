// Native resource operations and pointer accesses to the same contents share precision demands.
// The intrinsic declarations below exercise the IR memory operations directly; ordinary helper
// function bodies remain separate precision boundaries.
//TEST:SIMPLE(filecheck=MATH): -target spirv-asm -entry pointerStoreNativeLoad -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry pointerStoreNativeLoad -stage compute -O0
//TEST:SIMPLE(filecheck=MATH): -target spirv-asm -entry nativeStorePointerLoad -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry nativeStorePointerLoad -stage compute -O0
//TEST:SIMPLE(filecheck=MATH): -target spirv-asm -entry nativeByteAccess -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry nativeByteAccess -stage compute -O0
//TEST:SIMPLE(filecheck=MATH): -target spirv-asm -entry nativeImageAccess -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry nativeImageAccess -stage compute -O0
//TEST:SIMPLE(filecheck=MATH): -target spirv-asm -entry disjointImageCoordinates -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry disjointImageCoordinates -stage compute -O0
//TEST:SIMPLE(filecheck=MATH): -target spirv-asm -entry disjointResourcesAndIndices -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry disjointResourcesAndIndices -stage compute -O0
//TEST:SIMPLE(filecheck=DESCRIPTOR): -target spirv-asm -entry preciseDescriptor -stage compute -O0
//TEST:SIMPLE(filecheck=STATUS): -target hlsl -entry declaredNativeStatus -stage compute -O0
//TEST:SIMPLE(filecheck=STATUSINDEX): -target hlsl -entry nativeStatusDestination -stage compute -O0

StructuredBuffer<float4> inputs;
RWStructuredBuffer<float> memory;
RWStructuredBuffer<float> otherMemory;
RWStructuredBuffer<float> outputs;
RWStructuredBuffer<uint> statuses;
RWByteAddressBuffer bytes;
RWTexture2D<float> image;

__intrinsic_op(rwstructuredBufferStore)
void nativeStore(RWStructuredBuffer<float> target, uint index, float value);

__intrinsic_op(rwstructuredBufferLoadStatus)
float nativeStatusLoad(RWStructuredBuffer<float> target, uint index, out uint status);

__intrinsic_op(byteAddressBufferLoad)
float nativeByteLoad(RWByteAddressBuffer target, uint offset, uint alignment);

__intrinsic_op(byteAddressBufferStore)
void nativeByteStore(RWByteAddressBuffer target, uint offset, uint alignment, float value);

__intrinsic_op(imageLoad)
float nativeImageLoad(RWTexture2D<float> target, int2 coordinate);

__intrinsic_op(imageStore)
void nativeImageStore(RWTexture2D<float> target, int2 coordinate, float value);

// The subscript writes through an element address; Load reads the same resource natively.
[numthreads(1, 1, 1)]
void pointerStoreNativeLoad()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    float sum = product + data.z;
    memory[0] = sum;
    precise float result = memory.Load(0);
    outputs[0] = result;
}

// The reverse direction must work as well: a native store supplies a pointer-based read.
[numthreads(1, 1, 1)]
void nativeStorePointerLoad()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    float sum = product + data.z;
    nativeStore(memory, 0, sum);
    precise float result = memory[0];
    outputs[0] = result;
}

// Byte-buffer stores place alignment before the value in their native operand list.
[numthreads(1, 1, 1)]
void nativeByteAccess()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    float sum = product + data.z;
    nativeByteStore(bytes, 0, 4, sum);
    precise float result = nativeByteLoad(bytes, 0, 4);
    outputs[0] = result;
}

// Image coordinates identify resource contents without qualifying the image descriptor.
[numthreads(1, 1, 1)]
void nativeImageAccess()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    float sum = product + data.z;
    nativeImageStore(image, int2(0, 0), sum);
    precise float result = nativeImageLoad(image, int2(0, 0));
    outputs[0] = result;
}

// One different literal coordinate component proves the texels are separate locations.
[numthreads(1, 1, 1)]
void disjointImageCoordinates()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    float sum = product + data.z;
    nativeImageStore(image, int2(0, 0), sum);
    nativeImageStore(image, int2(0, 1), data.z * data.w);
    precise float result = nativeImageLoad(image, int2(0, 0));
    outputs[0] = result;
    outputs[1] = nativeImageLoad(image, int2(0, 1));
}

// The other writes stay observable, but neither a distinct resource nor a different known
// element of the demanded resource contributes to the precise read.
[numthreads(1, 1, 1)]
void disjointResourcesAndIndices()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    float sum = product + data.z;
    nativeStore(memory, 0, sum);
    memory[1] = data.z * data.w;
    nativeStore(otherMemory, 0, data.x + data.w);
    precise float result = memory.Load(0);
    outputs[0] = result;
    outputs[1] = memory.Load(1);
    outputs[2] = otherMemory.Load(0);
}

// A precise descriptor constrains its own computation, rather than every stored resource value.
[numthreads(1, 1, 1)]
void preciseDescriptor()
{
    float4 data = inputs[0];
    precise RWStructuredBuffer<float> selected = memory;
    nativeStore(selected, 0, data.x * data.y);
    outputs[0] = selected.Load(0);
}

// A declared status-output policy demands computation of the load's incoming index.
[numthreads(1, 1, 1)]
void declaredNativeStatus()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    precise uint status = 0;
    outputs[0] = nativeStatusLoad(memory, uint(product), status);
    statuses[0] = status;
}

// A demanded numeric result does not demand arithmetic that forms an output-only address.
[numthreads(1, 1, 1)]
void nativeStatusDestination()
{
    float4 data = inputs[0];
    float indexProduct = data.x * data.y;
    precise float result = nativeStatusLoad(memory, 0, statuses[uint(indexProduct)]);
    outputs[0] = result;
}

//MATH: OpDecorate %[[PRODUCT:product[A-Za-z0-9_]*]] NoContraction
//MATH: OpDecorate %[[SUM:sum[A-Za-z0-9_]*]] NoContraction
//MATH: %[[PRODUCT]] = OpFMul %float
//MATH: %[[SUM]] = OpFAdd %float %[[PRODUCT]]
//TWO-COUNT-2: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//TWO-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//DESCRIPTOR-NOT: NoContraction
//DESCRIPTOR: OpFMul %float
//DESCRIPTOR-NOT: NoContraction
//STATUS: precise float product
//STATUSINDEX-NOT: precise float
//STATUSINDEX: statuses{{.*}}[uint({{.*}} * {{.*}})]
//STATUSINDEX-NOT: precise float
