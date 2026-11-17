// Native atomic operations participate in the same local storage policy as ordinary accesses.
// Their integer values carry demands back to floating-point computations before conversion.
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry atomicStoreLoad -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry atomicUpdate -stage compute -O0
//TEST:SIMPLE(filecheck=THREE): -target spirv-asm -entry atomicCompareExchange -stage compute -O0
//TEST:SIMPLE(filecheck=ONE): -target spirv-asm -entry independentAtomicElement -stage compute -O0
//TEST:SIMPLE(filecheck=ONE): -target spirv-asm -entry declaredAtomicStore -stage compute -O0

StructuredBuffer<float4> inputs;
RWStructuredBuffer<Atomic<uint>> atomicMemory;
RWStructuredBuffer<uint> outputs;
groupshared precise Atomic<uint> declaredAtomic;

// An atomic load demands the computation of a value supplied by an atomic store.
[numthreads(1, 1, 1)]
void atomicStoreLoad()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    float sum = product + data.z;
    atomicMemory[0].store(uint(sum));
    precise uint result = atomicMemory[0].load();
    outputs[0] = result;
}

// A demanded update includes both its explicit addend and the previously stored contents.
[numthreads(1, 1, 1)]
void atomicUpdate()
{
    float4 data = inputs[0];
    float initial = data.x * data.y;
    float addend = data.z * data.w;
    atomicMemory[0].store(uint(initial));
    atomicMemory[0].add(uint(addend));
    precise uint result = atomicMemory[0].load();
    outputs[0] = result;
}

// Compare/exchange evaluates its comparison and desired values as well as reading old contents.
[numthreads(1, 1, 1)]
void atomicCompareExchange()
{
    float4 data = inputs[0];
    float initial = data.x + data.y;
    float comparison = data.x * data.y;
    float desired = data.z * data.w;
    atomicMemory[0].store(uint(initial));
    atomicMemory[0].compareExchange(uint(comparison), uint(desired));
    precise uint result = atomicMemory[0].load();
    outputs[0] = result;
}

// A different known resource element remains independent even though its write is observable.
[numthreads(1, 1, 1)]
void independentAtomicElement()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    float ordinaryProduct = data.z * data.w;
    atomicMemory[0].store(uint(product));
    atomicMemory[1].store(uint(ordinaryProduct));
    precise uint result = atomicMemory[0].load();
    outputs[0] = result;
    outputs[1] = atomicMemory[1].load();
}

// A declared atomic destination constrains its store even without a demanded read afterward.
[numthreads(1, 1, 1)]
void declaredAtomicStore()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    declaredAtomic.store(uint(product));
    outputs[0] = declaredAtomic.load();
}

//TWO-COUNT-2: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//TWO-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//TWO: OpAtomicStore
//TWO: OpAtomicLoad
//THREE-COUNT-3: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//THREE-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//THREE: OpAtomicCompareExchange
//THREE: OpAtomicLoad
//ONE-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//ONE: OpDecorate %[[PRODUCT:product[A-Za-z0-9_]*]] NoContraction
//ONE-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//ONE: %[[PRODUCT]] = OpFMul %float
//ONE: OpAtomicStore
//ONE: OpAtomicLoad
