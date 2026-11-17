// Inferred demands on globals belong to the function being analyzed. Declared demands,
// including legacy field qualifiers inside an aggregate, constrain each affected write.
//TEST:SIMPLE(filecheck=INFERRED): -target spirv-asm -entry inferredGlobal -stage compute -O0
//TEST:SIMPLE(filecheck=ONE): -target spirv-asm -entry inferredGlobal -stage compute -O0
//TEST:SIMPLE(filecheck=DECLARED): -target spirv-asm -entry declaredGlobal -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry declaredGlobal -stage compute -O0
//TEST:SIMPLE(filecheck=AGGREGATE): -target spirv-asm -entry containedField -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry containedField -stage compute -O0
//TEST:SIMPLE(filecheck=PHANTOM): -target spirv-asm -entry phantomTypeArgument -stage compute -O0

StructuredBuffer<float4> inputBuffer;
RWStructuredBuffer<float> outputBuffer;
static float sharedValue;
static precise float declaredValue;

// The demanded read constrains the write within this function.
[noinline]
float writeAndDemand(float4 inputs)
{
    float demandedProduct = inputs.x * inputs.y;
    sharedValue = demandedProduct;
    precise float result = sharedValue;
    return result;
}

// This write has no demand, even though another function reads the same global precisely.
[noinline]
float writeOrdinary(float4 inputs)
{
    float ordinaryProduct = inputs.z * inputs.w;
    sharedValue = ordinaryProduct;
    return sharedValue;
}

// Both calls remain reachable so propagation through the global cannot hide behind DCE.
[numthreads(1, 1, 1)]
void inferredGlobal()
{
    float4 inputs = inputBuffer[0];
    outputBuffer[0] = writeAndDemand(inputs);
    outputBuffer[1] = writeOrdinary(inputs);
}

//INFERRED: OpDecorate %[[DEMANDED:demandedProduct[A-Za-z0-9_]*]] NoContraction
//INFERRED: %[[DEMANDED]] = OpFMul %float
//INFERRED: %ordinaryProduct{{[A-Za-z0-9_]*}} = OpFMul %float

// The declaration of `declaredValue` constrains writes in either helper.
[noinline]
float writeDeclaredFirst(float4 inputs)
{
    float firstProduct = inputs.x * inputs.y;
    declaredValue = firstProduct;
    return declaredValue;
}

// The second helper writes another product to the same explicitly precise global.
[noinline]
float writeDeclaredSecond(float4 inputs)
{
    float secondProduct = inputs.z * inputs.w;
    declaredValue = secondProduct;
    return declaredValue;
}

// Each helper performs a write governed by the same explicit declaration policy.
[numthreads(1, 1, 1)]
void declaredGlobal()
{
    float4 inputs = inputBuffer[0];
    outputBuffer[0] = writeDeclaredFirst(inputs);
    outputBuffer[1] = writeDeclaredSecond(inputs);
}

//DECLARED: OpDecorate %[[FIRST:firstProduct[A-Za-z0-9_]*]] NoContraction
//DECLARED: OpDecorate %[[SECOND:secondProduct[A-Za-z0-9_]*]] NoContraction
//DECLARED: %[[FIRST]] = OpFMul %float
//DECLARED: %[[SECOND]] = OpFMul %float

// A `PreciseLeaf` stores one value under an explicit precision demand.
struct PreciseLeaf
{
    precise float value;
};

// A `Wrapper<T>` stores `T` by value, including any transitively contained precise field.
struct Wrapper<T>
{
    T value;
};

// The unused type argument contributes no stored fields to `Phantom<T>`.
struct Phantom<T>
{
    float value;
};

// Initializing the complete aggregate writes its transitively contained precise field.
[numthreads(1, 1, 1)]
void containedField()
{
    float4 inputs = inputBuffer[0];
    float product = inputs.x * inputs.y;
    float sum = product + inputs.z;
    Wrapper<PreciseLeaf> result = { { sum } };
    outputBuffer[0] = result.value.value;
}

//AGGREGATE: OpDecorate %[[PRODUCT:product[A-Za-z0-9_]*]] NoContraction
//AGGREGATE: OpDecorate %[[SUM:sum[A-Za-z0-9_]*]] NoContraction
//AGGREGATE: %[[PRODUCT]] = OpFMul %float
//AGGREGATE: %[[SUM]] = OpFAdd %float %[[PRODUCT]]

// Merely naming a precise-containing type argument does not constrain this float field.
[numthreads(1, 1, 1)]
void phantomTypeArgument()
{
    float4 inputs = inputBuffer[0];
    float ordinaryProduct = inputs.x * inputs.y;
    Phantom<PreciseLeaf> result = { ordinaryProduct };
    outputBuffer[0] = result.value;
}

//PHANTOM-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//PHANTOM: %ordinaryProduct{{[A-Za-z0-9_]*}} = OpFMul %float
//PHANTOM-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction

// Counts complement the operation checks and reject propagation into the ordinary helper.
//ONE-COUNT-1: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//ONE-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//TWO-COUNT-2: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//TWO-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
