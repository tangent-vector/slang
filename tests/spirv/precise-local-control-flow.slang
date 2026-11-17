// The compiler propagates precision demands to all possible results of control flow.
// Logical operators depend on both Boolean operands, even when lowering turns one
// operand into a branch condition.
//TEST:SIMPLE(filecheck=PHI): -target spirv-asm -entry branchValues -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry branchValues -stage compute -O0
//TEST:SIMPLE(filecheck=LOOP): -target spirv-asm -entry loopValues -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry loopValues -stage compute -O0
//TEST:SIMPLE(filecheck=LOGICAL): -target spirv-asm -entry logicalOperands -stage compute -O0
//TEST:SIMPLE(filecheck=THREE): -target spirv-asm -entry logicalOperands -stage compute -O0
//TEST:SIMPLE(filecheck=EAGER): -target spirv-asm -entry eagerSelection -stage compute -O0
//TEST:SIMPLE(filecheck=THREE): -target spirv-asm -entry eagerSelection -stage compute -O0
//TEST:SIMPLE(filecheck=CONTROL): -target spirv-asm -entry controlPredicates -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry controlPredicates -stage compute -O0
//TEST:SIMPLE(filecheck=VECTOR_CONDITIONAL): -target spirv-asm -entry vectorConditional -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry vectorConditional -stage compute -O0

StructuredBuffer<float4> inputBuffer;
RWStructuredBuffer<float> outputBuffer;
StructuredBuffer<float2> vectorInputBuffer;
RWStructuredBuffer<float2> vectorOutputBuffer;

// Each branch contributes a possible value to the demanded read of `selected`.
[numthreads(1, 1, 1)]
void branchValues()
{
    float4 inputs = inputBuffer[0];
    float selected;
    if (inputs.w > 0.0f)
    {
        float leftProduct = inputs.x * 2.0f;
        selected = leftProduct;
    }
    else
    {
        float rightProduct = inputs.y * 3.0f;
        selected = rightProduct;
    }
    precise float result = selected;
    outputBuffer[0] = result;
}

//PHI: OpDecorate %[[LEFT:leftProduct[A-Za-z0-9_]*]] NoContraction
//PHI: OpDecorate %[[RIGHT:rightProduct[A-Za-z0-9_]*]] NoContraction
//PHI: %[[LEFT]] = OpFMul %float
//PHI: %[[RIGHT]] = OpFMul %float

// The demand on `sum` reaches the loop-carried addition and each contributing product.
[numthreads(1, 1, 1)]
void loopValues()
{
    float4 inputs = inputBuffer[0];
    float limitProduct = inputs.z * inputs.w;
    float sum = 0.0f;
    for (uint index = 0; index < uint(limitProduct); ++index)
    {
        float iterationProduct = inputBuffer[index].x * inputs.y;
        sum += iterationProduct;
    }
    precise float result = sum;
    outputBuffer[0] = result;
}

//LOOP: OpDecorate %[[PRODUCT:iterationProduct[A-Za-z0-9_]*]] NoContraction
//LOOP: OpDecorate %[[SUM:[A-Za-z0-9_]+]] NoContraction
//LOOP: OpFMul %float
//LOOP: %[[PRODUCT]] = OpFMul %float
//LOOP: %[[SUM]] = OpFAdd %float {{.*}} %[[PRODUCT]]

// Both operands of `&&` and `||` contribute to the demanded Boolean result.
[numthreads(1, 1, 1)]
void logicalOperands()
{
    float4 inputs = inputBuffer[0];
    float leftProduct = inputs.x * 2.0f;
    float rightProduct = inputs.y * 3.0f;
    float lastProduct = inputs.z * 4.0f;
    precise bool result = (leftProduct > 0.0f && rightProduct > 0.0f) || lastProduct > 0.0f;
    outputBuffer[0] = result ? 1.0f : 0.0f;
}

//LOGICAL: OpDecorate %[[LEFT:leftProduct[A-Za-z0-9_]*]] NoContraction
//LOGICAL: OpDecorate %[[RIGHT:rightProduct[A-Za-z0-9_]*]] NoContraction
//LOGICAL: OpDecorate %[[LAST:lastProduct[A-Za-z0-9_]*]] NoContraction
//LOGICAL: %[[LEFT]] = OpFMul %float
//LOGICAL: %[[RIGHT]] = OpFMul %float
//LOGICAL: %[[LAST]] = OpFMul %float

// Unlike a conditional expression, an eager `select` demands its predicate input too.
[numthreads(1, 1, 1)]
void eagerSelection()
{
    float4 inputs = inputBuffer[0];
    float conditionProduct = inputs.z * 2.0f;
    float leftProduct = inputs.x * 3.0f;
    float rightProduct = inputs.y * 4.0f;
    precise float result = select(conditionProduct > 0.0f, leftProduct, rightProduct);
    outputBuffer[0] = result;
}

//EAGER: OpDecorate %[[CONDITION:conditionProduct[A-Za-z0-9_]*]] NoContraction
//EAGER: OpDecorate %[[LEFT:leftProduct[A-Za-z0-9_]*]] NoContraction
//EAGER: OpDecorate %[[RIGHT:rightProduct[A-Za-z0-9_]*]] NoContraction
//EAGER: %[[CONDITION]] = OpFMul %float
//EAGER: %[[LEFT]] = OpFMul %float
//EAGER: %[[RIGHT]] = OpFMul %float

// None of these predicates receives a demand merely for controlling a precise value.
[numthreads(1, 1, 1)]
void controlPredicates()
{
    float4 inputs = inputBuffer[0];
    float ifProduct = inputs.x * 2.0f;
    float loopProduct = inputs.y * 3.0f;
    float conditionalProduct = inputs.z * 4.0f;
    float selected = inputs.w;
    if (ifProduct > 0.0f)
        selected = inputs.x;
    for (uint index = 0; index < uint(loopProduct); ++index)
        selected = inputBuffer[index].w;
    float leftProduct = selected * 5.0f;
    float rightProduct = selected * 6.0f;
    precise float result = conditionalProduct > 0.0f ? leftProduct : rightProduct;
    outputBuffer[0] = result;
}

//CONTROL: OpDecorate %[[LEFT:leftProduct[A-Za-z0-9_]*]] NoContraction
//CONTROL: OpDecorate %[[RIGHT:rightProduct[A-Za-z0-9_]*]] NoContraction
//CONTROL: %ifProduct{{[A-Za-z0-9_]*}} = OpFMul %float
//CONTROL: OpFMul %float {{.*}} %float_3
//CONTROL: %conditionalProduct{{[A-Za-z0-9_]*}} = OpFMul %float
//CONTROL: %[[LEFT]] = OpFMul %float {{.*}} %float_5
//CONTROL: %[[RIGHT]] = OpFMul %float {{.*}} %float_6

// A legacy vector predicate lowers directly to selection rather than scalar control flow.
// Its predicate remains unrestricted, just as the scalar conditional's predicate does.
[numthreads(1, 1, 1)]
void vectorConditional()
{
    float2 predicateProduct = vectorInputBuffer[0] * vectorInputBuffer[1];
    float2 leftProduct = vectorInputBuffer[2] * vectorInputBuffer[3];
    float2 rightProduct = vectorInputBuffer[4] * vectorInputBuffer[5];
    precise float2 result = predicateProduct > 0.0f ? leftProduct : rightProduct;
    vectorOutputBuffer[0] = result;
}

//VECTOR_CONDITIONAL: OpDecorate %[[LEFT:leftProduct[A-Za-z0-9_]*]] NoContraction
//VECTOR_CONDITIONAL: OpDecorate %[[RIGHT:rightProduct[A-Za-z0-9_]*]] NoContraction
//VECTOR_CONDITIONAL: %predicateProduct{{[A-Za-z0-9_]*}} = OpFMul %v2float
//VECTOR_CONDITIONAL: %[[LEFT]] = OpFMul %v2float
//VECTOR_CONDITIONAL: %[[RIGHT]] = OpFMul %v2float
//VECTOR_CONDITIONAL: OpSelect %v2float {{.*}} %[[LEFT]] %[[RIGHT]]

// The instruction checks identify the required restrictions; counts exclude the predicates.
//TWO-COUNT-2: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//TWO-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//THREE-COUNT-3: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//THREE-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
