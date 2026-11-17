// The compiler propagates precision demands backward through contributing values, even
// when simplification removes their local variables. Reading a precise value does not
// impose the same demand on an ordinary later consumer.
//TEST:SIMPLE(filecheck=BACKWARD): -target spirv-asm -entry backward -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry backward -stage compute -O0
//TEST:SIMPLE(filecheck=FORWARD): -target spirv-asm -entry forward -stage compute -O0
//TEST:SIMPLE(filecheck=ONE): -target spirv-asm -entry forward -stage compute -O0
//TEST:SIMPLE(filecheck=CSE): -target spirv-asm -entry identicalExpressions -stage compute -O3
//TEST:SIMPLE(filecheck=ONE): -target spirv-asm -entry identicalExpressions -stage compute -O3
//TEST:SIMPLE(filecheck=IDENTITY): -target spirv-asm -entry simplifiedIdentity -stage compute -O0
//TEST:SIMPLE(filecheck=VECTOR): -target spirv-asm -entry vectorArithmetic -stage compute -O0
//TEST:SIMPLE(filecheck=TWO): -target spirv-asm -entry vectorArithmetic -stage compute -O0
//TEST:SIMPLE(filecheck=MATRIX): -target spirv-asm -entry matrixArithmetic -stage compute -O0
//TEST:SIMPLE(filecheck=FOUR): -target spirv-asm -entry matrixArithmetic -stage compute -O0

StructuredBuffer<float4> inputBuffer;
RWStructuredBuffer<float> outputBuffer;
RWStructuredBuffer<float4> vectorOutputBuffer;

// Both arithmetic operations contribute to `result`, even though `product` is ordinary.
[numthreads(1, 1, 1)]
void backward()
{
    float4 inputs = inputBuffer[0];
    float product = inputs.x * inputs.y;
    precise float result = product + inputs.z;
    outputBuffer[0] = result + inputs.w;
}

//BACKWARD: OpDecorate %[[PRODUCT:product[A-Za-z0-9_]*]] NoContraction
//BACKWARD: OpDecorate %[[RESULT:[A-Za-z0-9_]+]] NoContraction
//BACKWARD: %[[PRODUCT]] = OpFMul %float
//BACKWARD: %[[RESULT]] = OpFAdd %float %[[PRODUCT]]
//BACKWARD: OpFAdd %float %[[RESULT]]

// Reading `product` does not constrain the subsequent addition.
[numthreads(1, 1, 1)]
void forward()
{
    float4 inputs = inputBuffer[0];
    precise float product = inputs.x * inputs.y;
    outputBuffer[0] = product + inputs.z;
}

//FORWARD: OpDecorate %[[PRODUCT:[A-Za-z0-9_]+]] NoContraction
//FORWARD: %[[PRODUCT]] = OpFMul %float
//FORWARD: OpFAdd %float %[[PRODUCT]]

// CSE must distinguish the resolved policies of otherwise equivalent products.
[numthreads(1, 1, 1)]
void identicalExpressions()
{
    float4 inputs = inputBuffer[0];
    float ordinaryProduct = inputs.x * inputs.y;
    precise float preciseProduct = inputs.x * inputs.y;
    outputBuffer[0] = ordinaryProduct;
    outputBuffer[1] = preciseProduct;
}

//CSE: OpDecorate %[[PRECISE:[A-Za-z0-9_]+]] NoContraction
//CSE: %[[ORDINARY:ordinaryProduct[A-Za-z0-9_]*]] = OpFMul %float %[[LEFT:[A-Za-z0-9_]+]] %[[RIGHT:[A-Za-z0-9_]+]]
//CSE: %[[PRECISE]] = OpFMul %float %[[LEFT]] %[[RIGHT]]

// Eliminating an identity expression must preserve the demand on its surviving input.
[numthreads(1, 1, 1)]
void simplifiedIdentity()
{
    float4 inputs = inputBuffer[0];
    float product = inputs.x * inputs.y;
    precise float result = product + 0.0f;
    outputBuffer[0] = result;
}

//IDENTITY: OpDecorate %[[PRODUCT:product[A-Za-z0-9_]*]] NoContraction
//IDENTITY: %[[PRODUCT]] = OpFMul %float

// Vector operations obey the same backward propagation rule as scalar operations.
[numthreads(1, 1, 1)]
void vectorArithmetic()
{
    float4 inputs = inputBuffer[0];
    float4 product = inputs * inputBuffer[1];
    precise float4 result = product + inputBuffer[2];
    vectorOutputBuffer[0] = result;
}

//VECTOR: OpDecorate %[[PRODUCT:product[A-Za-z0-9_]*]] NoContraction
//VECTOR: OpDecorate %[[SUM:[A-Za-z0-9_]+]] NoContraction
//VECTOR: %[[PRODUCT]] = OpFMul %v4float
//VECTOR: %[[SUM]] = OpFAdd %v4float %[[PRODUCT]]

// Matrix legalization expands each componentwise operation into row operations.
// Each emitted row must retain the operation's resolved precision restriction.
[numthreads(1, 1, 1)]
void matrixArithmetic()
{
    float2x2 inputs = float2x2(inputBuffer[0].xy, inputBuffer[1].xy);
    float2x2 product = inputs * inputs;
    precise float2x2 result = product + inputs;
    vectorOutputBuffer[0] = float4(result[0], result[1]);
}

//MATRIX: OpDecorate %[[MUL0:[A-Za-z0-9_]+]] NoContraction
//MATRIX: OpDecorate %[[MUL1:[A-Za-z0-9_]+]] NoContraction
//MATRIX: OpDecorate %[[ADD0:[A-Za-z0-9_]+]] NoContraction
//MATRIX: OpDecorate %[[ADD1:[A-Za-z0-9_]+]] NoContraction
//MATRIX: %[[MUL0]] = OpFMul %v2float
//MATRIX: %[[MUL1]] = OpFMul %v2float
//MATRIX: %[[ADD0]] = OpFAdd %v2float
//MATRIX: %[[ADD1]] = OpFAdd %v2float

// Exact counts complement the instruction checks by rejecting unrelated restrictions.
//ONE-COUNT-1: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//ONE-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//TWO-COUNT-2: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//TWO-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//FOUR-COUNT-4: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//FOUR-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
