//TEST:SIMPLE(filecheck=CHECK): -target spirv-asm -entry main -stage compute -fp-mode fast

// An eager select demands every evaluated input even when constant folding can choose a result.
// The unselected input's multiplication also feeds the output, so it survives and must be precise.
StructuredBuffer<float> inputValues;
RWStructuredBuffer<float> outputValues;

[numthreads(1, 1, 1)]
void main()
{
    float product = inputValues[0] * inputValues[1];
    precise int selected = select(true, 0, int(product));
    outputValues[0] = product + float(selected);
}

//CHECK: OpDecorate %[[PRODUCT:[A-Za-z0-9_]+]] NoContraction
//CHECK-NOT: OpDecorate %{{.*}} NoContraction
//CHECK: %[[PRODUCT]] = OpFMul %float
