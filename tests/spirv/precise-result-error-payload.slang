//TEST:SIMPLE(filecheck=CHECK): -target spirv-asm -entry main -stage compute -fp-mode fast

// A declared precise result constrains successful returns. A thrown error has a different
// declared type and policy, even though error-handling lowering returns both through Result<T,E>.
StructuredBuffer<float> inputValues;
RWStructuredBuffer<float> outputValues;

[noinline]
precise float calculate(float a, float b, float c, float x, float y, float z, bool fail) throws float
{
    if (fail)
        throw x / y + z;
    return a * b + c;
}

[numthreads(1, 1, 1)]
void main(uint3 tid : SV_DispatchThreadID)
{
    do
    {
        outputValues[0] = try calculate(
            inputValues[0],
            inputValues[1],
            inputValues[2],
            inputValues[3],
            inputValues[4],
            inputValues[5],
            tid.x != 0);
    }
    catch (error : float)
    {
        outputValues[0] = error;
    }
}

// Exactly the multiply and add in the successful return are constrained. The error payload's
// division and addition remain unrestricted, and the decorated addition uses the decorated product.
//CHECK: OpDecorate %[[PRODUCT:[A-Za-z0-9_]+]] NoContraction
//CHECK: OpDecorate %[[SUM:[A-Za-z0-9_]+]] NoContraction
//CHECK-NOT: OpDecorate %{{.*}} NoContraction
//CHECK: %[[PRODUCT]] = OpFMul %float
//CHECK: %[[SUM]] = OpFAdd %float %[[PRODUCT]]
