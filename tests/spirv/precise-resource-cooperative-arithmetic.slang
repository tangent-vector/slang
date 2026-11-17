// These intrinsic declarations isolate native cooperative arithmetic from ordinary numeric
// wrapper calls. A precise native operation needs the same contract for every element type shape.
//TEST:SIMPLE(filecheck=CHECK): -target spirv-asm -entry vectorArithmetic -stage compute -capability cooperative_vector -O0
//TEST:SIMPLE(filecheck=CHECK): -target spirv-asm -entry matrixArithmetic -stage compute -capability cooperative_matrix -O0

using namespace linalg;

StructuredBuffer<float4> inputs;
RWStructuredBuffer<float> outputs;

__intrinsic_op(mul)
CoopVec<float, 4> nativeMultiply(CoopVec<float, 4> left, CoopVec<float, 4> right);

[numthreads(1, 1, 1)]
void vectorArithmetic()
{
    float4 data = inputs[0];
    CoopVec<float, 4> left = CoopVec<float, 4>(data.x);
    CoopVec<float, 4> right = CoopVec<float, 4>(data.y);
    precise CoopVec<float, 4> product = nativeMultiply(left, right);
    outputs[0] = product[0];
}

typealias Matrix = CoopMat<float, MemoryScope.Subgroup, 16, 16, CoopMatMatrixUse.MatrixAccumulator>;

__intrinsic_op(mul)
Matrix nativeMultiply(Matrix left, Matrix right);

[numthreads(1, 1, 1)]
void matrixArithmetic()
{
    float4 data = inputs[0];
    Matrix left = Matrix(data.x);
    Matrix right = Matrix(data.y);
    precise Matrix product = nativeMultiply(left, right);
    outputs[0] = product[0];
}

//CHECK: OpDecorate %[[PRODUCT:product[A-Za-z0-9_]*]] NoContraction
//CHECK-NOT: NoContraction
//CHECK: %[[PRODUCT]] = OpFMul
