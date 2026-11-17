//TEST:SIMPLE(filecheck=CHECK): -target spirv-asm -entry main -stage compute

// Precision markers must preserve constant-expression arguments before specialization as well
// as fold away after specialization supplies constant values for those arguments.
int requireConstant(constexpr int value)
{
    return value;
}

int chooseConstant(constexpr bool condition)
{
    precise int chosen = select(condition, 4, 8);
    return requireConstant(chosen);
}

RWStructuredBuffer<float> outputValues;

[numthreads(1, 1, 1)]
void main()
{
    // An immutable precise initializer must remain eligible for constant evaluation too.
    precise let chosen = chooseConstant(true);
    outputValues[0] = float(chosen);
}

//CHECK: OpEntryPoint
//CHECK: OpConstant %float 4
