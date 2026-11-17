//TEST:INTERPRET(filecheck=CHECK):

// The interpreter can encounter precision markers before target-specific precision resolution.
// Explicit writes, legacy precise-containing aggregates, and short-circuit dependencies all
// return operand zero's runtime value without allocating or reading a separate dependency.
struct Value
{
    precise float preciseField;
    float ordinaryField;
}

[noinline]
precise float multiply(float a, float b)
{
    precise float result = a * b;
    return result;
}

[noinline]
Value makeValue(float a, float b)
{
    return {a, b};
}

[noinline]
bool bothPositive(float a, float b)
{
    return a > 0.0f && b > 0.0f;
}

void main()
{
    //CHECK: 12.000000
    printf("%f\n", multiply(3.0f, 4.0f));
    //CHECK: 7.000000
    printf("%f\n", makeValue(2.0f, 7.0f).ordinaryField);
    //CHECK: 1
    printf("%d\n", int(bothPositive(2.0f, 3.0f)));
    //CHECK: 0
    printf("%d\n", int(bothPositive(-2.0f, 3.0f)));
}
