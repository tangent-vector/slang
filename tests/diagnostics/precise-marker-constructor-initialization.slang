//DIAGNOSTIC_TEST:SIMPLE(diag=CHECK,exit-code=0): -no-codegen

// Aggregate return preservation adds an identity marker even without an explicit qualifier,
// because a specialized aggregate can contain precise fields. Constructor analysis must follow
// that identity: the incomplete constructor still gets its field diagnostic, and the complete
// constructor must not get a general uninitialized-value warning from its return marker.
struct Incomplete
{
    float value;

    __init() {}
    //CHECK: warning E41020
    //CHECK: exiting constructor without initializing field 'value'
}

struct Complete
{
    float value;

    __init(float input)
    {
        value = input;
    }
}

RWStructuredBuffer<float> outputValues;

[numthreads(1, 1, 1)]
void main()
{
    Incomplete incomplete = Incomplete();
    Complete complete = Complete(1.0);
    outputValues[0] = incomplete.value + complete.value;
}
