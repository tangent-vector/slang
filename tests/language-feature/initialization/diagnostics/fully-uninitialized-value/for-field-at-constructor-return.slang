//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

// Returning from a construct without fully initializing
// one of the fields of `this` is an error.

struct SomeType
{
    int fieldWithDefaultValue = 0;
    int fieldThatGetsInitializedInCtor;
    int fieldThatGetsLeftUninitialized;

    __init()
    {
        fieldThatGetsInitializedInCtor = 1;
        // CHECK: error 41020: exiting constructor without initializing field 'fieldThatGetsLeftUninitialized'
    }
}
