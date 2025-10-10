//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

// Returning from a construct without fully initializing
// one of the fields of `this` is an error.

struct Test
{
    int getsInitialized;
    int doesNotGetInitialized;
}

struct SomeType
{
    int fieldWithDefaultValue = 0;
    int fieldThatGetsInitializedInCtor;
    Test fieldThatGetsLeftPartiallyUninitialized;

    __init()
    {
        fieldThatGetsInitializedInCtor = 1;
        fieldThatGetsLeftPartiallyUninitialized.getsInitialized = 2;
        // CHECK: error 41020: exiting constructor without initializing field 'fieldThatGetsLeftUninitialized'
    }
}
