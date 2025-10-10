//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

// returning an unitialized value is an error

struct Test
{
    int getsInitialized;
    int doesNotGetInitialized;
}

Test use_uninitialized_as_return()
{
    Test returnVal;
    returnVal.getsInitialized = 1;
    // CHECK: error 41016: use of uninitialized variable 'returnVal'
    return returnVal;
}
