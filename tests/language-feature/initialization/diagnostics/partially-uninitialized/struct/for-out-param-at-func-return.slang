//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

// Returning from a function without fully initializing
// an `out` parameter is an error.

struct Test
{
    int getsInitialized;
    int doesNotGetInitialized;
}

int uninitialized_out_param(out Test outParam)
{
    outParam.getsInitialized = 1;
    // CHECK: error 41018: returning without initializing out parameter 'outParam'
    return 0;
}
