//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

// Returning from a function without fully initializing
// an `out` parameter is an error.

int uninitialized_out_param(out int outParam)
{
    // CHECK: error 41018: returning without initializing out parameter 'outParam'
    return 0;
}
