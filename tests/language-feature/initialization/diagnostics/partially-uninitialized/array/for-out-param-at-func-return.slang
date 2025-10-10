//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

// Returning from a function without fully initializing
// an `out` parameter is an error.

int uninitialized_out_param(out int[2] outParam)
{
    outParam[0] = 1;
    // CHECK: error 41018: returning without initializing out parameter 'outParam'
    return 0;
}
