//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

// returning an unitialized value is an error

int[2] use_uninitialized_as_return()
{
    int[2] returnVal;
    returnVal[0] = 1;
    // CHECK: error 41016: use of uninitialized variable 'returnVal'
    return returnVal;
}
