//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

// returning an unitialized value is an error

int use_uninitialized_as_return()
{
    int returnVal;
    // CHECK: error 41016: use of uninitialized variable 'returnVal'
    return returnVal;
}
