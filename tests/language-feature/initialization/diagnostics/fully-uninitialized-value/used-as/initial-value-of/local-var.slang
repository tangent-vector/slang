//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

//
// Using an uninitialized value as the initial value
// for a variable declaration is an error.
//

void use_uninitialized_as_initial_value()
{
    int initVal;
    // CHECK: error 41016: use of uninitialized variable 'initVal'
    int localVar = initVal;
}
