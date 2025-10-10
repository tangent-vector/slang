//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

//
// Using an uninitialized value as the initial value
// for a variable declaration is an error.
//

void use_uninitialized_as_initial_value()
{
    int[2] initVal;
    initVal[1] = 2;
    //
    // TODO(tfoley): we only catch a case like this
    // if the uninitialized value goes on to be used
    // in some downstream code. If `localVar`
    // get eliminated (e.g., because it is unused)
    // then no diagnostic gets emitted.
    //
    // CHECK: error 41016: use of uninitialized variable 'initVal'
    int[2] localVar = initVal;
    func_with_in_param(localVar);
}

void func_with_in_param(int[2] param);
