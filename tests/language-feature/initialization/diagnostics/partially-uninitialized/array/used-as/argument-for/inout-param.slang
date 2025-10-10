//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

//
// Calling a function with an uninitialized value as an
// argument for an `inout` parameter is an error.
//

void use_uninitialized_as_argument_for_inout_param()
{
    int[2] argForInOutParam;
    argForInOutParam[1] = 0;
    //
    // TODO(tfoley): this case is being ignored rather
    // than diagnosed; we are allowing uninitialized
    // variables to be passed as arguments for `inout`
    // parameters.
    //
    // CHECK: error 41016: use of uninitialized variable 'argForInOutParam'
    func_with_inout_param(argForInOutParam);
}

void func_with_inout_param(inout int[2] param);
