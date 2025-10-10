//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

//
// Calling a function with an uninitialized value as an
// argument for an `inout` parameter is an error.
//

struct Test
{
    int getsInitialized;
    int doesNotGetInitialized;
}

void use_uninitialized_as_argument_for_inout_param()
{
    Test argForInOutParam;
    argForInOutParam.getsInitialized = 1;
    //
    // TODO(tfoley): this case is being ignored rather
    // than diagnosed; we are allowing uninitialized
    // variables to be passed as arguments for `inout`
    // parameters.
    //
    // CHECK: error 41016: use of uninitialized variable 'argForInOutParam'
    func_with_inout_param(argForInOutParam);
}

void func_with_inout_param(inout Test param);
