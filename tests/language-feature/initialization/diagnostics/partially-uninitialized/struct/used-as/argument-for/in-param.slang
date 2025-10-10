//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

//
// Calling a function with an uninitialized value as an
// argument for an ordinary (`in`) parameter is an error.
//

struct Test
{
    int getsInitialized;
    int doesNotGetInitialized;
}

void use_uninitialized_as_argument()
{
    Test argForInParam;
    argForInParam.getsInitialized = 1;
    // CHECK: error 41016: use of uninitialized variable 'argForInParam'
    func_with_in_param(argForInParam);
}

void func_with_in_param(Test param);
