//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

//
// Calling a method where the base (`this`) expression
// is an uninitialized value is an error.
//

struct Test
{
    [mutating] void method();
}

void use_uninitialized_as_base_for_method_call()
{
    Test base;
    // CHECK: error 41016: use of uninitialized variable 'base'
    base.method();
}
