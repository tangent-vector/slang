//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

//
// Writing to a field of an uninitialized value of some
// struct type `T` is an error if the type `T` is not
// eligible for field-by-field initialization.
//
// If the type `T` has any user-defined constructors,
// that makes it ineligible for field-by-field initialization
// (because initialization must go through a constructor).
//

struct Test
{
    __init();

    int field;
}

void use_uninitialized_as_base_for_field_write( int param )
{
    Test base;
    // CHECK: error 41016: use of uninitialized variable 'base'
    base.field = param;
}
