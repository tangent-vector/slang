//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

//
// Reading a field from an uninitialized value is an error.
//

struct Test
{
    int field;
}

int use_uninitialized_as_base_for_field_read()
{
    Test base;
    // CHECK: error 41016: use of uninitialized variable 'base'
    return base.field;
}
