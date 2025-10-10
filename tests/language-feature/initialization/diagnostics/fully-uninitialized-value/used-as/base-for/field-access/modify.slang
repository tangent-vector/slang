//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

//
// Modifying a field from an uninitialized value is an error.
//

struct Test
{
    int field;
}

void use_uninitialized_as_base_for_field_modify()
{
    Test base;
    // CHECK: error 41016: use of uninitialized variable 'base'
    base.field++;
}
