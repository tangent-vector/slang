//TEST:SIMPLE:

//
// A local variable of `struct` type can be fully initialized
// by initializing each of its fields.
//
// This is only allowed for `struct` types that:
//
// * have no user-defined constructors
// * have no fields with default initial values
// * have no fields that are not visible in the context where
//   the variable is declared.
//

struct Test<T,U>
{
    T tField;
    U uField;
}

void test<T, U>(T tVal)
{
    Test<T,U> localVar;
    localVar.tField = tVal;
    func_with_out_param(localVar.uField);
    //
    func_with_in_param(localVar);
}

void func_with_in_param<V>(V param);
void func_with_out_param<W>(out W param);
