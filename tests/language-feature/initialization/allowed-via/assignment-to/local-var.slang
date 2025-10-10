//TEST:SIMPLE:

//
// Assigning to a local variable makes it fully initialized.
//
// This test case is only here for completeness; if
// this case didn't work then a whole lot of other
// tests would break too.
//

void test<T>(T srcVal)
{
    T localVar;
    localVar = srcVal;
    func_with_in_param(localVar);
}

void func_with_in_param<U>(U param);
