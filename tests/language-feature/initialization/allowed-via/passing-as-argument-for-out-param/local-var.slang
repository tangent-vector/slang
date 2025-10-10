//TEST:SIMPLE:

//
// A local variable passed to a function call as an
// argument for an `out` parameter is fully initialized
// upon return from that call.
//

void test<T>(T srcVal)
{
    T localVar;
    func_with_out_param(localVar);
    func_with_in_param(localVar);
}

void func_with_in_param<U>(U param);
void func_with_out_param<V>(out V param);
