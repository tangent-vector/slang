// uninitialized-variable.slang

//TEST:SIMPLE:

// Confirm that we generate warnings for potential use of uninitialized variables.

int test0(int a)
{
    // Clear-cut case: variabe is definitely used when uninitialized.
    int b;
    return a + b;
}

int test1(int a)
{
    // Negative test, don't give err for variable that is undefined
    // on entry to a block if it gets initialized in the block

    int x;
    while(a)
    {
        x = a + 1;
        a = a - 1;
    }
    return x;
}