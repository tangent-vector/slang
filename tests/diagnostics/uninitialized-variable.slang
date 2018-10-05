// uninitialized-variable.slang

//TEST:SIMPLE:

// Confirm that we generate warnings for potential use of uninitialized variables.

int test0(int a)
{
    // Clear-cut case: variabe is definitely used when uninitialized.
    int b;
    return a + b;
}
