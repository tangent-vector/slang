//TEST:SIMPLE(filecheck=CHECK): -I $dirname/.. -target hlsl

// The qualified name inferred for this input is `tools.main`.
// Its implementing file still refers to the primary file by the simple name `main`.
__include impl;

[shader("fragment")]
int fragmentMain() : SV_Target
{
    // CHECK: return 41{{L?}};
    return implementationValue;
}
