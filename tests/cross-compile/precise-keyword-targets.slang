// precise-keyword-targets.slang

// `precise` is only valid HLSL/GLSL syntax; on the other C-like targets the
// qualifier is dropped (with a warning) rather than emitted as an invalid token.
// Native LLVM output also diagnoses local restrictions that the emitter cannot encode.

//TEST:SIMPLE(filecheck=CUDA):  -target cuda  -stage compute -entry computeMain
//TEST:SIMPLE(filecheck=CPP):   -target cpp   -stage compute -entry computeMain
//TEST:SIMPLE(filecheck=METAL): -target metal -stage compute -entry computeMain
//TEST:SIMPLE(filecheck=WGSL):  -target wgsl  -stage compute -entry computeMain
//TEST:SIMPLE(filecheck=LLVM):  -target llvm-shader-ir -stage compute -entry computeMain
//TEST:SIMPLE(filecheck=LLVM_NEG): -target llvm-shader-ir -stage compute -entry negateMain
//TEST:SIMPLE(filecheck=HLSL):  -target hlsl  -stage compute -entry computeMain
//TEST:SIMPLE(filecheck=GLSL):  -target glsl  -stage compute -entry computeMain
//DIAGNOSTIC_TEST:SIMPLE(diag=DIAG,exit-code=0): -target cuda  -stage compute -entry computeMain
//DIAGNOSTIC_TEST:SIMPLE(diag=DIAG,exit-code=0): -target cpp   -stage compute -entry computeMain
//DIAGNOSTIC_TEST:SIMPLE(diag=DIAG,exit-code=0): -target metal -stage compute -entry computeMain
//DIAGNOSTIC_TEST:SIMPLE(diag=DIAG,exit-code=0): -target wgsl  -stage compute -entry computeMain
//DIAGNOSTIC_TEST:SIMPLE(diag=DIAG,exit-code=0): -target llvm-shader-ir -stage compute -entry computeMain
//DIAGNOSTIC_TEST:SIMPLE(diag=DIAG,exit-code=0): -target llvm-shader-ir -stage compute -entry negateMain

RWStructuredBuffer<float> gOut;

[shader("compute")]
[numthreads(1, 1, 1)]
void computeMain()
{
    precise float r = gOut[0] * 2.0f;
    gOut[0] = r;
}

// This entry point exercises unary arithmetic with a local precision demand.
[shader("compute")]
[numthreads(1, 1, 1)]
void negateMain()
{
    precise float negatedValue = -gOut[0];
    gOut[0] = negatedValue;
}

// Targets that do not accept the qualifier: it must be gone.
// CUDA:      2.0f
// CUDA-NOT:  precise float
// CPP:       2.0f
// CPP-NOT:   precise float
// METAL:     2.0f
// METAL-NOT: precise float
// WGSL:      2.0f
// WGSL-NOT:  precise var

// The native LLVM path still emits its arithmetic after diagnosing the restriction.
// LLVM:      fmul float
// LLVM_NEG:  fneg float

// Targets that do accept it: the qualifier stays.
// HLSL:      precise float
// GLSL:      precise float

//DIAG: warning E56005
