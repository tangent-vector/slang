//TEST:COMPILE: $dirname/second/tools/tone.slang -I $dirname/second -DPRECOMPILE_TONE -o $dirname/second/tools/tone.slang-module
//TEST:SIMPLE(filecheck=PAIRED): -I $dirname/second -target hlsl
//DIAGNOSTIC_TEST:SIMPLE(filecheck=CONFLICT,exit-code=2): -no-codegen -I $dirname/first -I $dirname/second -DCONFLICTING_ARTIFACTS

// With both roots, directory order selects the first root's source. Binary
// preference selects the second root's precompiled module, so the import
// must diagnose that those choices identify two different modules.
import tools.tone;

// CONFLICT: error[E38221]: module import has conflicting source and binary candidates
// CONFLICT-DAG: directory order selects {{.*first[/\\]+tools[/\\]+tone\.slang}}
// CONFLICT-DAG: artifact preference selects {{.*second[/\\]+tools[/\\]+tone\.slang-module}}

#ifndef CONFLICTING_ARTIFACTS
// With only the second root, its source and binary represent one module.
// Binary preference selects the value supplied when `PRECOMPILE_TONE` was defined.
[shader("fragment")]
int main() : SV_Target
{
    // PAIRED: return 23{{L?}};
    return tone;
}
#endif
