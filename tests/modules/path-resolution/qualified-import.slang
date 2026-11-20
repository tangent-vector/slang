//TEST:SIMPLE: -no-codegen -I $dirname/second -DSECOND_ROOT_ONLY
//DIAGNOSTIC_TEST:SIMPLE(filecheck=CONFLICT,exit-code=2): -no-codegen -I $dirname/first -I $dirname/second
//DIAGNOSTIC_TEST:SIMPLE(filecheck=CONFLICT,exit-code=2): -no-codegen -I $dirname/first -I $dirname/second -DSECOND_IMPORTED_FIRST

// The importers each spell their request as `import palette`. The order of those
// requests cannot make the second root's palette the module named `tools.palette`
// when root-only lookup selects the first root's palette.

#if defined(SECOND_ROOT_ONLY)
import tools.secondImporter;
import tools.palette;

// With one root, both import spellings must refer to the same nominal type.
Palette roundTripPalette(Palette palette)
{
    return useSecondPalette(palette);
}
#elif defined(SECOND_IMPORTED_FIRST)
import tools.secondImporter;
import tools.firstImporter;
#else
import tools.firstImporter;
import tools.secondImporter;
#endif

// CONFLICT: error[E38220]: module import conflicts with root lookup
// CONFLICT-DAG: tools.palette
// CONFLICT-DAG: resolves to {{.*second[/\\]+tools[/\\]+palette\.slang}}
// CONFLICT-DAG: root-only lookup selects {{.*first[/\\]+tools[/\\]+palette\.slang}}
