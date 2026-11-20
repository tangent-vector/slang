//TEST:COMPILE: $dirname/../second/tools/serializedImporter.slang -I $dirname/../second -o $dirname/tools/serializedImporter.slang-module
//TEST:COMPILE: $dirname/../second/tools/palette.slang -I $dirname/../second -o $dirname/tools/palette.slang-module
//TEST:SIMPLE(filecheck=CHECK): -I $dirname -target hlsl -O0

// The importer first resolves the sibling `palette.slang` in the second root.
// We then load both binaries through the package root. Resolving the archived
// dependency from the importer's directory would find `tools/tools/palette.slang`.
// Resolving `tools.palette` through the package root finds the same binary that
// the consumer imports directly.

import tools.serializedImporter;
import tools.palette;

[shader("fragment")]
int main() : SV_Target
{
    Palette palette = {31};
    return useSerializedPalette(palette).intensity;
}

// The generated code includes the definition from the palette binary.
// CHECK: struct [[PALETTE:Palette_[0-9]+]]
// CHECK: [[PALETTE]] [[BRIGHTEN:brightenPalette_[0-9]+]](
// CHECK-NEXT: {
// CHECK-NEXT: return {{.*}} + 2{{L?}});
// CHECK: [[PALETTE]] [[USE_PALETTE:useSerializedPalette_[0-9]+]](
// CHECK-NEXT: {
// CHECK-NEXT: return [[BRIGHTEN]](
// CHECK: int main(
// CHECK: return [[USE_PALETTE]](
