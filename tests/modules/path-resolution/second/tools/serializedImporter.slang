// The importer's signature and body both refer to the sibling `palette.slang`.
import palette;

public Palette useSerializedPalette(Palette palette)
{
    return brightenPalette(palette);
}
