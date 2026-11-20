// This import finds the sibling `palette.slang`, whose qualified name is `tools.palette`.
import palette;

#ifdef SECOND_ROOT_ONLY
public Palette useSecondPalette(Palette palette)
{
    return palette;
}
#endif
