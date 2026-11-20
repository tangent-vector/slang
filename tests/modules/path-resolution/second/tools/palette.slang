// This file supplies `tools.palette` when we search only the second root.
public struct Palette
{
    public int intensity;
}

public Palette brightenPalette(Palette palette)
{
    return {palette.intensity + 2};
}
