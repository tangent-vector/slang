// precise-keyword.slang.hlsl
//TEST_IGNORE_FILE:

// We restrict the arithmetic results directly. Declaring `z_0` precise would also tag
// FXC's final output move, which is not floating-point arithmetic.
float4 main(float2 v_0 : V) : SV_TARGET
{
    float _S1 = v_0.x;
    float z_0;

    if (_S1 > 0.00000000000000000000)
    {
        precise float product = _S1 * v_0.y;
        precise float sum = product + _S1;
        z_0 = sum;
    }
    else
    {
        float _S2 = v_0.y;
        precise float product = _S2 * _S1;
        precise float sum = product + _S2;
        z_0 = sum;
    }

    return (float4) z_0;
}
