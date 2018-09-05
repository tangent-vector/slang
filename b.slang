
import aa;

float4 main(float4 uv : UV) : SV_Target
{
	return uv + helper(uv.x, uv.y);
}