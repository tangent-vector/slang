// hello-world.slang
StructuredBuffer<float> buffer0;
StructuredBuffer<float> buffer1;
RWStructuredBuffer<float> result;

float doAdd(float a, float b)
{
    float c = a + b;
    return c;
}

[shader("compute")]
[numthreads(1,1,1)]
void computeMain(uint3 threadId : SV_DispatchThreadID)
{
    uint index = threadId.x;
    result[index] = doAdd( buffer0[index], buffer1[index] );
}
