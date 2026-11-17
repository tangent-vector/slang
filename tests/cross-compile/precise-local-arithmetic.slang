// Text targets receive restrictions on each contributing operation, including ordinary
// intermediates. The final addition reads a precise value without becoming precise.
//TEST:SIMPLE(filecheck=CHECK): -target hlsl -entry main -stage compute -O0
//TEST:SIMPLE(filecheck=CHECK): -target glsl -entry main -stage compute -O0

StructuredBuffer<float4> inputBuffer;
RWStructuredBuffer<float> outputBuffer;

[numthreads(1, 1, 1)]
void main()
{
    float4 inputs = inputBuffer[0];
    float product = inputs.x * inputs.y;
    precise float result = product + inputs.z;
    outputBuffer[0] = result + inputs.w;
}

//CHECK: precise float [[PRODUCT:[A-Za-z0-9_]+]] = {{.*}} * {{.*}};
//CHECK: precise float [[RESULT:[A-Za-z0-9_]+]] = [[PRODUCT]] + {{.*}};
//CHECK-NOT: precise
//CHECK: = [[RESULT]] + {{.*}};
//CHECK-NOT: precise
