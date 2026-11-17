// A conditional pointer may refer to explicitly precise storage. The compiler must
// constrain a stored value if either possible destination was declared precise.
// The IR check isolates Slang's policy inference from C++'s unsupported-target warning.
//TEST:SIMPLE(filecheck=IR): -target cpp -entry computeMain -stage compute -O0 -dump-ir-after propagatePreciseDemands
//TEST:SIMPLE(filecheck=PLAIN): -target cpp -entry ordinaryPointer -stage compute -O0 -dump-ir-after propagatePreciseDemands

StructuredBuffer<float4> inputBuffer;
RWStructuredBuffer<float> outputBuffer;

[numthreads(1, 1, 1)]
void computeMain()
{
    float4 inputs = inputBuffer[0];
    precise float preciseValue = 0.0f;
    float ordinaryValue = 0.0f;
    float* destination = inputs.w > 0.0f ? &preciseValue : &ordinaryValue;
    float product = inputs.x * inputs.y;
    float sum = product + inputs.z;
    *destination = sum;
    outputBuffer[0] = preciseValue;
    outputBuffer[1] = ordinaryValue;
}

//IR: ### AFTER propagatePreciseDemands:
//IR: %[[POLICY:[A-Za-z0-9_]+]] {{.*}} = Precise
//IR: %[[PRODUCT:product[A-Za-z0-9_]*]] {{.*}} = mul({{.*}}, %[[POLICY]])
//IR: %[[SUM:sum[A-Za-z0-9_]*]] {{.*}} = add(%[[PRODUCT]], {{.*}}, %[[POLICY]])
//IR: store({{.*}}, %[[SUM]])

// A precise pointer restricts computation of its address value, without declaring its
// pointee precise. After lowering wraps the pointer initializer in `MakePrecise`, stores
// through `pointerToOrdinary` must still follow the ordinary local's declaration policy.
[numthreads(1, 1, 1)]
void ordinaryPointer()
{
    float4 inputs = inputBuffer[0];
    precise float preciseValue = 0.0f;
    float ordinaryValue = 0.0f;
    float* precisePointer = &preciseValue;
    precise float* pointerToOrdinary = &ordinaryValue;
    float preciseProduct = inputs.x * inputs.y;
    float ordinaryProduct = inputs.z * inputs.w;
    *precisePointer = preciseProduct;
    *pointerToOrdinary = ordinaryProduct;
    outputBuffer[0] = preciseValue;
    outputBuffer[1] = ordinaryValue;
}

//PLAIN: ### AFTER propagatePreciseDemands:
//PLAIN: %[[POLICY:[A-Za-z0-9_]+]] {{.*}} = Precise
//PLAIN: %[[PRECISE:preciseProduct[A-Za-z0-9_]*]] {{.*}} = mul({{.*}}, %[[POLICY]])
//PLAIN: %[[ORDINARY:ordinaryProduct[A-Za-z0-9_]*]] {{.*}} = mul(%{{[A-Za-z0-9_]+}}, %{{[A-Za-z0-9_]+}})
