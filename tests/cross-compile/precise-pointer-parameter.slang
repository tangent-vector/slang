// A precise pointer input restricts assignments of the pointer value, rather than writes through
// that pointer. Global shader parameters represent pointer values too.
//TEST:SIMPLE(filecheck=POINTEE): -target cpp -entry ordinaryPointee -stage compute -O0 -dump-ir-after propagatePreciseDemands
//TEST:SIMPLE(filecheck=POINTER): -target cpp -entry pointerAssignment -stage compute -O0 -dump-ir-after propagatePreciseDemands
//TEST:SIMPLE(filecheck=POINTEE): -target cpp -entry globalPointer -stage compute -O0 -dump-ir-after propagatePreciseDemands

StructuredBuffer<float4> inputs;
RWStructuredBuffer<float> outputs;
precise float* globalDestination;

[noinline]
void writeThrough(precise float* pointer, float a, float b)
{
    float product = a * b;
    *pointer = product;
}

[noinline]
void reassign(precise float* pointer, float a, float b)
{
    float indexProduct = a * b;
    pointer = pointer + uint(indexProduct);
    *pointer = 0.0f;
}

[numthreads(1, 1, 1)]
void ordinaryPointee()
{
    float value = 0.0f;
    float4 data = inputs[0];
    writeThrough(&value, data.x, data.y);
    outputs[0] = value;
}

[numthreads(1, 1, 1)]
void pointerAssignment()
{
    float4 data = inputs[0];
    reassign(&outputs[0], data.x, data.y);
}

[numthreads(1, 1, 1)]
void globalPointer()
{
    float4 data = inputs[0];
    float product = data.x * data.y;
    *globalDestination = product;
}

//POINTEE: ### AFTER propagatePreciseDemands:
//POINTEE: %[[PRODUCT:product[A-Za-z0-9_]*]] {{.*}} = mul(%{{[A-Za-z0-9_]+}}, %{{[A-Za-z0-9_]+}})

//POINTER: ### AFTER propagatePreciseDemands:
//POINTER: %[[POLICY:[A-Za-z0-9_]+]] {{.*}} = Precise
//POINTER: %[[INDEX:indexProduct[A-Za-z0-9_]*]] {{.*}} = mul({{.*}}, %[[POLICY]])
