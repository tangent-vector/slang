// Precision identities preserve the concrete runtime contents of an interface value through
// type-flow specialization. Declared payload writes retain demands; unevaluated queries add none.
//TEST:SIMPLE(filecheck=PAYLOAD): -target spirv-asm -entry observePayload -stage compute -O0
//TEST:SIMPLE(filecheck=METADATA): -target spirv-asm -entry metadataOnly -stage compute -O0

interface IValue
{
    float read();
}

struct Box : IValue
{
    float value;
    float read() { return value; }
}

StructuredBuffer<float2> inputs;
RWStructuredBuffer<float> outputs;

[numthreads(1, 1, 1)]
void observePayload()
{
    float2 data = inputs[0];
    float product = data.x * data.y;
    Box box = {product};
    precise IValue value = box;
    outputs[0] = value.read();
}

// Demanding an unevaluated size query does not demand the payload. The separate output keeps
// the product observable without an independent declared precision policy on the interface value.
[numthreads(1, 1, 1)]
void metadataOnly()
{
    float2 data = inputs[0];
    float product = data.x * data.y;
    Box box = {product};
    IValue value = box;
    precise bool matches = sizeof(value) == sizeof(Box);
    outputs[0] = product;
    outputs[1] = matches ? 1.0f : 0.0f;
}

//PAYLOAD-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//PAYLOAD: OpDecorate %[[PRODUCT:product[A-Za-z0-9_]*]] NoContraction
//PAYLOAD-NOT: OpDecorate %{{[A-Za-z0-9_]+}} NoContraction
//PAYLOAD: %[[PRODUCT]] = OpFMul %float
//METADATA-NOT: NoContraction
//METADATA: OpFMul %float
//METADATA-NOT: NoContraction
