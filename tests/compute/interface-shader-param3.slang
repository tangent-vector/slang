// interface-shader-param3.slang

// This test builds on `interface-shader-param2.slang` by putting
// interface types at more complicated places in the overall layout.
//

// NOTE TO SELF:
//
// First issue is that the constant buffer layouts aren't being
// computed correctly for `gStrategy` (even in the previous test),
// so that it doesn't get a `b` register bound.
//
// Second issue is that the type legalization logic is now overzealous,
// and moves the `modifier` member of the entry-point constant buffer
// out to global scope, when it should allocate it inside the constant
// buffer.
//



//TEST(compute):COMPARE_COMPUTE_EX:-slang -compute
//TEST(compute):COMPARE_COMPUTE_EX:-slang -compute -dx12
//TEST(compute, vulkan):COMPARE_COMPUTE_EX:-vk -compute

// A lot of the setup is the same as for `interface-shader-param`,
// so look there if you want the comments.

interface IRandomNumberGenerator
{
    [mutating]
    int randomInt();
}

interface IRandomNumberGenerationStrategy
{
    associatedtype Generator : IRandomNumberGenerator;
    Generator makeGenerator(int seed);
}

interface IModifier
{
    int modify(int val);
}

int test(
    int                             seed,
    IRandomNumberGenerationStrategy inStrategy,
    IModifier                       modifier)
{
    let strategy = inStrategy;
    var generator = strategy.makeGenerator(seed);
    let unused = generator.randomInt();
    let val = generator.randomInt();
    let modifiedVal = modifier.modify(val);
    return modifiedVal;
}


//TEST_INPUT:ubuffer(data=[0 0 0 0], stride=4):dxbinding(0),glbinding(0),out
RWStructuredBuffer<int> gOutputBuffer;

//TEST_INPUT:cbuffer(data=[9 0 0 0 1 0 0 0], stride=4):dxbinding(0),glbinding(1)
cbuffer C
{
    IRandomNumberGenerationStrategy gStrategy;
    int                             gExtra;
}

[numthreads(4, 1, 1)]
void computeMain(
//TEST_INPUT:cbuffer(data=[16384 0 0 0 8 0 0 0], stride=4):dxbinding(1),glbinding(2)
    uniform IModifier   modifier,
    uniform int         extra,
            uint3       dispatchThreadID : SV_DispatchThreadID)
{
    let tid = dispatchThreadID.x;

    let inputVal : int = tid;
    let outputVal = test(inputVal, gStrategy, modifier)
    	+ extra*gExtra;

    gOutputBuffer[tid] = outputVal;
}

// Okay, now we get to the part that is unique starting
// in this test: we add data to the concrete types
// that we will use as parameters.

struct MyStrategy : IRandomNumberGenerationStrategy
{
    int globalSeed;

    struct Generator : IRandomNumberGenerator
    {
        int state;

        [mutating]
        int randomInt()
        {
            return state++;
        }
    }

    Generator makeGenerator(int seed)
    {
        Generator generator = { seed ^ globalSeed };
        return generator;
    }
}

struct MyModifier : IModifier
{
    int localModifier;

    int modify(int val)
    {
        return val * localModifier;
    }
}

//TEST_INPUT: globalExistentialType MyStrategy
//TEST_INPUT: entryPointExistentialType MyModifier
