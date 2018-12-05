IR Serialization
================

A Naive Starting Point
----------------------

The primary task of the IR serialization format is to allow for reconstruction of the hierarchy of `IRInst`s that make up a module.
The in-memory IR is relatively uniform in design, so that a completely naive serialization strategy could be employed.
We will start by outlining such a naive strategy, so that subsequent design choices can be motivated by how they improve on the basics.

An IR instruction is currently described by the following data:

* An integer opcode
* An operand representing the result type, which may be a "void" type for instructions that don't produce a value
* Zero or more operands representing the "arguments" to the operation
* Zero or more decorations, which attach additional data to the instruction
* Zero or more child instructions

An IR module is basically a tree of IR instructions rooted at the instruction representing the module.

We can thus imagine serializing an arbitrary IR instruction along the lines of the following pseudo-C declarations:

    struct SerializedIRInst
    {
        uint32_t            opcode;
        uint32_t            typeID;
        uint32_t            resultID;

        uint32_t            argCount;
        uint32_t            argIDs[argCount];

        uint32_t            decorationCount;
        SerializedIRInst    decorations[decorationCount];

        uint32_t            childCount;
        SerializedIRInst    children[childCount];
    };

The idea behind this encoding is that any value-producing instruction will have a unique `resultID`, and the `typeID` and `argIDs` of other instructions will reference those IDs to reference the orignal instruction that produced the value.

This design can encode almost any instruction we currently have perfectly, with a few important exceptions:

* IR constants require storage for raw data rather than operands. This could be solved by special-casing constants so that their `argIDs` array is interpreted as raw data rather than as IDs of operands.

* Currently "global" IR instructions (those that can have linkage) store their mangled name directly on the instruction. This should probably be fixed so that any such extra data is turned into either ordinary operands, or decorations (the latter makes the most sense for a mangled name).

The "naive" encoding there looks a lot like SPIR-V!
---------------------------------------------------

SPIR-V uses a very similar encoding to the above. A SPIR-V module is defined as sequence of 32-bit "words" and in particular each operand of an instruction is encoded as one word.

The main differences between the above encoding and SPIR-V are:

* SPIR-V tries to be a *bit* clever in that it packs the equivalent of the `opcode` and `argCount` fields as 16 bits each in the first 32-bit word of an instruction.

* A SPIR-V instruction only stores the `typeID` or `resultID` fields if they are required by a particular instruction. Knowing whether these are present or not requires looking up the opcode in a table (it is not possible to fully decode an instruction without knowing opcode-specific information)

* To allow a SPIR-V module to be read even when it contains unknown instructions, the equivalent of `argCount` actually stores the total number of words in the instruction, so that an unknown instruction can be easily skipped.

* Decorations in SPIR-V are not nested under the instruction they describe, and instead appear as distinct top-level instructions that have their target as an operand.

* SPIR-V only has one case of a "parent" instruction (functions), and rather than encode anything like the `childCount` above, they instead have a `OpFunction` instruction serve as an implicit "begin" marker that must be terminated with a matching `OpFunctionEnd`.

Of course, the fact that SPIR-V uses such an encoding is not a reason to follow suit (otherwise it would be natural to ask "can we just use SPIR-V"? ... which is maybe worth asking in terms of interoperability).

Other people have looked at making SPIR-V less bloated
------------------------------------------------------

At this point it is useful to read up on the [work](http://aras-p.info/blog/2016/09/01/SPIR-V-Compression/) Aras from Unity did on trying to apply a "fitler" to SPIR-V to turn it into something more amenable to off-the-shelf compression like zlib. The high-order bits from his effort were:

* Use a "varint" encoding so that integer values that are typically small but may need 16/32/64 bits in the worst case can still be stored compactly

* Remap the opcode range so that the most commonly occuring instructions (in a pre-determined corpus) get smaller opcode numbers and thus require fewer bits to encode their opcode.

* Delta-encode various things when they are likely to be identical or sequential across instructions (result IDs of value-producing instructions, target IDs of decorations).

* Use a table keyed on instruction opcodes to look up the right encoding strategy to apply to its operands.

These choices greatly reduced the size of a raw SPIR-V module, and also made the result more compressible with off-the-shelf tools.

Given that we are likely to use `.zip` files as a container technology, it makes some sense to also optimize our encoding for a combination of code simplicity and good size reduction with off-the-shelf tools (rather than go and do extremely "clever" things to make our encoding as compact as possible without the need for standard compression).

Abbreviations
-------------

There is this idea of "abbreviations" which gets used in both the DWARF format for debugging information and the LLVM bitcode format. In the DWARF context, the basic idea is that a serialized record (information about a type, statement, function, whatever) is a bunch of key-value pairs, but in most cases a file with contain many records with exactly the same keys, and values with similar size or distribution of values. Rather than store a record as something like:

    <recordTypeID>
    <key0> <value0>
    <key1> <value1>
    ...

There is instead an abbreviation definition somwhere like:

    DEFINE_ABBREVIATION <abbreviationID>
    <recordTypeID>
    <key0> <encoding0>
    <key1> <encoding1>

and then individual records are encoded as:

    <abbreviationID>
    <value0>
    <value1>
    ...

The `encoding` entries in an abberviation definition define how the values for the given key should be read from the file: fixed-width integers, varints, length-prefixed blobs, nul-temrinated strings, etc. There is also for an abbreviation to define that all records created with that abbreviation should use the same value for a given key (you can think of this as a zero-bit "encoding").

They LLVM bitcode notion of abbreviations is quite similar, except that they encode records as just a sequence of scalar values, and don't mandate a key/value format (although you could of course encode key/value pairs).

Defining abbreviations for Slang's IR
-------------------------------------

I'm going to try to stop myself from extrapolating from the IR encoding to anything large (like how to encode the AST/reflection structures, etc.).

We can imagine that an IR module starts with a list of 0 or more encoded abbreviations. A completely naive representation of an abbreviation might be:

    struct Abbreviation
    {
        uint32_t opcode; // the opcode for the result

        TypeEncodingOp typeOp; // how is the type encoded?
        uint32_t fixedTypeID;

        ResultEncodingOp resultOp; // how is the result encoded?

        ArgsEncodingOp argsOp; // how are the args encoded?
        uint32_t fixedArgsCount;

        // Leaving decorations out...

        ChildrenEncodingOp childrenOp;
    };

    // How is the type encoded?
    enum class TypeEncodingOp
    {
        None,       // Result type is "void"
        Fixed,      // No per-instruction bytes used to store type, just use `fixedTypeID`
        Explicit,   // Type encoded as an unsigned varint operand
    };

    // How is the result encoded?
    enum class ResultEncodingOp
    {
        None,       // This instruction doesn't produce a result, or is never referenced as an operand
        Implicit,   // Assign this instruction the next available ID implicitly
    };

    // How are arguments encoded?
    enum class ArgsEncodingOp
    {
        Fixed,      // There are a fixed number of argument operands, given by `fixedArgsCount`
        Variable,   // The instruction starts with a VarUInt count, which added to `fixedArgsCount` gives the total number of operands

        FixedData,  // The `fixedArgsCount` is a number of bytes of data to read for the value of a constant instruction
        VarData,    // The instruction starts with a VarUInt for the amount of data for a constant instruction
    };

    // How are the children, if any, encoded?
    enum class ChildrenEncodingOp
    {
        None,       // Instructions with this abbreviation will have no children
        Variable,   // Instruction will be followed by zero or more children, terminated with an `END` pseudo-instruction
    };

Again, I want to emphasize that the above is an extremely naive representation for abbreviations, and I'm not trying to advocate that we use exactly that.

Given some representation of abbeviations with the same degree of flexibility, though, you can imagine writing out a table of abbreviations like:

    //        opcode,         type                        result      args        children
    [0] = {   OPCODE_ADD,     Fixed, <type ID of `int`>,  Implicit,   Fixed, 2,   None }
    [1] = {   OPCODE_CALL,    Explicit, 0,                Implicit,   Variable,1, None }
    [2] = {   OPCODE_CALL,    None,0,                     None,       Variable,1, None }
    [3] = {   OPCODE_LABEL,   None,0,                     Implicit,   Fixed,0,    None }
    [4] = {   OPCODE_JUMP,    None,0,                     None,       Fixed,1,    None }
    [5] = {   OPCODE_FUNC,    Explicit,0,                 Implicit,   Fixed,0,    Variable }
    [6] = {   OPCODE_PARAM,   Explicit,0,                 Implicit,   Fixed,0,    None }
    [7] = {   OPCODE_END,     None,0,                     None,       Fixed,0,    None }
    [8] = {   OPCODE_RETURN,  None,0,                     None,       Fixed,1,    None }
    // ...

In this example, we define abbreviation zero to stand in for an `ADD` operation that always has type `int`, and always has two operands, so that assuming our operands can be encoded as single bytes, an add instruction could be encoded as three bytes:

    0x00 <leftOperand> <rightOperand>

The example defines two abbreviations for `CALL` instructions, one for calls that return a value, and one for calls to `void` functions. In each case, they use a varint to encode the number of arguments after the required function argument. So a call to a three-argument function returning an `int` might be:

    0x01 <typeIDOfInt> 0x03 <functionID> <argID0> <argID1> <argID2>

while a call to a one-argument function returning `void` could be:

    0x02 0x01 <functionID> <argID0>

An encoding of a simple function that computes the sum of two `int` params might be:

    0x05 <typeIDOfFunc>                 // Start a function with given type
        0x06 <typeIDOfInt>              // Define first parameter - implicitly gets local ID #0
        0x06 <typeIDOfInt>              // Define second parameter - implicitly gets local ID #1
        0x00 <localID 0> <localID 1>    // Add the two parameters - result gets local ID #2
        0x08 <localID 2>                // Return the sum
    0x07                                // end function

What "SMOL-V" does with SPIR-V is very similar to the kind of encoding I'm describing here, just with the table of "abbreviations" being fixed. We could do something similar and have a hard-coded table in the compiler, but I think the representation can be more future-proof (and self-desriving) by included the abbreviation table in the serialized file (just with a more clever encoding of the abbreviations themselves).

Local IDs and Non-Local IDs
---------------------------

In the above examples I've been hand-waving the encoding of operands, and now I'd like to describe a straw-man design for what operand encoding might look like.

First lets define what I mean by a "local" value. When starting to decode a parent instruction (one with children, like the function above) we conceptually open a new scope for local values. Any instructions that produce values in that scope will be implicitly identified by their zero-based index within that scope.

Every operand will be encoded as a single unsigned varint. If the least-significant bit of the decoded value `V` is zero, then we will treat the operand as representing a local value, while if it is one we will treat it as a "non-local" value. I'm not claiming this is the best possible encoding of that choice, but it is at least an option.

For a local operand, the raw value (after shifting that low bit out) is the delta to subtract from the highest-numbered local value defined so far, to get the index of the right local value.
Conceptually, while parsing in a local scope, if we run into an instruction with an abbreviation that marks it as value-producing, we do something like:

    localScope->lastLocalValueID = localScope->localValues.Count();
    localScope->localValues.add(inst);

Later, if we have an operand with an encoded value `V`, with the low bit clear, we do something like:

    IRInst* operand = localScope->localValues[ localScope->lastLocalValueID - (V>>1)];

Note that this works because the way we've designed out IR eliminates forward references within a function (with the exception of targets for jumps, which is a wrinkle I can get to later). You can only ever refer to values that have already been defined, so an unsigned offset that gets subtracted from the maximum ID works fine.

The case of "non-local" references is trickier. A naive encoding would say that after shifting out the low bit that flagged an operand as a non-local ID, we should just attempt to resolve the operand as if it occured "one level up". That is, something like:

    IRInst* resolveOperand(UInt rawVal, DecodeingScope* localScope)
    {
        if(rawVal & 1 == 0) { ... local case ... }
        else
        {
            // Try to resolve this operand in the next scope up the chain
            return resolveOperand(rawVal >> 1, localScope->parent);
        }
    }

That basic strategy would work, except for two important details:

1. It is likely that a given local scope (e.g., a function) only ever refers to a small number of values from the outer scope, and the outer scope is likely to have many values (e.g., for a full module) so that the likelihood that a non-local operand will be compact when encoded as a varint is small.

2. When it comes to things like the global/module scope, it is possible (though rare) that a forward reference will be required. That is, if we have mutually recursive functions, then the body of each needs to form a reference to the other.

Resovling issue (2) means that when we go to resolve a non-local operand in a parent scope, we can't assume the positive delta-encoding approach outlined above. We could either say that non-local IDs are always absolute (rather than delta-encoded) or that they use a signed delta encoding (which really amounts to burning another bit). Another option would be to optimize the delta encoding for the case of backward references so that with K instruction seen to far in a scope, encoded values in [0,K-1) represent back-references, while values in [K, infinity) represent forward references (after subtracting K).

Issue (1) is mostly a performance issue, but it could be resolved by having an "up-value" pseudo-instruction (borrowing a name from Lua, but not using it exactly the same way). An up-value instruction would take zero or more varints as operands. These varints would be resolved as global IDs according to the chosen encoding (see previous paragraph), but for the rest of the current local scope any remaining "global IDs" would be taken as indices into the array of instructions resolved by the up-value instruction.

So if we had code like (kind of doing psuedo-assembly here)

    FUNC ...        // defines ID #0 at module scope
    FUNC ...        // ID #1 at module scope

    FUNC <someType> // ID #2 at module scope

        UP_VALUES( 0x01, 0x03 ) // using absolute IDs here, for simplicity

        CALL ... 0x03 ...

    END

    FUNC ...        // ID #3 at module scope

The 0x03 as an operand to CALL would get recognized as a non-local operand, and shifted to get 0x01. Rather than refer to global value #1, though, this would be indirected through the table set up by the UP_VALUES instruction, and refer to global value 0x03 instead.

Obviously in the case above this doesn't save any encoding space (actualy it wastes space, setting up the UP_VALUES), but in cases where an instruction from outside the function gets used a lot (e.g., a user-defined structure type that gets used for multiple local variables) it could allow for a one-byte encoding in cases where multiple bytes would otherwise be needed.

The nice thing about defining UP_VALUES like this is that it completely optional, and we could start out without it and see whether it would pay off.

A Note On Basic Blocks
----------------------

In the in-memory encoding of the IR, basic blocks are themselves instructions, so tha the children of a function are its blocks, while the children of the blocks are the actual instructions.
This has advantages (but also disadvantages) for the front-end, but in the context of a serialized format, the extra level of hierarchy just seems wasteful.

Instead, I'd propose that inside the body of a function, the first block is implicitly started at the top of the function, and thereafter a new block can be introduced with a LABEL instruction (similar to SPIR-V, but without the requirement for a LABEL on the first block).
Blocks would be assigned IDs independent of other instructions, so that the first label is always block ID #0, and so forth (it is illegal to jump to the start of a function in the IR, so it doesn't need an ID).
Operands that refer to blocks should always just be encoded as a varint of the ID of the target block (maybe with a signed delta encoding rather than absolute indices).
This will work because the only instructions that can refer to blocks are those nested directly in the same function (no need to worry about local/non-local references), and the IR doesn't allow blocks to appear where other operands are expected or vice versa.

We could try to relax this and allow blocks to be referenced like ordinary operands, so that we don't lock ourselves out of things like expressing jumps to computed addresses in the future. If we do that, though, then "ordinary operands" need to gain the ability to do both backward and forward references, since we can forward reference blocks.

A Note on Decorations
---------------------

It seems simplest to just have the encoding write out decorations like ordinary instructions, but as a prefix on the instruction they decorate. The only degree of freedom this really eliminates is the ability for a decoration to itself be decorated (although that could be re-introduced with a pseudo-instruction is really required).

A Note on Opcodes
-----------------

Obviously the serialized IR needs a stable space of opcodes. My assumption is that the opcode values used for serialization will *not* in general be the same as those used in the in-memory IR.

My reason for thinking this is that the in-memory IR carefully arranges the order of opcodes so that "subtype" tests can be done by testing if an opcode is in a certain range. When a new opcode gets added, it might need to be wedged in between two existing opcodes to make the queries still work, which could shift everything around if they are densely packed.

In contrast, the serialized encoding would seem to benefit from having a single master `enum` of opcode values that only ever gets extended at the end of the range, so that everything is densely packed and as close to zero as possible (with abbreviations being able to remap the range for a particular file so that low-numbered abbreviations can make an operation with a high opcode number cheap to encode).

The only alternative I can see for using the same opcode space in memory and on disk would be to define the opcode space with lots of intentional "holes" so that we can easily go in and add new opcodes into empty space in existing ranges. I'm not sure the potential benefits of such an approach are worth the delicacy of maintaining a hand-crafted opcode space.

A Note on Mangled Names
-----------------------

Right now our in-memory IR stores mangled names directly on global instructions that might be imported/exported. I think we should probably move to something like SPIR-V's "linkage" decorations, by having an `IRImportDecoration` and an `IRExportDecoration` which both inherit from `IRLinkageDecoration`. The `IRLinkageDecoration` would have an operand for the mangled name (an `IRStringLiteral`), and the import/export distinction would indicate the role that the given instruction plays in the module.

In terms of the encoding, this eliminates one more special case to worry about.

A Note on Fast Lookup
---------------------

Everything I've described so far is kind of based on the idea that we'd start with a pointer to a serialized IR module and then just walk the whole thing. In practice, however, we really want to have the ability to look up and then deserialize individual top-level instructions as needed.

My assumption is that along with the flat serialized data stream for a module (encoded along the lines of what I've laid out so far, or using some completely different strategy) plus a flat array of offsets into that stream that maps the zero-based index of a top-level instruction to its starting point, or the starting point of its first decoration. Ideally, any symbol with linkage would put its import/export decoration first, so that we can easily read off its mangled name without doing a search over decorations.

Then a separate hash table can be built and serialized that maps hashed mangled names to the indices of global instruction in each hash bucket. A fast lookup scheme can then hash a mangled name, find the bucket, and scan the instructions listed in the bucket to see if any of them are actually a hit for the given mangled name. It could then deserialize just enough of the instruction to find any decorations that affect linking choices (e.g., the current IRTargetIntrinsicDecoration), and use those to decide which, if any, of the instructions matching a mangled name should be deserialized.

I'm assuming the hash table itself wouldn't contain the keys at all, so that the string storage only sits in the serialized IR.

A Note on Strings
-----------------

A lot of mangled names will share a common prefix. It might be beneficial if the strings in the serialized IR allow for some amount of redundancy to be eliminated, even if this isn't mirrored in the in-memory representation.

A simple appraoch would be to encode a string literal as an optional reference to an existing string literal (with the empty string being the default case) and then giving some additional characters that go after that other string. This would allow us to factor out common prefixes into their own strings, and then only store the unique suffixes.

Doing this at the level of string instructions in the IR might be wasteful, so this might actually need to turn into a separate string table that the strings constant instructons in the IR reference by index.

A more optimal string table could then be made by taking all the strings we need, sorting them lexicographically and then building a table that is something like:

    struct StringInfo
    {
        uint32_t bytesSharedWithPreviousString; // number of bytes in common prefix with "previous" string (for first entry in table, "previous" string is empty)
        uint32_t newByteCount;                  // number of new bytes to append to that common prefix to get this string
        uint32_t newBytesOffset;                // offset into the raw storage for the string table for the new bytes this string adds
    };

    struct StringTable
    {
        uint32_t dataOffset; // offset to the raw storage for string bytes

        // Table of string infos for the strings
        uint32_t stringCount;
        StringInfo strings[stringCount];
    };

Conclusion
----------

Okay, that was probably mostly a grab-bag of thoughts, but hopefully some of what I'd been thinking about comes to the surface here.
