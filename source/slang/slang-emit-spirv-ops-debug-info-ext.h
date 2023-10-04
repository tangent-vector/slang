#ifdef SLANG_IN_SPIRV_EMIT_CONTEXT

// https://github.com/KhronosGroup/SPIRV-Registry/blob/main/nonsemantic/NonSemantic.Shader.DebugInfo.100.asciidoc#DebugCompilationUnit
template<typename T>
SpvInst* emitOpDebugCompilationUnit(SpvInstParent* parent, IRInst* inst, const T& idResultType, SpvInst* set, SpvInst* version, SpvInst* dwarfVersion, SpvInst* source, SpvInst* language)
{
    static_assert(isSingular<T>);
    return emitInst(parent, inst, SpvOpExtInst, idResultType, kResultID, set, SpvWord(1), version, dwarfVersion, source, language);
}

// https://github.com/KhronosGroup/SPIRV-Registry/blob/main/nonsemantic/NonSemantic.Shader.DebugInfo.100.asciidoc#DebugSource
template<typename T>
SpvInst* emitOpDebugSource(SpvInstParent* parent, IRInst* inst, const T& idResultType, SpvInst* set, IRInst* file, IRInst* text)
{
    static_assert(isSingular<T>);
    return emitInst(parent, inst, SpvOpExtInst, idResultType, kResultID, set, SpvWord(35), file, text);
}

// https://github.com/KhronosGroup/SPIRV-Registry/blob/main/nonsemantic/NonSemantic.Shader.DebugInfo.100.asciidoc#DebugLine
template<typename T>
SpvInst* emitOpDebugLine(SpvInstParent* parent, IRInst* inst, const T& idResultType, SpvInst* set, IRInst* source, IRInst* lineStart, IRInst* lineEnd, IRInst* colStart, IRInst* colEnd)
{
    static_assert(isSingular<T>);
    return emitInst(parent, inst, SpvOpExtInst, idResultType, kResultID, set, SpvWord(103), source, lineStart, lineEnd, colStart, colEnd);
}

// https://github.com/KhronosGroup/SPIRV-Registry/blob/main/nonsemantic/NonSemantic.Shader.DebugInfo.100.asciidoc#DebugScope
template<typename T>
SpvInst* emitOpDebugScope(
    SpvInstParent* parent, IRInst* inst, const T& idResultType, SpvInst* set,
    IRInst* scope)
{
    static_assert(isSingular<T>);
    return emitInst(parent, inst, SpvOpExtInst, idResultType, kResultID, set, SpvWord(23),
        scope);
}

template<typename T>
SpvInst* emitOpDebugFunction(
    SpvInstParent* parent,
    IRInst* inst,
    const T& idResultType,
    SpvInst* set,
    SpvInst* name,
    IRInst* debugType,
    SpvInst* debugSource,
    SpvInst* line,
    SpvInst* col,
    SpvInst* scope,
    SpvInst* linkageName,
    SpvInst* flags,
    IRInst* scopeLine)
{
    static_assert(isSingular<T>);
    return emitInst(parent, inst, SpvOpExtInst, idResultType, kResultID, set, SpvWord(20),
        name, debugType, debugSource, line, col, scope, linkageName, flags, scopeLine);
}

template<typename T>
SpvInst* emitOpDebugFunctionDefinition(
    SpvInstParent* parent,
    IRInst* inst,
    const T& idResultType,
    SpvInst* set,
    SpvInst*    debugFunc,
    SpvInst*    func)
{
    static_assert(isSingular<T>);
    return emitInst(parent, inst, SpvOpExtInst, idResultType, kResultID, set, SpvWord(101), debugFunc, func);
}

template<typename T>
SpvInst* emitOpDebugTypeFunction(
    SpvInstParent*  parent,
    IRInst*         inst,
    const T&        idResultType,
    SpvInst*        set,
    SpvInst*        flags,
    IRInst*         debugFuncType)
{
    static_assert(isSingular<T>);
    return emitInst(parent, inst, SpvOpExtInst, idResultType, kResultID, set, SpvWord(8),
        flags,
        OperandsOf(debugFuncType));
}

template<typename T>
SpvInst* emitOpDebugTypeBasic(
    SpvInstParent* parent,
    IRInst* inst,
    const T& idResultType,
    SpvInst* set,
    IRInst* name,
    SpvInst* size,
    SpvInst* encoding,
    SpvInst* flags)
{
    static_assert(isSingular<T>);
    return emitInst(parent, inst, SpvOpExtInst, idResultType, kResultID, set, SpvWord(2),
        name, size, encoding, flags);
}

template<typename T>
SpvInst* emitOpDebugTypeVector(
    SpvInstParent* parent,
    IRInst* inst,
    const T& idResultType,
    SpvInst* set,
    IRInst* elementType,
    IRInst* elementCount)
{
    static_assert(isSingular<T>);
    return emitInst(parent, inst, SpvOpExtInst, idResultType, kResultID, set, SpvWord(6),
        elementType, elementCount);
}

template<typename T>
SpvInst* emitOpDebugTypeComposite(
    SpvInstParent* parent,
    IRInst* inst,
    const T& idResultType,
    SpvInst* set,
    SpvInst* name,
    SpvInst* tag,
    SpvInst* source,
    SpvInst* line,
    SpvInst* col,
    SpvInst* parentScope,
    SpvInst* linkageName,
    SpvInst* size,
    SpvInst* flags
    /* TODO: members... */
    )
{
    static_assert(isSingular<T>);
    return emitInst(parent, inst, SpvOpExtInst, idResultType, kResultID, set, SpvWord(10),
        name,
        tag,
        source,
        line,
        col,
        parentScope,
        linkageName,
        size,
        flags);
}

template<typename T>
SpvInst* emitOpDebugInfoNone(
    SpvInstParent* parent,
    IRInst* inst,
    const T& idResultType,
    SpvInst* set)
{
    static_assert(isSingular<T>);
    return emitInstMemoized(parent, inst, SpvOpExtInst, idResultType, kResultID, set, SpvWord(0));
}

template<typename T>
SpvInst* emitOpDebugLocalVariable(
    SpvInstParent* parent,
    IRInst* inst,
    const T& idResultType,
    SpvInst* set,
    SpvInst* name,
    IRInst* debugType,
    SpvInst* debugSource,
    SpvInst* line,
    SpvInst* col,
    SpvInst* scope,
    SpvInst* flags,
    SpvInst* optParamIndex = nullptr)
{
    static_assert(isSingular<T>);
    return emitInst(
        parent, inst, SpvOpExtInst, idResultType, kResultID, set, SpvWord(26),
        name, debugType, debugSource, line, col, scope, flags,
        optionalPtrOperand(optParamIndex));
}

#if 0

    getSection(SpvLogicalSectionID::ConstantsAndTypes),
    inst,
    getVoidType(),
    getNonSemanticDebugInfoExtInst(),
    getDebugString(irDebugInfo->findNameInst()),
    irDebugInfo->findDebugType(),
    getDebugSource(irSourceLoc),
    getDebugLine(irSourceLoc),
    getDebugCol(irSourceLoc),
    getDebugScope(irDebugInfo->getParentScope()),
    emitIntConstant(0, builder.getIntType()));
#endif

#endif        // SLANG_IN_SPIRV_EMIT_CONTEXT
