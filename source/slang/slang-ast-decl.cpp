// slang-ast-decl.cpp
#include "slang-ast-decl.h"

#include "slang-ast-builder.h"
#include "slang-ast-dispatch.h"
#include "slang-syntax.h"

#include <assert.h>

namespace Slang
{

const TypeExp& TypeConstraintDecl::getSup() const
{
    SLANG_AST_NODE_VIRTUAL_CALL(TypeConstraintDecl, getSup, ())
}

const TypeExp& TypeConstraintDecl::_getSupOverride() const
{
    SLANG_UNEXPECTED("TypeConstraintDecl::_getSupOverride not overridden");
    // return TypeExp::empty;
}

InterfaceDecl* findParentInterfaceDecl(Decl* decl)
{
    auto ancestor = decl->parentDecl;
    for (; ancestor; ancestor = ancestor->parentDecl)
    {
        if (auto interfaceDecl = as<InterfaceDecl>(ancestor))
            return interfaceDecl;

        if (as<ExtensionDecl>(ancestor))
            return nullptr;
    }
    return nullptr;
}

bool isInterfaceRequirement(Decl* decl)
{
    auto ancestor = decl->parentDecl;
    for (; ancestor; ancestor = ancestor->parentDecl)
    {
        if (as<InterfaceDecl>(ancestor))
            return true;

        if (as<ExtensionDecl>(ancestor))
            return false;
    }
    return false;
}


List<Decl*> const& ContainerDecl::getMembers()
{
    if (_members.isDoingOnDemandDecode())
    {
        _members.ensureAllDirectMemberDeclsAreLoaded();
    }

    return _members.members;
}

Count ContainerDecl::getDirectMemberDeclCount()
{
    return _members.members.getCount();
}

Decl* ContainerDecl::getDirectMemberDecl(Index index)
{
    SLANG_ASSERT(!_members.isDoingOnDemandDecode());

    return _members.members[index];
}

List<TransparentMemberInfo> const& ContainerDecl::getTransparentMembers()
{
    SLANG_ASSERT(!_members.isDoingOnDemandDecode());

    _ensureLookupAcceleratorsAreValid();
    return _members.transparentMembers;
}

/// Find the first direct member declaration of this container declaration
/// that has the given `name`.
///
Decl* ContainerDecl::findFirstDirectMemberOfName(Name* name)
{
    if (_members.isDoingOnDemandDecode())
    {
        return _members.findDirectMemberDeclByNameInBinaryModule(name);
    }

    _ensureLookupAcceleratorsAreValid();
    Decl* decl = nullptr;
    _members.memberDictionary.tryGetValue(name, decl);
    return decl;
}

/// Find the next direct member declaration of this container declaration
/// that has the same name as the given `memberDecl`.
///
Decl* ContainerDecl::findNextDirectMemberDeclWithSameName(Decl* memberDecl)
{
    SLANG_ASSERT(!_members.isDoingOnDemandDecode());

    _ensureLookupAcceleratorsAreValid();
    return memberDecl->nextInContainerWithSameName;
}

void ContainerDeclMembers::_add(Decl* decl)
{
    SLANG_ASSERT(isDoingOnDemandDecode());
    members.add(decl);
}

void ContainerDeclMembers::_initForOnDemandDecode(
    Count memberCount,
    UInt32 idForContainerDecl,
    void const* dataForContainerDeclMembers,
    RefPtr<RefObject> decodeContext)
{
    members.reserve(memberCount);
    for (Index i = 0; i < memberCount; ++i)
        members.add(nullptr);

    this->onDemandDecodeData = dataForContainerDeclMembers;
    this->onDemandDecodeContext = decodeContext;
    this->onDemandDecodeID = idForContainerDecl;
}

bool ContainerDeclMembers::isDoingOnDemandDecode()
{
    return this->onDemandDecodeData != nullptr;
}

void ContainerDeclMembers::ensureAllDirectMemberDeclsAreLoaded()
{
    if (!isDoingOnDemandDecode())
        return;

    auto memberCount = members.getCount();
    for (Index i = 0; i < memberCount; ++i)
    {
        if (members[i])
            continue;

        members[i] = getDirectMemberDeclByIndexInBinaryModule(i);
    }

    // TODO: In principle we could clear the `onDemandDecodeData`
    // member here, so that the container no longer returns `true`
    // for `isDoingOnDemandDecode()`.
    //
    // The reason this isn't being done yet is that there are
    // cases where a `ModuleDecl` needs to retain the ability
    // to do on-demand operations (e.g., to resolve the mapping
    // from mangled names to exports), and we don't want to
    // accidentally disable that just because all of a module's
    // direct members have ended up being loaded.
}

/// Add the given `memberDecl` as a direct member declaration.
///
void ContainerDecl::addDirectMemberDecl(Decl* memberDecl)
{
    SLANG_ASSERT(!_members.isDoingOnDemandDecode());

    if (!memberDecl)
        return;

    memberDecl->parentDecl = this;
    _members.members.add(memberDecl);
}

// The functions after this point are *technically* part of the public
// API of `ContainerDecl`, but they are really not things that code
// *should* be using, if they can be avoided.

void ContainerDecl::_removeDirectMemberDecl(Decl* memberDecl)
{
    SLANG_ASSERT(!_members.isDoingOnDemandDecode());

    _members.members.remove(memberDecl);

    _invalidateLookupAccelerators();
}

void ContainerDecl::_replaceDirectMemberDeclAtIndex(Index index, Decl* replacementMemberDecl)
{
    SLANG_ASSERT(!_members.isDoingOnDemandDecode());

    _members.members[index] = replacementMemberDecl;

    _invalidateLookupAccelerators();
}

void ContainerDecl::_insertDirectMemberDeclAtIndex(Index index, Decl* memberDecl)
{
    SLANG_ASSERT(!_members.isDoingOnDemandDecode());

    _members.members.insert(index, memberDecl);

    _invalidateLookupAccelerators();
}

void ContainerDecl::_invalidateLookupAcceleratorsBecauseMemberDeclWillBecomeTransparent()
{
    SLANG_ASSERT(!_members.isDoingOnDemandDecode());

    _invalidateLookupAccelerators();
}

bool ContainerDecl::_areLookupAcceleratorsValid()
{
    SLANG_ASSERT(!_members.isDoingOnDemandDecode());

    return _members.memberCountWhenAcceleratorsLastBuilt == _members.members.getCount();
}

void ContainerDecl::_invalidateLookupAccelerators()
{
    SLANG_ASSERT(!_members.isDoingOnDemandDecode());

    _members.memberCountWhenAcceleratorsLastBuilt = -1;
}

void ContainerDecl::_ensureLookupAcceleratorsAreValid()
{
    SLANG_ASSERT(!_members.isDoingOnDemandDecode());

    // If the acceleration structures are already valid,
    // then we skip out on re-building them.
    //
    if (_areLookupAcceleratorsValid())
        return;

    // If the value of `dictionaryLastCount` is less than
    // zero, it means that `_invalidateLookupAccelerators`
    // has been called to completely invalidate all of
    // the acceleration structures.
    //
    if (_members.memberCountWhenAcceleratorsLastBuilt < 0)
    {
        // We reset the acceleration strucures to be empty,
        // so that we can re-build everything from scratch.
        //
        _members.memberCountWhenAcceleratorsLastBuilt = 0;
        _members.memberDictionary.clear();
        _members.transparentMembers.clear();
    }

    // There is some special-case logic inside the loop below,
    // that triggers when the container declaration is a generic.
    // We check whether or not `this` is a generic declaration
    // outside the loop, in a bid to be slightly more efficient.
    //
    GenericDecl* genericDecl = as<GenericDecl>(this);

    const Index memberCount = _members.members.getCount();

    SLANG_ASSERT(
        _members.memberCountWhenAcceleratorsLastBuilt >= 0 &&
        _members.memberCountWhenAcceleratorsLastBuilt <= memberCount);

    for (Index memberIndex = _members.memberCountWhenAcceleratorsLastBuilt;
         memberIndex < memberCount;
         ++memberIndex)
    {
        Decl* memberDecl = _members.members[memberIndex];

        // TODO(tfoley): Once we are doing on-demand deserialization,
        // we will need to decide what to do with a null `memberDecl`
        // in this case.

        // Members that are marked as transparent go into a separate
        // list, since they need to be queried for all lookup operations
        // into the container.
        //
        if (memberDecl->hasModifier<TransparentModifier>())
        {
            TransparentMemberInfo info;
            info.decl = memberDecl;
            _members.transparentMembers.add(info);
        }

        // Other than transparent members (handled above), we don't
        // attempt to accelerate lookup of members with no name.
        //
        auto name = memberDecl->getName();
        if (!name)
            continue;

        // A generic declaration currently stores its "inner"
        // declaration as one of its direct members, but we don't
        // want that inner declaration to be found during name
        // lookup in the scope of the generic, so if we find
        // an inner declaration here, we skip it.
        //
        if (genericDecl && memberDecl == genericDecl->inner)
            continue;

        // We are chaining all the direct member declarations
        // with the same name together into a singly-linked
        // list, connected by the `nextInContainerWithSameName`
        // field.
        //
        // Note: while the name of that member implies that
        // the dictionary would hold whatever member declaration
        // of the given name is first *by inde*, it actually
        // works the other way around: the dictionary holds the
        // most-recently-added declaration with the given name,
        // since in simple cases such a declaration would shadow
        // preceding declarations of that name.
        //
        Decl* nextMemberWithSameName = nullptr;
        _members.memberDictionary.tryGetValue(name, nextMemberWithSameName);
        memberDecl->nextInContainerWithSameName = nextMemberWithSameName;

        _members.memberDictionary[name] = memberDecl;
    }

    _members.memberCountWhenAcceleratorsLastBuilt = memberCount;
    SLANG_ASSERT(_areLookupAcceleratorsValid());
}

bool isLocalVar(const Decl* decl)
{
    const auto varDecl = as<VarDecl>(decl);
    if (!varDecl)
        return false;
    const Decl* pp = varDecl->parentDecl;
    if (as<ScopeDecl>(pp))
        return true;
    while (auto genericDecl = as<GenericDecl>(pp))
        pp = genericDecl->inner;
    if (as<FunctionDeclBase>(pp))
        return true;

    return false;
}

ThisTypeDecl* InterfaceDecl::getThisTypeDecl()
{
    return getMembersOfType<ThisTypeDecl>().getFirst();
#if 0
    for (auto member : members)
    {
        if (auto thisTypeDeclCandidate = as<ThisTypeDecl>(member))
        {
            return thisTypeDeclCandidate;
        }
    }
    SLANG_UNREACHABLE("InterfaceDecl does not have a ThisType decl.");
#endif
}

InterfaceDecl* ThisTypeConstraintDecl::getInterfaceDecl()
{
    return as<InterfaceDecl>(parentDecl->parentDecl);
}

void AggTypeDecl::addTag(TypeTag tag)
{
    typeTags = (TypeTag)((int)tag | (int)tag);
}

bool AggTypeDecl::hasTag(TypeTag tag)
{
    return ((int)typeTags & (int)tag) != 0;
}

void AggTypeDecl::unionTagsWith(TypeTag other)
{
    addTag(other);
}

} // namespace Slang
