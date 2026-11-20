#ifndef SLANG_INCLUDE_SYSTEM_H
#define SLANG_INCLUDE_SYSTEM_H
// slang-include-system.h

#include "compiler-core/slang-source-loc.h"

namespace Slang
{

// A directory to be searched when looking for files (e.g., `#include`)
struct SearchDirectory
{
    SearchDirectory() = default;
    SearchDirectory(SearchDirectory const& other) = default;
    SearchDirectory(String const& path)
        : path(path)
    {
    }
    SearchDirectory& operator=(SearchDirectory const& other) = default;

    String path;
};

/// A list of directories to search for files (e.g., `#include`)
struct SearchDirectoryList
{
    // A parent list that should also be searched
    SearchDirectoryList* parent = nullptr;

    // Directories to be searched
    List<SearchDirectory> searchDirectories;
};

/* A helper class that builds basic include handling on top of searchDirectories/fileSystemExt and
 * optionally a sourceManager */
struct IncludeSystem
{
    enum class Mode
    {
        Quote,
        System,
    };

    SlangResult findFile(
        const String& pathToInclude,
        const String& pathIncludedFrom,
        PathInfo& outPathInfo,
        Mode mode = Mode::Quote);
    SlangResult findFile(
        SlangPathType fromPathType,
        const String& fromPath,
        const String& path,
        PathInfo& outPathInfo);

    /// Compute the spelling to use when looking up a file in the filesystem.
    ///
    /// Combines a relative `path` with a nonempty `fromPath`, then uses the
    /// filesystem's simplified spelling when available. On success, writes
    /// the spelling to `outPath` so the caller can use it as a cache key before
    /// probing for the file.
    SlangResult calcLookupPath(
        SlangPathType fromPathType,
        const String& fromPath,
        const String& path,
        String& outPath);

    /// Find the file at the supplied lookup path and obtain its filesystem identity.
    ///
    /// Uses `lookupPath` verbatim and records it in `outPathInfo` on success.
    /// Forwards filesystem failures and returns `SLANG_E_NOT_FOUND` when the
    /// filesystem reports a non-file path type.
    SlangResult findFileAtPath(const String& lookupPath, PathInfo& outPathInfo);

    String simplifyPath(const String& path);
    SlangResult loadFile(
        const PathInfo& pathInfo,
        ComPtr<ISlangBlob>& outBlob,
        SourceFile*& outSourceFile);
    inline SlangResult loadFile(const PathInfo& pathInfo, ComPtr<ISlangBlob>& outBlob)
    {
        SourceFile* sourceFile;
        return loadFile(pathInfo, outBlob, sourceFile);
    }

    SlangResult findAndLoadFile(
        const String& pathToInclude,
        const String& pathIncludedFrom,
        PathInfo& outPathInfo,
        ComPtr<ISlangBlob>& outBlob,
        Mode mode = Mode::Quote);

    SearchDirectoryList* getSearchDirectoryList() const { return m_searchDirectories; }
    ISlangFileSystemExt* getFileSystem() const { return m_fileSystemExt; }
    SourceManager* getSourceManager() const { return m_sourceManager; }

    /// Ctor
    IncludeSystem() = default;
    IncludeSystem(
        SearchDirectoryList* searchDirectories,
        ISlangFileSystemExt* fileSystemExt,
        SourceManager* sourceManager = nullptr);

protected:
    SearchDirectoryList* m_searchDirectories;
    ISlangFileSystemExt* m_fileSystemExt;
    SourceManager*
        m_sourceManager; ///< If not set, will not look up the content in the source manager
};

} // namespace Slang

#endif // SLANG_INCLUDE_HANDLER_H
