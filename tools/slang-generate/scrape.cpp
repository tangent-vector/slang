// scrape.cpp
#include "scrape.h"

namespace slang_generate
{
    //


    // Lexer

    // Parser

    //

    struct ScrapeContext
    {

    };

    FileDecl* scrapeFile(
        ScrapeContext*  context,
        char const*     path)
    {
        // First, figure out what kind of file we are dealing with,
        // by looking at its extension.
    }

    FileDecl* scrapeFile(char const* path)
    {
        ScrapeContext context;
        return scrapeFile(&context, path);
    }
}
