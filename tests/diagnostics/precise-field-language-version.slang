//DIAGNOSTIC_TEST:SIMPLE(diag=WARN,exit-code=0): -std legacy -no-codegen
//DIAGNOSTIC_TEST:SIMPLE(diag=WARN,exit-code=0): -std 2018 -no-codegen
//DIAGNOSTIC_TEST:SIMPLE(diag=WARN,exit-code=0): -std 2025 -no-codegen
//DIAGNOSTIC_TEST:SIMPLE(diag=WARN,exit-code=0): -std 2026 -no-codegen
//DIAGNOSTIC_TEST:SIMPLE(diag=ERROR): -std 202c -no-codegen

// Field qualifiers remain effective in earlier language versions. Slang 202c rejects both
// instance and static fields, including class fields whose objects use reference semantics.
struct Value
{
    precise float field;
    //WARN: warning E56022
    //WARN: is deprecated and will be rejected in Slang 202c
    //ERROR: error E56023
    //ERROR: is not allowed on struct and class fields, including static fields, in Slang 202c

    static precise float staticField;
    //WARN: warning E56022
    //WARN: is deprecated and will be rejected in Slang 202c
    //ERROR: error E56023
    //ERROR: is not allowed on struct and class fields, including static fields, in Slang 202c

    float method(float input)
    {
        // A local variable inside a member function is a storage declaration, not a field.
        precise float local = input;
        return local;
    }
}

class Reference
{
    precise float field;
    //WARN: warning E56022
    //WARN: is deprecated and will be rejected in Slang 202c
    //ERROR: error E56023
    //ERROR: is not allowed on struct and class fields, including static fields, in Slang 202c

    static precise float staticField;
    //WARN: warning E56022
    //WARN: is deprecated and will be rejected in Slang 202c
    //ERROR: error E56023
    //ERROR: is not allowed on struct and class fields, including static fields, in Slang 202c
}

// Parameter and result qualifiers have function-local meaning and remain accepted in Slang 202c.
precise float supportedDeclarations(precise float input, out precise float output)
{
    precise float local = input;
    output = local;
    return local;
}
