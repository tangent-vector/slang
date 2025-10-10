//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

//
// Using an uninitialized value as the source of an
// assignment is an error.
//

void use_uninitialized_as_assignment_source()
{
    int assignmentSrc;
    int assignmentDst;
    // CHECK: error 41016: use of uninitialized variable 'assignmentSrc'
    assignmentDst = assignmentSrc;
}
