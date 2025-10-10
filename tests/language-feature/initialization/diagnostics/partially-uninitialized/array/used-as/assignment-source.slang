//DIAGNOSTIC_TEST:SIMPLE(filecheck=CHECK):

//
// Using an uninitialized value as the source of an
// assignment is an error.
//

void use_uninitialized_as_assignment_source()
{
    int[2] assignmentSrc;
    assignmentSrc[0] = 1;
    int[2] assignmentDst;
    //
    // TODO(tfoley): we only catch a case like this
    // if the uninitialized value goes on to be used
    // in some downstream code. If `assignmentDst`
    // get eliminated (e.g., because it is unused)
    // then no diagnostic gets emitted.
    //
    // CHECK: error 41016: use of uninitialized variable 'assignmentSrc'
    assignmentDst =  assignmentSrc;
    func_with_in_param(assignmentDst);
}

void func_with_in_param(int[2] param);
