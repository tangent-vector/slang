---
name: slang-refactor-trial
description: Advance this branch's Slang architecture extraction and initial code migration experiment. Use for module selection, refinement, migration, or resumption within Tess's authorized trial; excludes phase three and general cleanup.
---

# Slang Refactoring Trial

This branch tests a hierarchy of implementation modules described by Markdown documents that remain beside the resulting code.
The experiment preserves existing behavior and declaration names while changing ownership, directories, headers, and namespace qualification as needed for the agreed hierarchy.
Phase three is excluded; no additional goal may be inferred from that placeholder in the pitch.

## Resume the Authorized Work

Read the workspace's ignored `tmp/refactor-trial/resume.md`, `journal.md`, and current worklist before selecting work.
The checkpoint records the comparison revision, authorized deadline, branch, persistence remote, reviewed milestones, unresolved findings, and next action.
Verify those facts against the live checkout and current time.
At the deadline, preserve a recoverable checkpoint, stop delegated work, and report the result without starting another increment.
Do not extend the window or create an upstream pull request.

Use the host's `slang-author-and-self-review` skill and the repository clarity criteria for authored work.
Tess's pitch and subsequent instructions govern the experiment's scope; general cleanup guidance does not authorize signature changes, behavior changes, or deeper comment rewriting deferred by the pitch.
Local skills and Python helpers are permitted when they remove a concrete source of workflow error.
Prefer native bounded subagents to building a second agent orchestration system unless experience demonstrates that the latter is needed.

## Select and Refine One Module

Select a pending module in dependency-respecting, broadly top-down order.
Before refinement, read its ancestor documents from the root down, then its own document and relevant code.
The parent assigns the concern; the module author decides how that concern decomposes.
If the assignment cannot accommodate a necessary dependency or coherent responsibility, record a focused concern against the ancestor instead of silently changing neighboring modules.

A refined primary document explains:

- The services the module provides, briefly and in terms a reader can understand without knowing its declarations.
- The relevant concepts, with one canonical definition for each introduced term and links to definitions owned elsewhere.
- Dependencies and the purpose each serves.
- Immediate children, their public or private visibility, and links to their primary documents; a leaf instead describes its principal service areas.
- The existing files and partial-file responsibilities assigned to the module or its descendants.

Use `architecture/slang/.../<module>/<module>.md` before the tree transition and `source/slang/.../<module>/<module>.md` afterward.
The private-child `_` directory prefix is not part of the module's document basename: `_reader/reader.md` describes the private `reader` module.
Create a new child's stub with only a title and a brief service description.
Do not prescribe that child's internal structure while refining its parent.

Keep proposed boundaries distinguishable from observed implementation facts.
Before assigning an existing file wholesale, inspect its declarations throughout the file and record any services owned elsewhere as partial-file assignments.
A filename or a successful file-coverage check does not establish that all of its declarations serve one concern.
Definitions of existing project terms must agree with the owning types and their lifetime or behavior contracts; a familiar generic meaning is insufficient evidence.
An include edge is evidence to investigate, not proof of a semantic dependency.
Document mixed responsibilities and dependency cycles rather than hiding them in a vague shared module.
Product API headers under `include/` remain existing external contracts during the prototype; their use does not assign the entire public API to the compiler implementation module.

Use a fresh author context for each module where practical.
Request independent review of the actual document and implementation evidence, and a blinded reader check that excludes the authoring conversation and intended interpretation.
Resolve supported in-scope findings and repeat the relevant review before marking the module ready for migration.
Retain review coverage, version fingerprints, findings, and dispositions under the ignored trial directory.

For a fresh author or blinded reader created with the native collaboration tools, explicitly set `fork_turns` to `"none"`.
Omitting that option inherits the parent conversation and does not create the intended isolation.
Give a blinded reader the assignment, applicable requirements, document and source locations, and relevant scope boundaries, without the author's discussion or conclusions.
Record the actual context setup with the review evidence.
An audit that inherited authoring context may still provide useful independent source checks, but must not be described as blinded.
If that distinction was missed, obtain the separate blinded check before accepting the module for migration.

## Migrate an Investigated Module

Code porting requires the module's architecture document to exist, have completed the applicable review, and identify its ownership and dependencies.
The relevant ancestor documents must provide enough context to justify the placement.
Documentation and code remain together throughout migration; update the documents whenever code demonstrates that the proposed structure needs revision.

Before the first port, preserve baseline build and test evidence, then perform the pitch's `source/` to `old-source/` and `architecture/` to `source/` transition as a separate recoverable increment.
Keep the staging build usable while migrating small coherent services.
Preserve existing output filenames, exported product contracts, and behavior.
Audit all build consumers of moved implementations, including the runtime's separate compilation of foundation code.

For each increment, organize the public services before copying declarations, then migrate their implementations into the appropriate internal sections.
Public declarations receive accurate client documentation.
Implementation files receive a brief explanation of their task and organization.
Preserve good existing comments.
Record inherited naming or signature concerns, suspected bugs, and missing implementation narrative with specific `TODO(tfoley-refactor)` comments as directed by the pitch.
Do not fix those deferred concerns during this phase.

## Verify and Persist

Run the applicable formatter checks on the owned files before freezing a delivery or its comparison inputs.
Inspect any emitted corrections even when the formatter subsequently fails; an error exit does not invalidate a useful formatting diff.
Keep an actual unavailable check distinct from a source correction that can be completed.

In this Windows workspace, Gersemi 0.21 fails with its optional cache enabled but passes the same checks with `--no-cache`.
That supported control option leaves the formatting rules unchanged.
Retain the repository's other options and definition inputs, record the cache-disabled invocation, and preserve earlier failure logs rather than relabeling them as passing.

When a dedicated before-and-after observer is needed, keep stable provisional versions for author and independent source/prose review before declaring the final baseline sealed.
Apply the canonical documentation criteria to its selection adapter, capture helpers, and scratch build comments as well as its main source.
Seal the accepted original source, inputs, executable, outputs, and provenance before authorizing the production port.
If a later correction is necessary, preserve the sealed version and create a distinct revised baseline before proceeding; do not rewrite the prior evidence.

Use the platform-aware Slang build skill and Windows-native tools on this host.
Build with `SLANG_IGNORE_ABORT_MSG=ON`, keep assertions enabled, and avoid assertion modes that deliberately open dialogs.
Run tests through the trial's bounded process helper and test servers, with finite RPC timeouts.
Preserve exact commands, logs, exit status, configuration, and relevant skip or failure counts.

For a migrated service, verify that the build compiles its implementation from the new hierarchy and no longer compiles the old implementation into the same consumer.
An existing production consumer must exercise that implementation.
Use appropriate existing tests and focused differential checks when movement or namespace changes create risks not directly covered by those tests.
Scaffolding builds establish infrastructure progress only.

Review the cumulative owned diff, including new files, before publishing each coherent checkpoint.
Push only to the recorded personal fork branch, then verify the persisted head.
Keep trial logs, candidate reviews, baseline artifacts, and temporary experiments out of commits.
Leave the worklist explicit about unrefined documents, pending migration, unresolved concerns, and unavailable validation.
