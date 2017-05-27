## change to inputs in, e.g, rownames(cov) = NULL

When handling non-simple RHS in assign-handler, any variable indicated as an
update is also indicated as an input for that expression.

in the above example, cov will NOT be listed as an output (only an
update and input)

The reason for this is that all the dependency-related code appeared
to assume this would occur.

## signficant change to handling functions applied via "functional" (ie *apply calls)

The function applied is now listed under functions *not* under inputs
(unless funcsAsInputs is TRUE for the collector used...).

Functions applied via the dplyr mutate_(all|if|at) and
summarize_(all|if|at) and purrr's map* are handled as called as well.
well.


## getVariableDepends and getSectionDepends now accept ...

The ... is passed directly to getInputs (and in getVariableDepends'
case, to getSectionDepends) when creating the default info value. This
allows users to, e.g., specify a collector, or formulaInputs=TRUE
directly int he call to the get*Depends functions.

## getVariables now works correctly.

*functions* argument is now ignored unless inputs is TRUE (user defined
functions created in that block are already captured as ouputs by
defualt)

## getCallGraph only accepts package names

The code-paths for other input types is currently broken, and I don't
have time to fix it right away, so the documentation has been updated
to reflect this.