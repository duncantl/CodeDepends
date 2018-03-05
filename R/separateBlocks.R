separateExpressionBlocks =
  #
  # This allows us to take a script that is made up of
  # blocks and unravel them into a list of individual top-level
  # calls. It does not unravel the individual calls, just the
  # top-level blocks. And it leaves top-level calls outside of {}
  # alone.
  #
  # This allows us to start with a block script and then to move
  # to individual calls.   In this way, we can work at higher resolution
  # and process/evaluate individual, top-level expressions rather than entire blocks.
  # We can easily compute the dependencies for either the blocks or the calls
  # and so by converting the blocks to calls, we work with not aggregated groups of calls
  # but individual ones directly.
  
function(blocks)
{

  tmp = lapply(blocks, function(x) {
                          if(is(x, "{"))
                             as.list(x[-1])
#                             unlist(x, recursive = FALSE)
                          else
                             x
  })
    if(all(is.na(names(tmp))))
        names(tmp) = NULL
  unlist(tmp, recursive = FALSE)
}
