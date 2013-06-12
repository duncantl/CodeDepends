#
# If we create the timeline plot in D3, we can make it
# interactive and show the associated code in an HTML
# document. We can mark it up to show syntax, links, etc,
# but then when we higlight a point in the plot, we see
# the corresponding code hightlighted.
# Do this for the variables as inputs and outputs, i.e. separate modes.
library(CodeDepends)
library(RD3Device)
z = readScript("~/GitWorkingArea/RClangSimple/Paper/RClang.tex", "JSS")
dtm = getDetailedTimelines(z)
dev = d3Device()
plot(dtm)
dev.off()
