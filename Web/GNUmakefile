
DOCS= CodeDepends.pdf ../inst/doc/design.pdf
include $(OMEGA_HOME)/R/Config/GNUmakefile.Web

%.pdf: %.tex
	@echo "Making $(@F)"
	$(MAKE) -C $(@D) $(@F)

