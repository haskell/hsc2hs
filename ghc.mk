
# ToDo: maybe use ghc-cabal to configure this stuff and build
# Paths_hsc2hs for us.
utils/hsc2hs_dist_MODULES = Main Paths_hsc2hs
utils/hsc2hs_VERSION = 0.67

ifeq "$(Windows)" "YES"
utils/hsc2hs_dist_PROG = hsc2hs
$(eval $(call build-prog,utils/hsc2hs,dist,0))

$(HSC2HS_INPLACE) : inplace/hsc2hs-$(utils/hsc2hs_VERSION)/template-hsc.h

utils/hsc2hs_template=inplace/hsc2hs-$(utils/hsc2hs_VERSION)/template-hsc.h

else
utils/hsc2hs_dist_PROG = hsc2hs-real
$(eval $(call build-prog,utils/hsc2hs,dist,0))

utils/hsc2hs_template=inplace/lib/hsc2hs-$(utils/hsc2hs_VERSION)/template-hsc.h

$(HSC2HS_INPLACE) : $(INPLACE_BIN)/hsc2hs-real \
	            $(utils/hsc2hs_template)
	$(RM) -f $@
	echo '#!$(SHELL)'  >> $@
	echo 'executablename=$(FPTOOLS_TOP_ABS)/$(INPLACE_BIN)/hsc2hs-real' >>$@
	echo 'datadir=$(FPTOOLS_TOP_ABS)/inplace/lib/hsc2hs-$(utils/hsc2hs_VERSION)' >>$@
	cat utils/hsc2hs/hsc2hs.wrapper >>$@
	$(EXECUTABLE_FILE) $@
endif

$(utils/hsc2hs_template) : utils/hsc2hs/template-hsc.h
	@$(MKDIRHIER) $(dir $@)
	$(CP) $< $@


$(utils/hsc2hs_dist_depfile) : utils/hsc2hs/dist/build/Paths_hsc2hs.hs

utils/hsc2hs/dist/build/Paths_hsc2hs.hs : utils/hsc2hs/ghc.mk $(MKDIRHIER)
	@$(RM) $@
	@$(MKDIRHIER) $(dir $@)
	echo "module Paths_hsc2hs where" >>$@
	echo "import Data.Version" >>$@
	echo "getDataFileName s = return s" >>$@
	echo "version = Version [$(subst .,$(comma),$(utils/hsc2hs_VERSION))] []" >>$@
