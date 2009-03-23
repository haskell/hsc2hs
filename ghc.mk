utils/hsc2hs_USES_CABAL = YES
utils/hsc2hs_PACKAGE = hsc2hs

utils/hsc2hs_dist_PROG         = $(GHC_HSC2HS_PGM)
utils/hsc2hs_dist-install_PROG = $(GHC_HSC2HS_PGM)

utils/hsc2hs_dist_SHELL_WRAPPER = YES
utils/hsc2hs_dist_INSTALL_SHELL_WRAPPER = NO

utils/hsc2hs_dist-install_SHELL_WRAPPER = YES
utils/hsc2hs_dist-install_INSTALL_SHELL_WRAPPER = YES
utils/hsc2hs_dist-install_INSTALL_INPLACE = NO

$(eval $(call build-prog,utils/hsc2hs,dist,0))
$(eval $(call build-prog,utils/hsc2hs,dist-install,1))

# After build-prog above
utils/hsc2hs_dist_MODULES += Paths_hsc2hs
utils/hsc2hs_dist-install_MODULES = $(utils/hsc2hs_dist_MODULES)

utils/hsc2hs_dist_HC_OPTS         += -DNEW_GHC_LAYOUT
utils/hsc2hs_dist-install_HC_OPTS += -DNEW_GHC_LAYOUT

utils/hsc2hs_template=$(INPLACE_LIB)/template-hsc.h

$(HSC2HS_INPLACE) : $(utils/hsc2hs_template)

$(utils/hsc2hs_template) : utils/hsc2hs/template-hsc.h
	@$(MKDIRHIER) $(dir $@)
	$(CP) $< $@

install: install_utils/hsc2hs_dist_install

.PHONY: install_utils/hsc2hs_dist_install
install_utils/hsc2hs_dist_install: utils/hsc2hs/template-hsc.h
	$(MKDIRHIER $(datadir)
	$(CP) $< $(datadir)
