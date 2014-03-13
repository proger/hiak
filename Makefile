VIM= mvim


shell:
	-pkill 'hdevtools'
	nix-shell --command 'exec zsh'

# hack to make hdevtools work inside nix-shell
hdevtools: $(PACKAGE_CONF)
	-pkill 'hdevtools'
	 env GHC_PACKAGE_PATH=$$(/nix/store/b1k4pnh667p2qp7dy6dh3mi72vq6wixv-ghc-packages.sh) hdevtools check $(HDEVTOOLS_ARGS) Hiak/CHash.hs

$(PACKAGE_CONF):
	cabal configure
