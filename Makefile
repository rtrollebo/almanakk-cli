# Makefile for almanakk-cli

haskell_cmd=stack
haskell_cmd_build=build
haskell_cmd_package=sdist
path_to_library=../almanakk-lib
library_archive=almanakk-lib-1.0.0.1.tar.gz
path_to_library_distributable=/.stack-work/dist/aarch64-osx/Cabal-3.6.3.0/$(library_archive)
executable=almanakk-cli
path_to_cli=.stack-work/dist/aarch64-osx/Cabal-3.8.1.0/build/almanakk-cli/almanakk-cli


all: build_library ./lib/$(library_archive) build_cli almanakk-cli

# Build almanakk-lib
build_library: 
	cd ../almanakk-lib; $(haskell_cmd) $(haskell_cmd_build); $(haskell_cmd) $(haskell_cmd_package);
	
# Copy almanakk-lib to current ./lib folder
./lib/$(library_archive): build_library
	cp $(path_to_library)$(path_to_library_distributable) lib/

build_cli: ./lib/$(library_archive)
	$(haskell_cmd) $(haskell_cmd_build)

almanakk-cli: build_cli
	strip $(path_to_cli) -o $(executable)

clean:	
	rm stack.yaml.lock
	rm almanakk-cli.cabal

	