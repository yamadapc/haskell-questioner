SRC=src/System/Console/Questioner.hs
VERSION := $(shell cat questioner.cabal | grep -i "^version" | grep -o "[^ ]\+$$")

default: questioner

configure: questioner.cabal
	cabal configure

configure-examples: questioner.cabal
	cabal configure -fexamples

questioner: $(SRC)
	stack build

examples: list-prompt checkbox-prompt spinner progressbar

list-prompt: configure-examples examples/ListPrompt.hs
	cabal build questioner-list-prompt

checkbox-prompt: configure-examples examples/CheckboxPrompt.hs
	cabal build questioner-checkbox-prompt

spinner: configure-examples examples/Spinner.hs
	cabal build questioner-spinner

progressbar: configure-examples examples/ProgressBar.hs
	cabal build questioner-progressbar

publish:
	echo "Assuming version: " $(VERSION)
	echo "Would you like to continue?"
	@read
	git tag $(VERSION)
	git push
	git push --tags
	cabal sdist
	cabal upload dist/questioner-$(VERSION).tar.gz
