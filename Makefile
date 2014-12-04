SRC=src/System/Console/Questioner.hs

default: questioner

configure: questioner.cabal
	cabal configure

configure-examples: questioner.cabal
	cabal configure -fexamples

questioner: configure $(SRC)
	cabal build

list-prompt: configure-examples examples/ListPrompt.hs
	cabal build questioner-list-prompt

checkbox-prompt: configure-examples examples/CheckboxPrompt.hs
	cabal build questioner-checkbox-prompt

spinner: configure-examples examples/Spinner.hs
	cabal build questioner-spinner
