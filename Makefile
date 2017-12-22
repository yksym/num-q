
all:
	stack build

run:
	stack exec toy-machine-exe

test:
	stack build --test

example:
	stack exec bvm-exe

.PHONY: all run test example