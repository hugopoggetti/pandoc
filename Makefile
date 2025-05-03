##
## EPITECH PROJECT, 2025
## Makefile
## File description:
## PROJECT
##

EXEC_BASE = mypandoc-exe

EXEC = mypandoc

all:
	@(stack build)
	@(cp $$(stack path --local-install-root)/bin/$(EXEC_BASE) .)
	@(mv $(EXEC_BASE) $(EXEC))

run:
	stack exec $(EXEC)

tests_run:
	make all
	python test/PandocTest.py

clean:
	rm -rf $(EXEC)
	stack clean

fclean:
	rm -rf $(EXEC)
	stack clean

re:		clean all

.PHONY: 	clean all
