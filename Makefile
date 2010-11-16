all: check

FORCE:

lispcmd: FORCE
	@echo MAKE $@ ; gnatmake -q -gnatoa -g lispcmd

check: lispcmd test.out test.in
	@echo CHECK
	@./lispcmd <test.in | (diff -u test.out - && echo "OK")

clean:
	rm -f *.ali *.o b~*
