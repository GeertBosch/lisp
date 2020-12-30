all: unittest check

FORCE:

lispcmd: FORCE
	@echo MAKE $@ ; gnatmake -q -gnatgo -fstack-check -g lispcmd -bargs -Es

unittest: FORCE
	@echo MAKE $@ ; gnatmake -q -gnatgo -fstack-check -g unittest -bargs -Es
	@./unittest && echo $@ OK

check: lispcmd test.out test.in
	@./lispcmd <test.in | (diff -u test.out - && echo $@ "OK")

clean:
	rm -f *.ali *.o b~* lispcmd unittest
