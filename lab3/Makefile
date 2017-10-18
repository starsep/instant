%.bc: %.ll
	llvm-as -o $@ $^

%.s: %-runtime.bc
	llc -o $@ -march=x86-64 $^

%.ll: %.cpp
	clang++ -o $@ -O0 -S -emit-llvm $<

%-runtime.bc: %.bc runtime.bc
	llvm-link -o $@ $^

BINARY=test hello

${BINARY}: %: %.s
	gcc -o $@ $^

clean:
	rm -f *.s *.bc ${BINARY}
