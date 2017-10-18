%.bc: %.ll
	llvm-as $^
%.s: %.bc
	llc $^
%.ll: %.cpp
	clang++ -S -emit-llvm $<
concat.bc: usefib.bc fib.bc
	llvm-link -o $@ $^
usefib: concat.s
	clang++ -o $@ $^
clean:
	rm -f *.s *.bc
