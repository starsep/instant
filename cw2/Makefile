%.bc: %.ll
	llvm-as $^
%.s: %.bc
	llc $^
%: %.s %.cpp
	g++ -o $@ $^
