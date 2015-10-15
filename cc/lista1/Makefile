all:	example.s

%.s:	%.ll
	llc -o $@ $<

%.ll:	%.c
	clang -S -emit-llvm -o $@ $<

%.1.ll: %.c
	clang -S -O1 -emit-llvm -o $@ $<

%.2.ll: %.c
	clang -S -O2 -emit-llvm -o $@ $<

%.3.ll: %.c
	clang -S -O3 -emit-llvm -o $@ $<

%.png: %.ll
	opt -analyze -dot-callgraph $<
	dot callgraph.dot -Tpng -o $@
	gwenview $@

clean:
	rm -vf *.ll *.s *.png callgraph.dot

.SECONDARY:
.PHONY:	all clean
