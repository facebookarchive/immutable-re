build:
	ocamlbuild -package topkg pkg/build.native
	./build.native build

test:
	rebuild -cflag -g -Is src,src/sets,src/core,src/queues,src/indexed,src/maps,src/utils,reUnit/src,test/testers,test ./test/ImmutableTest.byte
	./ImmutableTest.byte

perf-debug:
	rebuild -cflag -g -Is src/sets,src/core,src/queues,src/indexed,src/maps,src/utils,src,reUnit/src,perf ./perf/PerfTest.byte
	./PerfTest.byte

perf:
	rebuild -Is src/sets,src/core,src/queues,src/indexed,src/maps,src/utils,src,reUnit/src,perf -ocamlopt 'ocamlopt -p -inline 100 -unsafe -noassert -nodynlink' ./perf/PerfTest.native
	./PerfTest.native

clean:
	ocamlbuild -clean
