SRC_DIRS=src,src/sets,src/core,src/queues,src/indexed,src/maps,src/utils
build:
	rebuild -use-ocamlfind -Is $(SRC_DIRS) src/Immutable.cma
	rebuild -use-ocamlfind -Is $(SRC_DIRS) src/Immutable.cmxa


test-debug:
	rebuild -cflag -g -Is $(SRC_DIRS) -Is reUnit/src,test/testers,test ./test/ImmutableTest.byte
	./ImmutableTest.byte

perf-debug:
	rebuild -cflag -g -Is $(SRC_DIRS) -Is reUnit/src,perf ./perf/PerfTest.byte
	./PerfTest.byte

perf-native:
	rebuild -Is $(SRC_DIRS) -Is reUnit/src,perf -ocamlopt 'ocamlopt -p -inline 100 -unsafe -noassert -nodynlink' ./perf/PerfTest.native
	./PerfTest.native

clean:
	rebuild -clean
