build:
	cp pkg/META.in pkg/META
	ocamlbuild -package topkg pkg/build.native
	./build.native build

ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
SUBSTS:=$(ROOT_DIR)/pkg/substs

pre_release:
ifndef version
	$(error environment variable 'version' is undefined)
endif
	export git_version="$(shell git rev-parse --verify HEAD)"; \
	export git_short_version="$(shell git rev-parse --short HEAD)"; \
	$(SUBSTS) $(ROOT_DIR)/package.json.in; \
	$(SUBSTS) $(ROOT_DIR)/opam.in

release: pre_release
	git add package.json opam
	git commit -m "Version $(version)"
	git tag -a $(version) -m "Version $(version)."
	# Push first the objects, then the tag.
	git push "git@github.com:facebookincubator/immutable-re.git"
	git push "git@github.com:facebookincubator/immutable-re.git" tag $(version)

clean:
	ocamlbuild -clean

.PHONY: build pre_release
