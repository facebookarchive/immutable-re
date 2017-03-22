include $(shell opam config var solvuu-build:lib)/solvuu.mk

SUBSTS:=./substs

pre_release:
ifndef version
	$(error environment variable 'version' is undefined)
endif
	export git_version="$(shell git rev-parse --verify HEAD)"; \
	export git_short_version="$(shell git rev-parse --short HEAD)"; \
	$(SUBSTS) package.json.in; \
	$(SUBSTS) opam.in
