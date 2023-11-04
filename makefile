_opam:
	opam switch create . --no-install --yes

.PHONY: dev-env
dev-env: _opam
	opam install . --deps-only --with-test --yes
	ocaml-platform
	yarn

.PHONY: dev
dev:
	sh scripts/build_and_serve.sh

.DEFAULT_GOAL := dev
