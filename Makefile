OCAMLC = ocamlc
OCAMLDEP = ocamldep
OCAMLFIND = ocamlfind
OCAMLOPT = ocamlopt
MENHIR = menhir
OCAMLBUILD = ocamlbuild

# FILES
ROOT = $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
LIB_FOLDER = lib
LIB_BUILD = _build/$(LIB_FOLDER)
SRC_FOLDER = src



# FLAGS
FOLDERS = $(shell find $(LIB_FOLDER) -type d)
FOLDERS_FLAGS = $(foreach folder,$(FOLDERS),-I $(folder))
WARNING_FLAGS = -w @1..3@5..8@10..26@28..31+32..38@40..43@46..49+50
MENHIR_FLAGS = --explain --infer


LIBRARY_BASE = JMESPath
LIBRARY_OBJ = $(LIBRARY_BASE).cma
LIBRARY_NATIVE = $(LIBRARY_BASE).cmxa

BINARY = jmespathml.native


.PHONY: all
all: $(LIBRARY_OBJ) $(LIBRARY_NATIVE) $(BINARY)

$(LIBRARY_OBJ) $(LIBRARY_NATIVE):
	@ echo "Generating: $@"
	@ $(OCAMLBUILD) \
			$(FOLDERS_FLAGS) \
			-use-ocamlfind \
			-classic-display \
			-ocamlc "$(OCAMLC) $(WARNING_FLAGS) -annot" \
			-ocamlopt "$(OCAMLOPT) $(WARNING_FLAGS) -annot" \
			-menhir "$(MENHIR) $(MENHIR_FLAGS)" $@

$(BINARY): $(ROOT)/$(LIB_BUILD)/$(LIBRARY_NATIVE)
	@ echo "Generating: $@"
	@ $(OCAMLBUILD) \
			-I $(SRC_FOLDER) \
			-cflags "-I $(ROOT)/$(LIB_BUILD)" \
			-lflags "-I $(ROOT)/$(LIB_BUILD) $^" \
			-use-ocamlfind \
			-classic-display \
			-ocamlc "$(OCAMLC) $(WARNING_FLAGS) -annot" \
			-ocamlopt "$(OCAMLOPT) $(WARNING_FLAGS) -annot" \
			-ocamldep "$(OCAMLDEP) -I $(ROOT)/$(LIB_BUILD)" \
			$@

.PHONY: clean
clean:
	@ $(OCAMLBUILD) -classic-display -clean
	@ $(OCAMLBUILD) -classic-display -I src -clean


# Opam rules
install: META $(BINARY)
	$(OCAMLFIND) install $(BINARY) META

uninstall:
	$(OCAMLFIND) remove $(BINARY)
