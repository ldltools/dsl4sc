# -*-Makefile-*-
# $Id: $

all::

TOP_DIR		= $(shell readlink -f ../../..)
BUILD_DIR	= $(TOP_DIR)/_build/caml/modelgen
PREFIX		?= /usr/local

LDLSATLIBDIR	?= /usr/local/lib/ldlsat
DSL4SCLIBDIR	= $(TOP_DIR)/_build/caml/_export
SRC_DIRS	= .

OPAM_PREFIX	= $(shell opam config var prefix)
OCAMLFIND	= $(OPAM_PREFIX)/bin/ocamlfind
CAMLC		= $(OCAMLFIND) ocamlc
CAMLOPT		= $(OCAMLFIND) ocamlopt

ifdef PROF
#CAMLC		:= $(OCAMLFIND) ocamlcp
#CAMLOPT	:= $(OCAMLFIND) ocamloptp
endif

# for ocamlfind
CAML_PKG_PLAGS	=
CAML_PKG_FLAGS	+= -package ppx_deriving.eq
CAML_PKG_FLAGS	+= -package ppx_deriving.ord
CAML_PKG_FLAGS	+= -package ppx_deriving.show
CAML_PKG_FLAGS	+= -package ppx_deriving_yojson
#CAML_PKG_FLAGS	+= -package bigstring
#CAML_PKG_FLAGS	+= -package sexplib
#CAML_PKG_FLAGS	+= -package ppx_sexp_conv

# on our own
CAML_INC_FLAGS	=
CAML_INC_FLAGS	+= -I $(OPAM_PREFIX)/lib/result
CAML_INC_FLAGS	+= -I $(OPAM_PREFIX)/lib/ppx_deriving
CAML_INC_FLAGS	+= -I $(OPAM_PREFIX)/lib/ppx_deriving_yojson
CAML_INC_FLAGS	+= -I $(OPAM_PREFIX)/lib/yojson
CAML_INC_FLAGS	+= -I $(OPAM_PREFIX)/lib/biniou
CAML_INC_FLAGS	+= -I $(OPAM_PREFIX)/lib/easy-format
#CAML_INC_FLAGS	+= -I $(OPAM_PREFIX)/lib/bigstring
#CAML_INC_FLAGS	+= -I $(OPAM_PREFIX)/lib/sexplib
#CAML_INC_FLAGS	+= -I $(OPAM_PREFIX)/lib/ppx_sexp_conv
CAML_INC_FLAGS	+= -I $(OPAM_PREFIX)/lib/xml-light
CAML_INC_FLAGS	+= -I $(OPAM_PREFIX)/lib/gen
CAML_INC_FLAGS	+= -I $(OPAM_PREFIX)/lib/sedlex
CAML_INC_FLAGS	+= -I $(OPAM_PREFIX)/lib/menhirLib

CAML_INC_FLAGS	+= -I $(LDLSATLIBDIR)
CAML_INC_FLAGS	+= -I $(DSL4SCLIBDIR)

CAMLCFLAGS	= $(CAML_PKG_FLAGS) $(CAML_INC_FLAGS)
CAMLOPTFLAGS	= $(CAML_PKG_FLAGS) $(CAML_INC_FLAGS)

CAMLCFLAGS	+= -unsafe-string
CAMLOPTFLAGS	+= -unsafe-string

ifdef DEBUG
CAMLCFLAGS	+= -g -w +A
CAMLOPTFLAGS	+= -g -w +A
else
CAMLCFLAGS	+= -noassert -w -A
CAMLOPTFLAGS	+= -noassert -w -A
endif

ifdef PROF
#CAMLCFLAGS	+= -p a
CAMLOPTFLAGS	+= -p
endif

CMAS		=
#CMAS		+= nums.cma
CMAS		+= result.cma ppx_deriving_runtime.cma ppx_deriving_yojson_runtime.cma
CMAS		+= easy_format.cma biniou.cma yojson.cma
#CMAS		+= unix.cma bigarray.cma bigstring.cma sexplib.cma
#CMAS		+= ppx_sexp_conv_expander.cma ppx_sexp_conv.cma
CMAS		+= xml-light.cma
CMAS		+= gen.cma sedlexing.cma
CMAS		+= ldlsat.cma
CMXAS		= $(CMAS:%.cma=%.cmxa)

include $(TOP_DIR)/src/Makefile.common

BINS.byte	= modelgen.byte
BINS.opt	= modelgen.opt
BINS		= $(BINS.byte:%=$(BUILD_DIR)/%) $(BINS.opt:%=$(BUILD_DIR)/%)

LIBS.byte	= modelgen.cma
LIBS.byte	:= $(LIBS.byte:%=$(BUILD_DIR)/%)
LIBS.opt	= modelgen.cmxa
LIBS.opt	:= $(LIBS.opt:%=$(BUILD_DIR)/%)
LIBS		= $(LIBS.byte) $(LIBS.opt) $(LIBS.opt:%.cmxa=%.a)

all::	$(BINS)

$(BUILD_DIR)/modelgen.byte:	$(LIBS.byte) $(BUILD_DIR)/modelgen_main.cmo
	$(CAMLC) $(CAMLCFLAGS) -o $@ $(CMAS) menhirLib.cmo dsl4sc.cma $^
$(BUILD_DIR)/modelgen.opt:	$(LIBS.opt) $(BUILD_DIR)/modelgen_main.cmx
	$(CAMLOPT) $(CAMLOPTFLAGS) -o $@ $(CMXAS) menhirLib.cmx dsl4sc.cmxa $^

# library
$(BUILD_DIR)/modelgen.cma:	$(CMOS)
	$(CAMLC) $(CAMLCFLAGS) -o $@ -a \
	$(BUILD_DIR)/{oracle,model,modelgen}.cmo
$(BUILD_DIR)/modelgen.cmxa:	$(CMXS)
	$(CAMLOPT) $(CAMLOPTFLAGS) -o $@ -a \
	$(BUILD_DIR)/{oracle,model,modelgen}.cmx

#
install::	install_bins
install_bins::	all
	@mkdir -p $(PREFIX)/lib/dsl4sc/tools
	rsync -avzop $(BINS) $(PREFIX)/lib/dsl4sc/tools

clean::
veryclean::
