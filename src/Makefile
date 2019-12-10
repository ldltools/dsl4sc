# -*-Makefile-*-
# $Id: $

all::
clean::
veryclean::

TOP_DIR		= $(shell readlink -f ..)
BUILD_DIR	= $(TOP_DIR)/_build/caml
#PREFIX		?= $(TOP_DIR)/_build/install
PREFIX		?= /usr/local
LDLSATLIBDIR	?= $(PREFIX)/lib/ldlsat
SRC_DIRS	= ./rules ./spec

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
CAML_PKG_FLAGS	+= -package xml-light
#CAML_PKG_FLAGS	+= -package bigstring
#CAML_PKG_FLAGS	+= -package sexplib
#CAML_PKG_FLAGS	+= -package ppx_sexp_conv
CAML_PKG_FLAGS	+= -package sedlex

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

CAMLCFLAGS	= $(CAML_PKG_FLAGS) $(CAML_INC_FLAGS)
CAMLOPTFLAGS	= $(CAML_PKG_FLAGS) $(CAML_INC_FLAGS)

CAMLCFLAGS	+= -unsafe-string
CAMLOPTFLAGS	+= -unsafe-string

ifdef DEBUG
CAMLCFLAGS	+= -g -w +A
CAMLOPTFLAGS	+= -g -w +A
else
CAMLCFLAGS	+= -noassert -w -A+P
CAMLOPTFLAGS	+= -noassert -w -A+P
endif

ifdef PROF
#CAMLCFLAGS	+= -p a
CAMLOPTFLAGS	+= -p
endif

FOR_PACK	= -for-pack Dsl4sc

CMAS		=
#CMAS		+= nums.cma
CMAS		+= result.cma ppx_deriving_runtime.cma ppx_deriving_yojson_runtime.cma
CMAS		+= easy_format.cma biniou.cma yojson.cma
#CMAS		+= unix.cma bigarray.cma bigstring.cma sexplib.cma
#CMAS		+= ppx_sexp_conv_expander.cma ppx_sexp_conv.cma
CMAS		+= xml-light.cma
CMAS		+= gen.cma sedlex.cma
CMAS		+= ldlsat.cma
CMXAS		= $(CMAS:%.cma=%.cmxa)

BINS.byte	= rulespp.byte rules2ldl.byte
BINS.opt	= rulespp.opt rules2ldl.opt
BINS		= $(BINS.byte:%=$(BUILD_DIR)/%) $(BINS.opt:%=$(BUILD_DIR)/%)

LIBS.byte	= dsl4sc.cma
LIBS.byte	:= $(LIBS.byte:%=$(BUILD_DIR)/_export/%)
LIBS.opt	= dsl4sc.cmxa
LIBS.opt	:= $(LIBS.opt:%=$(BUILD_DIR)/_export/%)
LIBS		= $(LIBS.byte) $(LIBS.opt) $(LIBS.opt:%.cmxa=%.a)

-include ./Makefile.common

all::	$(LIBS) $(BINS)

# rulespp
$(BUILD_DIR)/rulespp.byte:	$(LIBS.byte) $(BUILD_DIR)/rules/rulespp_main.cmo
	$(CAMLC) $(CAMLCFLAGS) -o $@ $(CMAS) menhirLib.cmo $^
$(BUILD_DIR)/rulespp.opt:	$(LIBS.opt) $(BUILD_DIR)/rules/rulespp_main.cmx
	$(CAMLOPT) $(CAMLOPTFLAGS) -o $@ $(CMXAS) menhirLib.cmx $^
$(BUILD_DIR)/rules/rulespp_main.cmo:	rules/rulespp_main.ml $(BUILD_DIR)/_export/dsl4sc.cmi
	$(CAMLC) $(CAMLCFLAGS) -I $(BUILD_DIR)/_export -o $@ -c $<
$(BUILD_DIR)/rules/rulespp_main.cmx:	rules/rulespp_main.ml $(BUILD_DIR)/_export/dsl4sc.cmi
	$(CAMLOPT) $(CAMLOPTFLAGS) -I $(BUILD_DIR)/_export -o $@ -c $<

# rules2ldl
$(BUILD_DIR)/rules2ldl.byte:	$(LIBS.byte) $(BUILD_DIR)/rules/rules2ldl_main.cmo
	$(CAMLC) $(CAMLCFLAGS) -o $@ $(CMAS) menhirLib.cmo $^
$(BUILD_DIR)/rules2ldl.opt:	$(LIBS.opt) $(BUILD_DIR)/rules/rules2ldl_main.cmx
	$(CAMLOPT) $(CAMLOPTFLAGS) -o $@ $(CMXAS) menhirLib.cmx $^
$(BUILD_DIR)/rules/rules2ldl_main.cmo:	rules/rules2ldl_main.ml $(BUILD_DIR)/_export/dsl4sc.cmi
	$(CAMLC) $(CAMLCFLAGS) -I $(BUILD_DIR)/_export -o $@ -c $<
$(BUILD_DIR)/rules/rules2ldl_main.cmx:	rules/rules2ldl_main.ml $(BUILD_DIR)/_export/dsl4sc.cmi
	$(CAMLOPT) $(CAMLOPTFLAGS) -I $(BUILD_DIR)/_export -o $@ -c $<

# dsl4sc library (packed)
SRC_MODULES	= $(patsubst %,rules/%,fsa protocol property rule rules rulespp)
SRC_MODULES	+= $(patsubst %,spec/%,spec specpp spec2ldl)
SRC_MODULES	+= $(BUILD_DIR)/rules/rules_p rules/rules_l
SRC_MODULES	+= rules/version
$(BUILD_DIR)/_export/dsl4sc.mli:	$(SRC_MODULES:%=%.mli)
	mkdir -p $$(dirname $@)
	rm -f $@; touch $@
	for f in $^; do \
	  m=$$(basename $$f .mli); \
	  echo "module $${m^} : sig" >> $@; \
	  cat $$f >> $@; \
	  echo "end" >> $@; \
	done
$(BUILD_DIR)/_export/dsl4sc.cmi:	$(BUILD_DIR)/_export/dsl4sc.mli
	$(CAMLC) $(CAMLCFLAGS) -o $@ $<
$(BUILD_DIR)/_export/dsl4sc.cmo:	$(CMOS) $(BUILD_DIR)/_export/dsl4sc.cmi
	$(CAMLC) $(CAMLCFLAGS) -o $@ -pack \
	$(BUILD_DIR)/rules/{fsa,protocol,property,rule,rules,rulespp,rules_p,rules_l}.cmo \
	$(BUILD_DIR)/spec/{spec,specpp,spec2ldl}.cmo \
	$(BUILD_DIR)/rules/version.cmo
$(BUILD_DIR)/_export/dsl4sc.cma:	$(BUILD_DIR)/_export/dsl4sc.cmo
	$(CAMLC) -o $@ -a $<
$(BUILD_DIR)/_export/dsl4sc.cmx:	$(CMXS) $(BUILD_DIR)/_export/dsl4sc.cmi
	$(CAMLOPT) $(CAMLOPTFLAGS) -o $@ -pack \
	$(BUILD_DIR)/rules/{fsa,protocol,property,rule,rules,rulespp,rules_p,rules_l}.cmx \
	$(BUILD_DIR)/spec/{spec,specpp,spec2ldl}.cmx \
	$(BUILD_DIR)/rules/version.cmx
$(BUILD_DIR)/_export/dsl4sc.cmxa:	$(BUILD_DIR)/_export/dsl4sc.cmx
	$(CAMLOPT) -o $@ -a $<

# parser
$(BUILD_DIR)/rules/rules_p.cmi: $(BUILD_DIR)/rules/rules.cmi
$(BUILD_DIR)/rules/rules_p.cmo: $(BUILD_DIR)/rules/rules.cmi
$(BUILD_DIR)/rules/rules_p.cmx:	$(BUILD_DIR)/rules/rules.cmx

# lexer
$(BUILD_DIR)/rules/rules_l.cmi: $(BUILD_DIR)/rules/rules_p.cmi
$(BUILD_DIR)/rules/rules_l.cmo: $(BUILD_DIR)/rules/rules_l.ml $(BUILD_DIR)/rules/rules_l.cmi
	$(CAMLC) $(CAMLCFLAGS) $(FOR_PACK) -o $@ -c $<
$(BUILD_DIR)/rules/rules_l.cmx: $(BUILD_DIR)/rules/rules_l.ml $(BUILD_DIR)/rules/rules_l.cmi $(BUILD_DIR)/rules/rules_p.cmx
	$(CAMLOPT) $(CAMLOPTFLAGS) $(FOR_PACK) -o $@ -c $<
	touch -r $(BUILD_DIR)/rules/rules_l.cmo $(BUILD_DIR)/rules/rules_l.cmi
$(BUILD_DIR)/rules/rules_l.ml:	rules/rules_l.ml
#	$(OPAM_PREFIX)/lib/sedlex/ppx/ppx.exe --impl $< -o $@ --dump-ast
	$(OPAM_PREFIX)/lib/sedlex/ppx/ppx.exe --impl $< -o $@

# ppx
$(BUILD_DIR)/ppx:
	$(CAMLOPT) $(CAML_INC_FLAGS) \
	-I $(OPAM_PREFIX)/lib/ppx_driver \
	-I $(OPAM_PREFIX)/lib/ppx_core \
	-I $(OPAM_PREFIX)/lib/ppx_optcomp \
	-predicates ppx_driver -o $@ -linkpkg \
	-package ppx_deriving -package ppx_deriving.show -package ppx_deriving_yojson \
	{ppx_deriving_runtime,ppx_deriving_yojson_runtime}.cmxa \
	{ppx_core,ppx_optcomp,ppx_driver,ppx_driver_runner}.cmxa

#
Makefile.depend::	$(SRCS_mli) $(SRCS_ml) $(SRCS_mly)
	@n=0; missing=0;\
	while test $$n -lt 10; do \
	 test -s Makefile.depend || { sleep 1s; continue; };\
	 grep -q '^# generated at' Makefile.depend || { sleep 1s; continue; };\
	 line=`tail -1 Makefile.depend | grep '^# generated at'`;\
	 test ."$$line" = . && exit 0;\
	 break;\
	done

#
install::	install_bins install_libs
install_bins::	all
	@mkdir -p $(PREFIX)/bin
	for f in rulespp rules2ldl; do \
	rsync -avzop $(BUILD_DIR)/$$f.opt $(PREFIX)/bin/$$f; \
	done
install_libs::	all
	@mkdir -p $(PREFIX)/lib/dsl4sc
#	rsync -avzop $(LIBS) $(PREFIX)/lib/dsl4sc
#	rsync -avzop $(CMIS) $(PREFIX)/lib/dsl4sc
#	rsync -avzop $(SRCS_mli) $(PREFIX)/lib/dsl4sc
	rsync -avzop $(BUILD_DIR)/_export/dsl4sc.{mli,cmi,cma,cmxa,a} $(PREFIX)/lib/dsl4sc

ci::
	for d in `find . -type d`; do \
	  test -d $$d/RCS || continue;\
	  (cd $$d; echo *.ml* | xargs ci -m.);\
	done
