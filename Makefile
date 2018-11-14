# $Id: $

#PREFIX		?= $(shell readlink -f ./_build/install)
PREFIX		?= /usr/local
LDLSATLIBDIR	?= $(PREFIX)/lib/ldlsat

SUBDIRS		= src scripts tools examples tests docs

all::
	for d in $(SUBDIRS); do make -C $$d PREFIX=$(PREFIX) LDLSATLIBDIR=$(LDLSATLIBDIR) $@ || exit 1; done

install::	all
	for d in $(SUBDIRS); do make -C $$d PREFIX=$(PREFIX) $@; done

clean::
	find . -name '#*' -or -name '*~' -or -name '*.log' | xargs rm -f
	for d in $(SUBDIRS); do make -C $$d PREFIX=$(PREFIX) $@; done

veryclean::	clean
	for d in $(SUBDIRS); do make -C $$d PREFIX=$(PREFIX) $@; done
	rm -rf _build/*

#
GITHOME ?= $(HOME)/git/github.com/ldltools/dsl4sc
rsync::	clean
	test -d $(GITHOME) || exit 1
	rsync -avzop --exclude=_build --exclude=.git --exclude=out --exclude=obsolete ./ $(GITHOME)
tar:	veryclean
	(dir=`basename $$PWD`; cd ..; tar cvJf dsl4sc`date +%y%m%d`.tar.xz --exclude=.git --exclude=_build --exclude=RCS --exclude=obsolete $$dir)

# docker
DOCKER_IMAGE	= ldltools/dsl4sc
.PHONY:	$(DOCKER_IMAGE)-dev $(DOCKER_IMAGE)
$(DOCKER_IMAGE)-dev:
	docker images | grep -q '^ldltools/ldlsat-dev' || exit 1
	docker images | grep -q "^$@ " && { echo "** $@ exists"; exit 0; } ||\
	docker build --target builder -t $@ .
$(DOCKER_IMAGE):
	docker images | grep -q '^ldltools/ldlsat-dev' || exit 1
	docker images | grep -q "^$@ " && { echo "** $@ exists"; exit 0; } ||\
	docker build -t $@ .

docker-build-all:	$(DOCKER_IMAGE)-dev $(DOCKER_IMAGE)
docker-build:	$(DOCKER_IMAGE)
docker-run:	$(DOCKER_IMAGE)
	docker run -it --rm $<
