# $Id: $

#PREFIX		?= $(shell readlink -f ./_build/install)
PREFIX		?= /usr/local
LDLSATLIBDIR	?= $(PREFIX)/lib/ldlsat

SUBDIRS		= src scripts tools examples tests docs

all::
	for d in $(SUBDIRS); do $(MAKE) -C $$d PREFIX=$(PREFIX) LDLSATLIBDIR=$(LDLSATLIBDIR) $@ || exit 1; done

install::	all
	for d in $(SUBDIRS); do $(MAKE) -C $$d PREFIX=$(PREFIX) $@; done

clean::
	find . -name '#*' -or -name '*~' -or -name '*.log' | xargs rm -f
	for d in $(SUBDIRS); do $(MAKE) -C $$d PREFIX=$(PREFIX) $@; done

veryclean::	clean
	for d in $(SUBDIRS); do $(MAKE) -C $$d PREFIX=$(PREFIX) $@; done
	rm -rf _build

tar:	veryclean
	(dir=`basename $$PWD`; cd ..; tar cvJf dsl4sc`date +%y%m%d`.tar.xz --exclude=.git --exclude=_build --exclude=RCS --exclude=obsolete $$dir)

# docker

DOCKER_REPO	= ldltools/dsl4sc
DOCKER_OPTS	?=
VERSION		= $(shell gawk '/^let get /{print($$NF)}' src/rules/version.ml)

.PHONY:	$(DOCKER_REPO)-dev $(DOCKER_REPO)

$(DOCKER_REPO)-dev:
	docker build --target builder -t $(DOCKER_REPO)-dev .
$(DOCKER_REPO):
	docker build -t $(DOCKER_REPO) .

docker-build:	docker-build-$(DOCKER_REPO)
docker-run:	check-latest-$(DOCKER_REPO)
	docker run -it --rm $(DOCKER_OPTS) $(DOCKER_REPO)

#
define GENRULES
docker-build-all::	docker-build-$(1)
docker-build-$(1):	$(1)

docker-tag::	docker-tag-$(1)
docker-tag-$(1):	check-latest-$(1) docker-untag-$(1)
	docker tag $(1):latest $(1):$$(VERSION)
	docker rmi $(1):latest

docker-untag::	docker-untag-$(1)
docker-untag-$(1):	check-latest-$(1)
	@docker images --format "{{.Repository}}:{{.Tag}}" | grep -q "$(1):$$(VERSION)" && docker rmi $(1):$$(VERSION) || true

check-latest-$(1):
	@docker images --format "{{.Repository}}:{{.Tag}}" | grep -q "$(1):latest" || { echo "** image \"$(1):latest\" not found"; exit 1; }
endef
$(foreach repo,$(DOCKER_REPO)-dev $(DOCKER_REPO),$(eval $(call GENRULES,$(repo))))
