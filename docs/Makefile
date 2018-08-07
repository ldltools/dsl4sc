#
SUBDIRS = grammar

all::
	for d in $(SUBDIRS); do make -C $$d $@ || exit 1; done
clean::;	rm -f *~
veryclean::
install::
