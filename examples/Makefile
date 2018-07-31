# $Id: Makefile,v 1.1 2017/11/15 10:29:32 sato Exp sato $

SUBDIRS		= echo ping_pong even vending_machine car_loan
SUBDIRS		+= purchase_order accumulator approval

usage:
	@echo "run \"make\" in each of the following subdirectories"
	@for d in $(SUBDIRS); do echo -e "\t$$d"; done

all::
install::
clean::
	rm -f *~
	for d in $(SUBDIRS); do make -C $$d $@; done
veryclean::
	for d in $(SUBDIRS); do make -C $$d $@; done
