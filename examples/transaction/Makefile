all::
clean::;	rm -f *~
install::

include ../Makefile.common

%.scxml:	%.spec
	rules2scxml $< -o $@ --exit=tx_done

SPECS	= transaction_protocol.spec transaction.spec
scxml:	$(SPECS:%.spec=%.scxml)
