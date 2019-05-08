FROM ldltools/ldlsat-dev as builder
MAINTAINER LDL Tools development team <ldltools@outlook.com>

# dsl4sc
ADD . /root/dsl4sc
WORKDIR /root/dsl4sc
RUN eval `opam config env`;\
    opam install -y ocamlfind ppx_deriving ppx_deriving_yojson sedlex menhir xml-light;\
    apt-get install -y libgmp-dev; opam install -y z3;\
    export PREFIX=/usr/local; make veryclean && make && make install

# z3 archive
RUN tar cf /root/z3.tar `opam config var lib`/z3

# helpers
RUN apt-get install -y graphviz xqilla libxml2-utils

#
WORKDIR /root
CMD ["/bin/bash"]

# ====================
# final image
# ====================
FROM debian:stretch-slim
#FROM ubuntu:18.04

RUN echo "dash dash/sh boolean false" | debconf-set-selections;\
    dpkg-reconfigure -f noninteractive dash;\
    echo "/usr/local/lib" > /etc/ld.so.conf.d/usr-local-lib.conf;\
    apt-get update

# dsl4sc (and ldlsat)
COPY --from=builder /usr/local /usr/local
# z3
#COPY --from=builder /root/.opam /root/.opam
COPY --from=builder /root/z3.tar /root
RUN tar xf /root/z3.tar; rm -f /root/z3.tar
# helpers
RUN apt-get install -y gawk libgomp1 xqilla libxml2-utils

# examples & tests
ADD examples /root/examples
ADD tests /root/tests
RUN apt-get install -y make shelltestrunner

WORKDIR /root
CMD ["/bin/bash"]
