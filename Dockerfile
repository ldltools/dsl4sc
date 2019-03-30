FROM ldltools/ldlsat-dev as builder
MAINTAINER LDL Tools development team <ldltools@outlook.com>

# dsl4sc
ADD . /root/dsl4sc
WORKDIR /root/dsl4sc
RUN eval `opam config env`;\
    opam install -y ocamlfind ppx_deriving ppx_deriving_yojson sedlex menhir xml-light;\
    export PREFIX=/usr/local; make veryclean && make && make install

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
    echo "/usr/local/lib" > /etc/ld.so.conf.d/usr-local-lib.conf
RUN apt-get update;\
    apt-get install -y gawk;\
    apt-get install -y graphviz xqilla libxml2-utils

WORKDIR /usr/local
COPY --from=builder /usr/local .
RUN ldconfig

WORKDIR /root
CMD ["/bin/bash"]
