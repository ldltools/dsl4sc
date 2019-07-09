#
FROM ldltools/ldlsat-dev as builder
MAINTAINER LDL Tools development team <ldltools@outlook.com>

# dsl4sc
ADD . /root/dsl4sc
WORKDIR /root/dsl4sc
RUN eval `opam config env`;\
    opam update; opam upgrade -y;\
    opam install -y ocamlfind ppx_deriving ppx_deriving_yojson sedlex menhir xml-light;\
    apt-get install -y libgmp-dev python2.7; opam install -y z3;\
    export PREFIX=/usr/local; make veryclean && make && make install

# helpers
RUN apt-get install -y graphviz xqilla libxml2-utils shelltestrunner
# node (for tools/safeguard)
RUN apt-get install -y wget;\
    wget -qO- https://raw.githubusercontent.com/nvm-sh/nvm/v0.34.0/install.sh | /bin/bash;\
    . /root/.nvm/nvm.sh; nvm install node

# z3 archive (for copying to the final image)
RUN tar cf /root/z3.tar `opam config var lib`/z3

#
WORKDIR /root
CMD ["/bin/bash"]

# ====================
# final image
# ====================
FROM debian:buster-slim

RUN echo "dash dash/sh boolean false" | debconf-set-selections;\
    dpkg-reconfigure -f noninteractive dash;\
    echo "/usr/local/lib" > /etc/ld.so.conf.d/usr-local-lib.conf;\
    apt-get update

# dsl4sc (and ldlsat)
COPY --from=builder /usr/local /usr/local

# z3
#COPY --from=builder /root/.opam /root/.opam
COPY --from=builder /root/z3.tar /root
RUN tar xf /root/z3.tar; rm -f /root/z3.tar;\
    apt-get install -y libgomp1
# helpers
RUN apt-get install -y gawk xqilla libxml2-utils shelltestrunner
# nvm/node
COPY --from=builder /root/.nvm /root/.nvm
COPY --from=builder /root/.bashrc /root
# examples & tests
ADD examples /root/examples
ADD tests /root/tests

WORKDIR /root
CMD ["/bin/bash"]
