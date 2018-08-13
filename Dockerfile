FROM ldltools/ldlsat
MAINTAINER LDL Tools development team <ldltools@outlook.com>

ENV DEBIAN_FRONTEND noninteractive
ENV DEBIAN_PRIORITY critical
ENV DEBCONF_NOWARNINGS yes

SHELL /bin/bash
RUN apt-get update

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
