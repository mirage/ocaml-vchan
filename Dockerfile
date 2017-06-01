FROM unikernel/mirage
RUN opam remote add live git://github.com/ocaml/opam-repository
RUN opam update -uy
COPY vchan.opam /src/vchan.opam
RUN opam install depext -y
RUN opam pin add vchan /src -n
RUN opam depext vchan xen-gnt-unix xen-evtchn-unix mirage-xen -y
RUN opam install xen-gnt-unix xen-evtchn-unix mirage-xen -y
RUN opam install vchan --deps-only
WORKDIR /src
