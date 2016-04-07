FROM unikernel/mirage
RUN opam install --deps-only vchan -y
COPY build.sh /build.sh
