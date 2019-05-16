FROM ocaml/opam2 as bap-dev

RUN opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository#testing \
 && opam update \
 && opam depext --install bap --yes


FROM bap-dev as base
RUN sudo apt-get update \
  && sudo apt-get install -y zip

WORKDIR /bap-toolkit
COPY --chown=opam:opam . .
RUN opam exec -- python makes [build, install]

FROM ubuntu
ENV HOME /home/opam
COPY --from=base /home/opam/.opam/4.07/bin/bap* /usr/bin/
COPY --from=base /home/opam/.opam/4.07/lib/bap/*.plugin /home/opam/.opam/4.07/lib/bap/
COPY --from=base /home/opam/.opam/4.07/share /home/opam/.opam/4.07/share
