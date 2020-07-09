FROM binaryanalysisplatform/bap:latest as base

RUN sudo apt-get install zip --yes

COPY --chown=opam:nogroup . /bap-toolkit
WORKDIR /bap-toolkit
RUN eval $(opam env) && make && make install


FROM ubuntu:16.04
RUN apt-get update && apt-get install libgmp-dev binutils --yes
WORKDIR /home/opam
COPY --from=base /home/opam/.opam/4.09/bin/bap /usr/bin/
COPY --from=base /home/opam/.opam/4.09/lib/bap/*.plugin /home/opam/.opam/4.09/lib/bap/
COPY --from=base /home/opam/.opam/4.09/share/bap /home/opam/.opam/4.09/share/bap
COPY --from=base /home/opam/.opam/4.09/share/bap-api /home/opam/.opam/4.09/share/bap-api
COPY --from=base /home/opam/.opam/4.09/share/primus /home/opam/.opam/4.09/share/primus


RUN cp -l /usr/bin/arch /artifact
CMD ["bap", "disassemble", "/artifact", "--recipe=defective-symbol"]
