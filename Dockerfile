FROM binaryanalysisplatform/bap:testing as base

RUN sudo apk add zip python

COPY --chown=opam:nogroup . /bap-toolkit
WORKDIR /bap-toolkit
RUN eval $(opam env) && make && make install


FROM alpine
RUN apk update && apk add binutils gmp-dev libgcc libstdc++6
WORKDIR /home/opam
COPY --from=base /home/opam/.opam/4.09/bin/bap /usr/bin/
COPY --from=base /home/opam/.opam/4.09/lib/bap/*.plugin /home/opam/.opam/4.09/lib/bap/
COPY --from=base /home/opam/.opam/4.09/share/bap /home/opam/.opam/4.09/share/bap
COPY --from=base /home/opam/.opam/4.09/share/bap-api /home/opam/.opam/4.09/share/bap-api
COPY --from=base /home/opam/.opam/4.09/share/primus /home/opam/.opam/4.09/share/primus


RUN cp -l /usr/bin/ssl_client /artifact
CMD ["bap", "disassemble", "/artifact", "--recipe=defective-symbol"]
