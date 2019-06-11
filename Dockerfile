FROM binaryanalysisplatform/bap:testing as base

RUN sudo apk add zip python

COPY --chown=opam:nogroup . /bap-toolkit
WORKDIR /bap-toolkit
RUN opam exec -- python makes [build, install]


FROM alpine
RUN apk update && apk add binutils gmp-dev libgcc libstdc++6
WORKDIR /home/opam
COPY --from=base /home/opam/.opam/4.07/bin/bap /usr/bin/
COPY --from=base /home/opam/.opam/4.07/lib/bap/*.plugin /home/opam/.opam/4.07/lib/bap/
COPY --from=base /home/opam/.opam/4.07/share/bap /home/opam/.opam/4.07/share/bap
COPY --from=base /home/opam/.opam/4.07/share/bap-api /home/opam/.opam/4.07/share/bap-api
COPY --from=base /home/opam/.opam/4.07/share/primus /home/opam/.opam/4.07/share/primus


ENTRYPOINT ["bap"]
CMD ["/target", "--recipe=defective-symbol"]
