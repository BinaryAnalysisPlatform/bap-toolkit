FROM binaryanalysisplatform/bap:testing as base

COPY --chown=opam:nogroup src /src
WORKDIR /src
RUN eval "$(opam env)" && make build

FROM binaryanalysisplatform/bap-toolkit:testing as toolkit

COPY . /tests
WORKDIR /tests
COPY --from=base /src/compare-incidents /tests
COPY --from=binaryanalysisplatform/bap-artifacts:cron-3.0pl1-127 /artifact artifacts/cron-3.0pl1-127
COPY --from=binaryanalysisplatform/bap-artifacts:gifsicle-1.89 /artifact artifacts/gifsicle-1.89
COPY --from=binaryanalysisplatform/bap-artifacts:juliet-cwe-122 /artifact artifacts/juliet-cwe-122
COPY --from=binaryanalysisplatform/bap-artifacts:juliet-cwe-252 /artifact artifacts/juliet-cwe-252
COPY --from=binaryanalysisplatform/bap-artifacts:juliet-cwe-415 /artifact artifacts/juliet-cwe-415
COPY --from=binaryanalysisplatform/bap-artifacts:juliet-cwe-416 /artifact artifacts/juliet-cwe-416
COPY --from=binaryanalysisplatform/bap-artifacts:juliet-cwe-476 /artifact artifacts/juliet-cwe-476
COPY --from=binaryanalysisplatform/bap-artifacts:libgd-2.2.5 /artifact artifacts/libgd-2.2.5
COPY --from=binaryanalysisplatform/bap-artifacts:libssh-0.5.2 /artifact artifacts/libssh-0.5.2
COPY --from=binaryanalysisplatform/bap-artifacts:ntpd-4.2.8p12 /artifact artifacts/ntpd-4.2.8p12
COPY --from=binaryanalysisplatform/bap-artifacts:tcpreplay-4.3.1 /artifact artifacts/tcpreplay-4.3.1
RUN sh run.sh
