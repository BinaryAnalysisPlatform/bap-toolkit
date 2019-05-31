# openssh 7.3.p1
# CVE-2016-10708 (CWE 476)

FROM debian:stable-slim as builder

WORKDIR /build
RUN apt-get update && apt-get install -y build-essential wget

RUN apt-get install -y autoconf zlib1g-dev libssl1.0-dev

RUN wget https://github.com/openssh/openssh-portable/archive/V_7_3_P1.tar.gz && \
    tar xzf V_7_3_P1.tar.gz && \
    cd openssh-portable-V_7_3_P1 && \
    autoreconf && ./configure --prefix=/build && make && make install

FROM debian:stable-slim
WORKDIR /artifacts
COPY --from=builder /build/bin bin/
COPY --from=builder /build/sbin bin/
COPY --from=builder /build/libexec bin/
