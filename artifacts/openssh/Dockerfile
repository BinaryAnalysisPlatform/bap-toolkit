FROM debian:stable-slim as builder

WORKDIR /build
RUN apt-get update && apt-get install -y build-essential

RUN apt-get install -y autoconf zlib1g-dev libssl1.0-dev
ADD https://github.com/openssh/openssh-portable/archive/V_7_3_P1.tar.gz .
RUN tar xzvf V_7_3_P1.tar.gz

WORKDIR openssh-portable-V_7_3_P1
RUN autoreconf && ./configure --prefix=/build && make && make install

WORKDIR /build

FROM debian:stable-slim
WORKDIR /artifacts
COPY --from=builder /build/bin bin/
COPY --from=builder /build/sbin bin/
COPY --from=builder /build/libexec bin/

# CVE-2016-10708 (CWE 476)
