FROM debian:stable-slim as builder

WORKDIR /build
RUN apt-get update && apt-get install -y build-essential

ADD https://www.eecis.udel.edu/~ntp/ntp_spool/ntp4/ntp-4.2/ntp-4.2.8p5.tar.gz .
RUN tar xzvf ntp-4.2.8p5.tar.gz

WORKDIR ntp-4.2.8p5
RUN ./configure --prefix=/build && make && make install

WORKDIR /build

FROM debian:stable-slim
WORKDIR /artifacts
COPY --from=builder /build/bin bin/

# CVE-2015-7977 (CWE 476)
