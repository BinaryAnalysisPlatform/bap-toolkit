# ntpd 4.2.8p5
# CVE-2015-7977 (CWE 476)


FROM debian:stable-slim as builder

WORKDIR /build
RUN apt-get update && apt-get install -y build-essential wget

RUN wget https://www.eecis.udel.edu/~ntp/ntp_spool/ntp4/ntp-4.2/ntp-4.2.8p5.tar.gz && \
    tar xzf ntp-4.2.8p5.tar.gz && \
    cd ntp-4.2.8p5 && \
    ./configure --prefix=/build && make && make install

FROM debian:stable-slim
WORKDIR /artifacts
COPY --from=builder /build/bin bin/
