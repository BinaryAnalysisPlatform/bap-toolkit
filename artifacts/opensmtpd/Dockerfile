# opensmtpd 5.7.3p2


FROM debian:stable-slim as builder

WORKDIR /build
RUN apt-get update && apt-get install -y build-essential wget

RUN apt-get install -y libasr-dev autoconf libtool zlib1g-dev libssl1.0-dev libevent-dev bison

RUN wget https://github.com/OpenSMTPD/OpenSMTPD/archive/opensmtpd-5.7.3p2.tar.gz && \
    tar xzf opensmtpd-5.7.3p2.tar.gz && \
    cd OpenSMTPD-opensmtpd-5.7.3p2 && \
    ./bootstrap && \
    ./configure --prefix=/build && make && make install

FROM debian:stable-slim
WORKDIR /artifacts
COPY --from=builder /build/bin bin/
COPY --from=builder /build/sbin bin/
COPY --from=builder /build/libexec bin/
