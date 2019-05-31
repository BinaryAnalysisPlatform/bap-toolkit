# lighttpd 1.4.15
# CWE 252

FROM debian:stable-slim as builder

WORKDIR /build
RUN apt-get update && apt-get install -y build-essential wget

RUN wget https://download.lighttpd.net/lighttpd/releases-1.4.x/lighttpd-1.4.15.tar.gz && \
    tar xf lighttpd-1.4.15.tar.gz && \
    cd lighttpd-1.4.15 && \
    ./configure --prefix=/build && make && make install

FROM debian:stable-slim
WORKDIR /artifacts
COPY --from=builder /build/bin bin/
COPY --from=builder /build/sbin bin/
COPY --from=builder /build/lib lib/
