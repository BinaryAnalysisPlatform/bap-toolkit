# openssl 1.1.0
# CVE-2016-7053 (CWE 476)
# CVE-2017-3730 (CWE 476)

FROM debian:stable-slim as builder

WORKDIR /build
RUN apt-get update && apt-get install -y build-essential wget

RUN wget https://github.com/openssl/openssl/archive/OpenSSL_1_1_0.tar.gz && \
    tar xzf OpenSSL_1_1_0.tar.gz && \
    cd openssl-OpenSSL_1_1_0 && \
    ./config --prefix=/build && make && make install

FROM debian:stable-slim
WORKDIR /artifacts
COPY --from=builder /build/bin bin/
COPY --from=builder /build/lib lib/
