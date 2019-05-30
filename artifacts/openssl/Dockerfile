FROM debian:stable-slim as builder

WORKDIR /build
RUN apt-get update && apt-get install -y build-essential

ADD https://github.com/openssl/openssl/archive/OpenSSL_1_1_0.tar.gz .
RUN tar xzvf OpenSSL_1_1_0.tar.gz

WORKDIR openssl-OpenSSL_1_1_0
RUN ./config --prefix=/build && make && make install

WORKDIR /build

FROM debian:stable-slim
WORKDIR /artifacts
COPY --from=builder /build/bin bin/
COPY --from=builder /build/lib lib/

# CVE-2016-7053 (CWE 476)
# CVE-2017-3730 (CWE 476)
