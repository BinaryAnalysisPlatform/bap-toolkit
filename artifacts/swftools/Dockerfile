# swftools 0.9.2
# CVE-2017-16868 (CWE 476)
# CVE-2017-16711 (CWE 476)
# CVE-2017-11101 (CWE 476)
# CVE-2017-11100 (CWE 476)
# CVE-2017-11097 (CWE 476)
# CVE-2017-11096 (CWE 476)


FROM debian:stable-slim as builder

WORKDIR /build
RUN apt-get update && apt-get install -y build-essential wget

RUN apt-get install -y zlib1g-dev

RUN wget http://swftools.org/swftools-0.9.2.tar.gz && \
    tar xzf swftools-0.9.2.tar.gz && \
    cd /build/swftools-0.9.2 && \
    ./configure --prefix=/build && make && \
    sed -i -e "s/swf -o/swf /" swfs/Makefile && \
    sed -i -e "s/-L / /" swfs/Makefile && \
    make install

FROM debian:stable-slim
WORKDIR /artifacts
COPY --from=builder /build/bin bin/
