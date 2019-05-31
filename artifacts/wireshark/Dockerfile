# wireshark 2.6.0
# CVE-2019-10901 (CWE 476)
# CVE-2019-9214  (CWE 476)
# CVE-2019-9208  (CWE 476)
# CVE-2019-5719  (CWE 476)
# CVE-2018-19624 (CWE 476)
# CVE-2018-18227 (CWE 476)
# CVE-2018-11359 (CWE 476)
# CVE-2018-11356 (CWE 476)



FROM debian:stable-slim as builder

WORKDIR /build
RUN apt-get update && apt-get install -y build-essential wget

RUN apt-get install -y \
        doxygen bison cmake python libgcrypt20-dev libglib2.0-dev flex libpcap0.8-dev

RUN wget https://www.wireshark.org/download/src/all-versions/wireshark-2.6.0.tar.xz && \
    tar xf wireshark-2.6.0.tar.xz && \
    cd wireshark-2.6.0 && \
    ./configure --prefix=/build --disable-wireshark && make && make install

FROM debian:stable-slim
WORKDIR /artifacts
COPY --from=builder /build/bin bin/
COPY --from=builder /build/lib lib/
