# wpa_supplicant 2.2
# CVE-2019-11555 (CWE 476)
# CVE-2015-5316  (CWE 476)


FROM debian:stable-slim as builder

WORKDIR /build
RUN apt-get update && apt-get install -y build-essential wget

RUN apt-get install -y libssl1.0-dev libnl-3-dev libnl-genl-3-dev

RUN wget http://hostap.epitest.fi/releases/wpa_supplicant-2.2.tar.gz && \
    tar xzf wpa_supplicant-2.2.tar.gz && \
    cd wpa_supplicant-2.2/wpa_supplicant && \
    cp defconfig .config &&  \
    sed -i "s/#CONFIG_LIBNL32=y/CONFIG_LIBNL32=y/" .config && \
    CFLAGS=-I/usr/include/libnl3 make && cp wpa_supplicant wpa_cli wpa_passphrase /build

FROM debian:stable-slim
WORKDIR /artifacts
COPY --from=builder /build/wpa_supplicant bin/
COPY --from=builder /build/wpa_cli bin/
COPY --from=builder /build/wpa_passphrase bin/
