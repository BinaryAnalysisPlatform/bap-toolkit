FROM debian:stable-slim as builder

WORKDIR /build
RUN apt-get update && apt-get install -y build-essential

RUN apt-get install -y libssl1.0-dev libnl-3-dev libnl-genl-3-dev
ADD http://hostap.epitest.fi/releases/wpa_supplicant-2.2.tar.gz .
RUN tar xzvf wpa_supplicant-2.2.tar.gz

WORKDIR wpa_supplicant-2.2/wpa_supplicant
RUN cp defconfig .config &&  \
     sed -i "s/#CONFIG_LIBNL32=y/CONFIG_LIBNL32=y/" .config && \
     CFLAGS=-I/usr/include/libnl3 make && cp wpa_supplicant wpa_cli wpa_passphrase /build

WORKDIR /build

FROM debian:stable-slim
WORKDIR /artifacts
COPY --from=builder /build/wpa_supplicant bin/
COPY --from=builder /build/wpa_cli bin/
COPY --from=builder /build/wpa_passphrase bin/

# CVE-2019-11555 (CWE 476)
# CVE-2015-5316  (CWE 476)