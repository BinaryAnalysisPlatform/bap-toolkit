FROM servercontainers/samba:4.7.6 as builder

FROM debian:stable-slim
WORKDIR /artifacts
COPY --from=builder /bin/smb* /artifacts/bin/
COPY --from=builder /sbin/smb* /artifacts/bin/
COPY --from=builder /sbin/samba* /artifacts/bin/
COPY --from=builder /lib/libsmb* /artifacts/lib/
COPY --from=builder /lib/libsam* /artifacts/lib/
COPY --from=builder //lib/private /artifacts/lib/private

# CVE-2018-10918 (CWE 476)
