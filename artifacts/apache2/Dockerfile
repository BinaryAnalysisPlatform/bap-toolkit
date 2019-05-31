# apache 2.4.18

FROM httpd:2.4.18 as builder

FROM debian:stable-slim
WORKDIR /artifacts
COPY --from=builder /usr/local/apache2/bin/* bin/
