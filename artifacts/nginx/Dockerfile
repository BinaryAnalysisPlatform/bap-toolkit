# nginx 1.7

FROM nginx:1.7 as builder

FROM debian:stable-slim
WORKDIR /artifacts
COPY --from=builder /usr/sbin/nginx bin/
