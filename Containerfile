FROM alpine:latest
LABEL \
    org.opencontainers.image.authors="Rogerio Senna <rogeriocsenna@gmail.com>"
    purpose="Learning t3x/0"

RUN apk update && \
    apk upgrade && \
    apk add git && \

RUN useradd \
    --uid 1000 \
    --home-dir /home/user \
    --create-home \
    --shell /bin/bash \
    user

USER user

RUN git clone

WORKDIR 
