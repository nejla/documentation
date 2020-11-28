FROM ubuntu:eoan AS baseimage

LABEL project=nejlacommon-tutorial

RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get install -y --no-install-recommends  \
      ca-certificates \
      curl \
      libicu-dev \
      libpq-dev \
      locales \
      netbase  \
        && \
    rm -rf /var/lib/apt/lists/* && \
    localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8

ENV LANG=en_US.UTF-8

FROM baseimage

LABEL project=nejlacommon-tutorial

ADD ./dist/* /app/

ENTRYPOINT ["/app/nejlacommon-tutorial"]
