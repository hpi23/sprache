FROM rust as builder

RUN USER=root cargo install --git=https://github.com/hpi23/sprache hpi-cli

FROM debian

RUN apt-get update \
&& apt-get install openssl -y

COPY --from=builder /usr/local/cargo/bin/hpi-cli /bin/hpi-cli

RUN mkdir /app

WORKDIR /app/

COPY ./mensa.hpi /app/mensa.hpi

CMD ["/bin/hpi-cli", "run", "/app/mensa.hpi"]
