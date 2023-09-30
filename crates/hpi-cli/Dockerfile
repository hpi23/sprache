FROM alpine:3

RUN addgroup hpi && adduser \
    --disabled-password \
    --gecos "" \
    --home /app \
    --ingroup hpi \
    --no-create-home \
    --uid 1000 \
    yaus

RUN apk update \
    && apk add --no-cache ca-certificates tzdata \
    && rm -rf /var/cache/apk/*

RUN mkdir /app

COPY ./hpi-cli /bin/hpi-cli

WORKDIR /app/

CMD ["/bin/hpi-cli", "/app/main.hpi"]
