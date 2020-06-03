FROM ubuntu:focal

RUN apt-get update && apt-get -y install shellcheck

WORKDIR /app

COPY . /app

ENTRYPOINT ["/app/support/shellcheck/run-shellcheck"]