FROM gumjoe/wacc-ci-scala:slim

WORKDIR /app

COPY . .

RUN scala compile .
