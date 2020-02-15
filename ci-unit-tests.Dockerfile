FROM skeggib/elm_dev

COPY . /app/

WORKDIR /app

RUN elm-test
