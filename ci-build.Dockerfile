FROM skeggib/elm_dev

COPY . /app/

WORKDIR /app

RUN elm make src/Main.elm --output=output/main.js
