FROM skeggib/elm_dev

RUN npm install -g local-web-server@3.0.7

WORKDIR /project
COPY app app
COPY src src
COPY elm.json .

RUN elm make src/Main.elm --output=app/main.js
WORKDIR /project/app
EXPOSE 80

CMD ws --spa index.html --port=80
