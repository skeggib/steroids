FROM angular AS build

WORKDIR /app

COPY package.json /app
RUN npm i

COPY e2e /app/
COPY src /app/
COPY angular.json /app
COPY ts-config.json /app
RUN ng build --prod