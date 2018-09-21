FROM node:10.11.0-jessie as base
WORKDIR /genFlow

COPY package.json package-lock.json bsconfig.json /genFlow/

RUN npm install

CMD /bin/bash




