FROM node:10.11.0-jessie as base
WORKDIR /genType

COPY package.json package-lock.json bsconfig.json /genType/

RUN npm install

CMD /bin/bash




