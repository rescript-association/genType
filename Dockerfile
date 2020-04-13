FROM node:10.11.0-jessie as base
WORKDIR /reanalyze

COPY package.json package-lock.json bsconfig.json /reanalyze/

RUN npm install

CMD /bin/bash




