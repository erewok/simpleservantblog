#!/bin/bash

PROJ_DIR=/home/${PROJ_USER}/simpleservantblog
letsencrypt renew
cp /etc/letsencrypt/live/ekadanta.co/privkey.pem ${PROJ_DIR}/deployment/
cp /etc/letsencrypt/live/ekadanta.co/cert.pem ${PROJ_DIR}/deployment/
chown -R ${PROJ_USER}:${PROJ_USER} ${PROJ_DIR}/deployment
cd $PROJ_DIR
git pull
docker-compose build
docker-compose down
docker-compose up -d
