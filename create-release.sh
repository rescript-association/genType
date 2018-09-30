# Cleanup
rm -f lib/genflow-*.tar.gz

# MacOS
npm install
npm run clean && npm run build
tar czvf lib/genflow-macos.tar.gz  -C lib/bs/native genflow.native


# Linux
# Copies package.json etc. and runs `npm install`.
docker build -t genflow .

# Mounts only the lib and src volume and runs `npm run build`
docker run -it -v $PWD/lib:/genFlow/lib -v $PWD/src:/genFlow/src genflow bash -c "npm run clean && npm run build"

tar czvf lib/genflow-linux.tar.gz  -C lib/bs/native genflow.native
