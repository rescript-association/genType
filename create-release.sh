set -e

# Cleanup
rm -f lib/gentype-*.tar.gz

# MacOS
npm install
npm run clean && npm run build && npm run test
tar czvf lib/gentype-macos.tar.gz  -C src/lib/bs/native gentype.native


# Linux
# Copies package.json etc. and runs `npm install`.
docker build -t gentype .

# Mounts only the lib and src volume and runs `npm run build`
docker run -it -v $PWD/lib:/genType/lib -v $PWD/src:/genType/src gentype bash -c "npm run clean && npm run build"

tar czvf lib/gentype-linux.tar.gz  -C src/lib/bs/native gentype.native
