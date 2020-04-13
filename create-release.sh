set -e

# Cleanup
rm -f lib/reanalyze-*.tar.gz

# MacOS
npm install
npm run clean && npm run build && npm run test
tar czvf lib/reanalyze-macos.tar.gz  -C src/lib/bs/native reanalyze.native


# Linux
# Copies package.json etc. and runs `npm install`.
docker build -t reanalyze .

# Mounts only the lib and src volume and runs `npm run build`
docker run -it -v $PWD/lib:/reanalyze/lib -v $PWD/src:/reanalyze/src reanalyze bash -c "npm run clean && npm run build"

tar czvf lib/reanalyze-linux.tar.gz  -C src/lib/bs/native reanalyze.native
