#!/usr/bin/env bash
set -ex

# tar cf - . | zip backup -
#
# would compress the output of the tar command for the purpose of backing up the current directory. This generally produces better compression than the previous example using the  -r
# option because zip can take advantage of redundancy between files. The backup can be restored using the command
#
# unzip -p backup | tar xf -
#
#

timestamp() {
  # date +"%Y-%m-%dT%H:%M:%S%z"
  date +"%Y-%m-%d"
}

return=`pwd`
dist=/Users/mario/dev/projects/lamdera-compiler/ext-package-cache/dist
cache=$dist/elm-package-cache
zip="elm-package-cache-$(timestamp)"

du -sh $cache

# Artifacts & docs can be rebuilt by the Elm compiler from source
find $cache -name "artifacts.dat" | xargs rm
find $cache -name "docs.json" | xargs rm

du -sh $cache


cd $dist
tar cf - . | zip $zip -
mv $zip.zip $return

cd $return
du -sh $zip.zip
