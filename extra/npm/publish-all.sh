#!/bin/bash
set -ex

# The publish will fail if the package name and version combination already exists in the specified registry.

version="1.2.1"
versionFull="0.19.1-$version"
return=$(pwd)

cd packages/lamdera-darwin-arm64
curl https://static.lamdera.com/bin/lamdera-$version-macos-arm64 -o lamdera
chmod a+x lamdera
cd "$return"

cd packages/lamdera-darwin-x64
curl https://static.lamdera.com/bin/lamdera-$version-macos-x86_64 -o lamdera
chmod a+x lamdera
cd "$return"

cd packages/lamdera-linux-arm64
curl https://static.lamdera.com/bin/lamdera-$version-linux-arm64 -o lamdera
chmod a+x lamdera
cd "$return"

cd packages/lamdera-linux-x64
curl https://static.lamdera.com/bin/lamdera-$version-linux-x86_64 -o lamdera
chmod a+x lamdera
cd "$return"

cd packages/lamdera-win32-x64
curl https://static.lamdera.com/bin/lamdera-$version-windows-x86_64.zip -o lamdera.zip
unzip -o lamdera.zip
mv lamdera/* ./
chmod a+x lamdera.exe
rm -rf lamdera.zip lamdera
cd "$return"


cd packages/lamdera-darwin-arm64
npm publish --access public
cd "$return"

cd packages/lamdera-darwin-x64
npm publish --access public
cd "$return"

cd packages/lamdera-linux-arm64
npm publish --access public
cd "$return"

cd packages/lamdera-linux-x64
npm publish --access public
cd "$return"

cd packages/lamdera-win32-x64
npm publish --access public
cd "$return"


# Finally, publish the top level `lamdera` package
npm publish --access public

exit

# When we have an arm32 bit build working we can publish this
# cd packages/lamdera-linux-arm
# curl https://static.lamdera.com/bin/lamdera-$version-linux-arm -o lamdera
# chmod a+x lamdera
# npm publish --access public
# cd "$return" || exit
