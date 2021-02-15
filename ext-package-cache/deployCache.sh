#!/usr/bin/env bash
set -e

normal()  { tput sgr0;    }
red()     { tput setaf 1; }
green()   { tput setaf 2; }

# run this from both osx and linux
if [[ $OSTYPE == darwin* ]]; then
# osx
echo "Operating system: $(green)osx$(normal)"
BIN_OS="osx"
fi
if [[ $OSTYPE == linux* ]]; then
# linux
echo "Operating system: $(green)linux$(normal)"
BIN_OS="linux"
fi


# Ensure we've got app name argument, otherwise quit
if [ $# -ne 1 ]
  then
    echo "Requires path to zip"
    exit 1
fi

zip=$1

read -r -p "$(red)Deploy $zip to prod? [y/N]$(normal) " response

if [[ "$response" =~ ^([yY]|[yY][eE][sS])+$ ]]
then
  echo "Placing $(which elmx) into root@lamdera-frankfurt-1:/srv/static/cache/$zip"
  scp $zip root@lamdera-frankfurt-1:/srv/static/cache/$zip
else
  echo "aborted"
  exit 1
fi
echo "all good, $zip deployed!"
exit 0



# To download and open
# curl -O https://static.lamdera.com/cache/elm-package-cache-2021-02-13.zip && unzip -p elm-package-cache-2021-02-13.zip | tar xf -
