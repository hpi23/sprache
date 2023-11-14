#!/bin/sh

set -e

mkdir -p deps

fetch_repo() {
    if ! [ -d "$3" ]; then
        git clone "https://github.com/$1/$2.git" "$3"
        cd "deps/$2"
    else
        cd "$3"
        git pull
    fi

    cd ../..
}

if ! command -v curl &> /dev/null
then
    echo "Error: curl is not installed."
    exit 1
fi

fetch_repo hpi23 c-projects hpi-c-tests &

wait
