#!/bin/bash
echo "make jsonreader"

rm -f src/json_reader/jsonreader/*.o
rm -f src/json_reader/jsonreader/jsonreader
cd src/json_reader/jsonreader/
qmake
make
cd ..
cd ..
cd ..
