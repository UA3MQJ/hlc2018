#!/bin/bash
uname -r

echo "* unzip /tmp/data/data.zip -d /app/priv/data"
/usr/bin/unzip -qq /tmp/data/data.zip -d /app/priv/data
cp /tmp/data/options.txt /app/priv/data

echo "* start openresty"
cd priv
cd openresty
PATH=/usr/local/openresty/nginx/sbin:$PATH
export PATH
nginx -p `pwd`/ -c conf/nginx.conf
cd ..
cd ..

echo "* start"
iex --sname elx -S mix
