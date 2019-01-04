#!/bin/bash
echo "* unzip /tmp/data/data.zip -d /app/priv/data"
/usr/bin/unzip -qq /tmp/data/data.zip -d /app/priv/data
cp /tmp/data/options.txt /app/priv/data

# ls priv/data

# echo "* start"
iex --sname elx -S mix
