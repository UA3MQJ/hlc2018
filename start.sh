#!/bin/bash
echo "* unzip /tmp/data/data.zip -d /app/priv/data"
/usr/bin/unzip -qq /tmp/data/data.zip -d /app/priv/data

# tree priv

# echo "* start"
iex --sname elx -S mix
