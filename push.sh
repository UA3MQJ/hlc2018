rm -r -f _build
rm -r -f  deps
rm -r -f  priv/openresty/client_body_temp
rm -r -f  priv/openresty/fastcgi_temp
rm -r -f  priv/openresty/proxy_temp
rm -r -f  priv/openresty/scgi_temp
rm -r -f  priv/openresty/uwsgi_temp
rm -r -f  priv/openresty/logs/*

docker build -t elixir .

docker tag elixir stor.highloadcup.ru/accounts/barnacle_hunter

docker push stor.highloadcup.ru/accounts/barnacle_hunter
