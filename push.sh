rm -r -f _build
rm -r -f  deps

docker build -t elixir .

docker tag elixir stor.highloadcup.ru/accounts/curly_wasp

docker push stor.highloadcup.ru/accounts/curly_wasp
