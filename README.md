# elx_small_http_cowboy

создать образ

 docker build -t elixir .

подключиться и зайти в него

 docker run -i -t <id контейнера> /bin/bash

запустить

 docker run --rm -p 8080:8080 -t elixir

Загрузить

docker tag elixir stor.highloadcup.ru/accounts/axolotl_shooter
docker push stor.highloadcup.ru/accounts/axolotl_shooter

 
 немного тестовых методов
 
curl --header "Content-Type: application/json"   --reque POST   --data '{"country": "Индезия", "city": "Роттероштадт"}'   http://localhost:8080/accounts/7313/?query_id=500

