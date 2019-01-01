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

HttpTest2.KVS.account(7313)
HttpTest2.KVS.account(7313)[:city]

curl --header "Content-Type: application/json"   --reque POST   --data '{"country": "Индезия", "city": "Роттероштадт"}'   http://localhost:8080/accounts/7313/?query_id=500

accounts = HttpTest2.KVS.read_file_sync("accounts")

accounts \
|> Enum.map(fn({id,item}) -> \
  new_item = item \
  |> Map.merge(%{id => item[:id] + 100000}) \
  {id + 100000, new_item} \
end)

for n <- 2..50 do
  mul = n
  accounts2 = accounts \
  |> Enum.map(fn({id,item}) ->
      new_item = item
      |> Map.merge(%{"id" => id + 100000 * mul})
      |> Map.merge(%{"email" => "#{mul}" <> item["email"]}) 
  end)
  map = %{accounts: accounts2}
  json = Eljiffy.encode!(map)
  File.write!("priv/data/accounts_#{mul}.json", json, [])
end 


curl --header "Content-Type: application/json"   --reque POST   --data '{"country": "Индезия", "city": "Роттероштадт"}'   http://localhost:8080/accounts/7313/?query_id=500

curl --header "Content-Type: application/json"   --reque POST   --data '{}'   http://localhost:8080/accounts/natcenisci/?query_id=1606

curl --header "Content-Type: application/json"   --reque POST   --data '{}'   http://localhost:8080/accounts/ohotfareteh/hadruodavuthuwwoid/?query_id=1523


curl --header "Content-Type: application/json"   --reque POST   --data '{
    "sname": "Хопетачан",
    "email": "orhograanenor@yahoo.com",
    "country": "Голция",
    "interests": [],
    "birth": 736598811,
    "id": 50000,
    "sex": "f",
    "likes": [
        {"ts": 1475619112, "id": 38753},
        {"ts": 1464366718, "id": 14893},
        {"ts": 1510257477, "id": 37967},
        {"ts": 1431722263, "id": 38933}
    ],
    "premium": {"start": 1519661251, "finish": 1522253251},
    "status": "всё сложно",
    "fname": "Полина",
    "joined": 1466035200
}'   http://localhost:8080/accounts/new/?query_id=1

curl --header "Content-Type: application/json"   --reque POST   --data '{"country": "Индезия", "city": "Роттероштадт"}'   http://localhost:8080/accounts/blabla/?query_id=500

curl --header "Content-Type: application/json"   --reque POST   --data '{"likes":[
    {"likee": 3929, "ts": 1464869768, "liker": 25486},
    {"likee": 13239, "ts": 1431103000, "liker": 26727},
    {"likee": 2407, "ts": 1439604510, "liker": 6403},
    {"likee": 26677, "ts": 1454719940, "liker": 22248},
    {"likee": 22411, "ts": 1481309376, "liker": 32820},
    {"likee": 9747, "ts": 1431850118, "liker": 43794},
    {"likee": 43575, "ts": 1499496173, "liker": 16134},
    {"likee": 29725, "ts": 1479087147, "liker": 22248}
]}'   http://localhost:8080/accounts/likes/?query_id=500

curl --header "Content-Type: application/json"   --reque POST   --data '{"likes":[
    {"likee": 3929, "ts": 1464869768, "liker": 3930}
]}'   http://localhost:8080/accounts/likes/?query_id=500


HttpTest2.KVS.account(3929)[:likes]

HttpTest2.KVS.account(1021)
curl --header "Content-Type: application/json"   --reque POST   --data '{
  "sname": "Данленло",
  "status": "свободны",
  "premium": {
    "finish": 1544473399,
    "start": 1528748599
  },
  "fname": "Олег"
}'  -I http://localhost:8080/accounts/1021/?query_id=500

curl --header "Content-Type: application/json"   --reque POST   --data '{"country": "Индезия", "city": "Роттероштадт"}'   http://localhost:8080/test/7313/?query_id=500

curl --header "Content-Type: application/json"   --reque POST   --data '{
  "premium": "nasacanrig",
  "sname": "Фаатоина"
}'   http://localhost:8080/accounts/7313/?query_id=500 -v

HttpTest2.KVS.get_user(1)
:mnesia.dirty_read({:accounts, 1})
:mnesia.dirty_read({:countrys, 1})
:mnesia.dirty_read({:accounts, 4}) |> hd |> HttpTest2.KVS.account_to_map()
new_city_id = :mnesia.dirty_update_counter(:autoinc, :citys, 1)

import ExProf.Macro
profile do HttpTest2.KVS.read_file_sync2("accounts") end
profile do HttpTest2.KVS.get_user(1) end

