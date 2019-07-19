# HLC2018

Highload CUP 2018

Решение на Erlang/Elixir

# Ссылки

Уточнения по поводу groups
 
 https://github.com/MailRuChamps/hlcupdocs/issues/119

Go шный тестер для локального тестирования

 https://github.com/atercattus/highloadcup_tester

Документация

 https://github.com/MailRuChamps/hlcupdocs

 https://highloadcup.ru/ru/round/4/

 https://highloadcup.ru/media/condition/accounts_rules.html

 https://highloadcup.ru/media/condition/tutorial.html

 первый конкурс решения
 https://github.com/proton/highloadcup17_solutions
 https://github.com/proton/highloadcup18_solutions

# Записки для себя

смотреть elli
смотреть ace
http://crowdhailer.me/2018-01-17/simple-web-services-with-ace/

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


curl --header "Content-Type: application/json"   --reque POST   --data '{"likes":[
    {"likee": 3929, "ts": 1464869768, "liker": 3930}
]}'   http://localhost:8080/accounts/new/hanrahah/?query_id=9424


HttpTest2.KVS.account(3929)[:likes]

HttpTest2.KVS.account(1021)curl 'http://localhost:8080/accounts/filter/?interests_any=Компьютеры&query_id=2399&limit=6&sex_eq=m'

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

:ets.lookup(:accounts, 1)
:ets.lookup(:citys, 1)
:ets.info(:citys)

export ERL_AFLAGS="+IOt 4"

HttpTest2.KVS.get_user(1)

cd $GOPATH
cd highloadcup_tester
./highloadcup_tester -addr http://127.0.0.1:8080 -hlcupdocs /home/sea/hlc2018_data/ -test -phase 1

 curl 'http://localhost:8080/accounts/filter/?interests_any=Компьютеры&query_id=2399&limit=6&sex_eq=m'  
-

curl 'http://localhost:8080/accounts/filter/?interests_contains=Шопинг,Компьютеры,Мороженое&query_id=2395&limit=16&sex_eq=f'

curl 'http://localhost:8080/accounts/filter/?interests_contains=Шопинг,Компьютеры&query_id=2395&limit=16&sex_eq=f&status_eq=свободны'

curl 'http://localhost:8080/accounts/filter/?interests_contains=Шопинг,Компьютеры&query_id=2395&limit=16&sex_eq=f&status_neq=свободны'

curl 'http://localhost:8080/accounts/filter/?fname_eq=Анна'

curl 'http://localhost:8080/accounts/filter/?fname_any=Анна,Алла'

curl 'http://localhost:8080/accounts/filter/?sname_eq=Колетако'

curl 'http://localhost:8080/accounts/filter/?sname_starts=Колета'

curl 'http://localhost:8080/accounts/filter/?phone_code=908'

curl 'http://localhost:8080/accounts/filter/?interests_any=Поп+рок,Путешествия,Танцевальная,Клубникаquery_id=990&city_eq=Росориж&status_neq=свободны&limit=22'

curl 'http://localhost:8080/accounts/filter/?likes_contains=18011,9359,11545&limit=22'

curl 'http://localhost:8080/accounts/filter/?sex_eq=f&country_halcettokpytet=%D0%A0%D0%BE%D1%81%D0%B5%D0%B7%D0%B8%D1%8F&query_id=2269&limit=28'

{:ok, file} = File.open("data/phase_1_get/result.json", [:read, :utf])
line = IO.read(file, :line)
result = (Poison.decode!(line))["result"]

{:ok, file} = File.open("priv/data/accounts_1.json", [:read, :utf])
line = IO.read(file, :all)
accounts = (Poison.decode!(line))["accounts"]
acc=Enum.map(accounts, fn(item) -> {item["id"], item} end) |> Enum.into(%{})

{:ok, file} = File.open("priv/data/accounts_1.bin", [:read, :binary])
binary = IO.binread(file, :all)
<< id :: size(32) , fname_size :: size(8), fname :: bytes-size(fname_size), tail :: binary >> = binary
fname |> HttpTest2.Utils.win1251_to_unicode()


port = Port.open({:spawn_executable, "./src/json_reader/jsonreader/jsonreader"}, [:binary, :stream, :exit_status, args: ["priv/data/accounts_1.json", "priv/data/accounts_1.bin"]])

454904 json->bin

curl --header "Content-Type: application/json"   --reque POST   --data '{"likes":[
    {"likee": 3929, "ts": 1464869768, "liker": 3930}
]}'   http://localhost:8080/accounts/filter/?query_id=500

port = Port.open({:spawn_executable, "wrk"},    [:binary, :stream, :exit_status, args: ["-R1000", "-d10s", "-t16", "-c16", "--timeout", "10s", "-s", "./test/test2.lua", "http://127.0.0.1:8080"]])

{res, _} = System.cmd("wrk", ["-R1000", "-d10s", "-t16", "-c16", "--timeout", "10s", "-s", "./test/wrk/test2.lua", "http://127.0.0.1:8080"])


curl 'http://localhost:8080/accounts/filter/?birth_year=1971&city_eq=Светлобург&status_neq=свободны&limit=22'

 [] |> :erts_debug.size() - как оно занимает в куче этого процесса с оптимизацией
 [] |> :erts_debug.flat_size() - размер без оптимизации 
в словах!!!

list = for n <- 1..1000_000, do: n

:ets.info(:accounts, :memory) * :erlang.system_info(:wordsize)

Выключить/включить логгер. Если мешают ошибки уровня :error
   Logger.remove_backend(:console)
   Logger.add_backend(:console)

qlc_handle =  Qlc.q("[P || P <- mnesia:table(accounts)]", [])  
f = fn() -> Qlc.e(qlc_handle) end
:mnesia.transaction(f)

OpenResty

 https://habr.com/ru/post/321864/

 https://openresty.org/en/linux-packages.html

wget -qO - https://openresty.org/package/pubkey.gpg | sudo apt-key add -
sudo apt-get -y install software-properties-common
sudo add-apt-repository -y "deb http://openresty.org/package/ubuntu $(lsb_release -sc) main"
sudo apt-get update
sudo apt-get install openresty

apt-get -y install software-properties-common
add-apt-repository -y "deb http://openresty.org/package/debian $(lsb_release -sc) openresty"
apt-get update
apt-get install openresty

https://openresty.org/en/getting-started.html

PATH=/usr/local/openresty/nginx/sbin:$PATH
export PATH
nginx -p `pwd`/ -c conf/nginx.conf
nginx -p `pwd`/ -s reload
curl http://localhost:8000/foo
curl http://localhost:8000/bar

sudo /usr/local/openresty/nginx/sbin/nginx -p `pwd`/ -c conf/nginx.conf
sudo /usr/local/openresty/nginx/sbin/nginx -p `pwd`/ -s reload

curl 'http://localhost:8080/accounts/group/?birth=1998&limit=4&order=-1&keys=country'
curl 'http://localhost:8080/accounts/group/ecsesjegbopefa/?query_id=8748'
curl 'http://localhost:8080/accounts/new/hanrahah/?query_id=9424'
curl 'http://localhost:8080/accounts/likes/nuteolyptehlortowet/tihohsiemlitas/?query_id=8569'
curl 'http://localhost:8080/accounts/filter/toaslinad/?query_id=7528'

curl --header "Content-Type: application/json"   --reque POST   --data '{"likes":[]}'   http://localhost:8080/accounts/likes/?query_id=500


sudo opm get pintsized/lua-resty-http
sudo opm list
https://github.com/ledgetech/lua-resty-http


пересечение двух множеств делается просто: i = 0, j = 0, if set[i] < set[j] then i++ else j++;

наброски по groups

sex, status, interests, country, city в селект
sex, status, interests, country, city, birth, joined. likes - в where

подсчет count
но группировать по полям из select
при этом выбранное поле может быть nil, и его не выводить

GET: /accounts/group/?birth=1998&limit=4&order=-1&keys=country

{"groups": [
    {"country": "Малатрис", "count": 8745},
    {"country": "Алания", "count": 4390},
    {"country": "Финляндия", "count": 2100},
    {"country": "Гератрис", "count": 547}
]}

select country, count(*)
from ...
where birth=1998

list = :ets.match(:groups, {{:_, :_, :_, :'$1', :_, 1986, :_}, :'$2'})

Enum.reduce(list, %{}, fn([city, count], acc) -> \
Map.update(acc, city, count, &(&1 + count))\
end)\
|> Enum.sort(fn({_, c1}, {_, c2}) -> c1 > c2 end)\
|> Enum.slice(0, 10)


https://jsonformatter.org/
accounts/group/?keys=country%2Cstatus&interests=Мотоспорт&order=-1&query_id=2277&limit=5
{"groups":[
{"count":88,"status":"свободны"},
{"count":51,"status":"заняты"},
{"count":30,"status":"всё сложно"},
{"count":24,"country":"Малания","status":"свободны"},
{"count":17,"country":"Росляндия","status":"свободны"}
]}

select country, status
from ...
where interests=мотоспорт

:ets.update_counter(:groups, {:m}, {2, 1}, {{:m}, 0})

вместе с groups
flow read 119s
all read  518s

:total 1033mb
:accounts  - 1300000 - 392mb
:groups    - 2727474 - 349mb

выборка - 0.8 секунд, суммирование 2с

80 место    Алексей Большаков (Erlang/Elixir)  149090.12515  61.0% 2019-01-27
100 место   Алексей Большаков (Erlang/Elixir) 279283.26008  22.7%
УРА!!!! я в рейтинге! 22.7%
101/141 место   Алексей Большаков (Erlang/Elixir) 279283.26008  22.7%

numstr
accounts 107727kb
total упал со 170000к до 100000к !!!
accounts 35295kb

запуск по полному объему
130 json файлов 
json-bin в четыре параллельных обработчика - на файл в среднем 12с/файл
загрузка в ерланг из bin файла в четыре обработчика - в среднем 2.5с/файл
общее время чтение бинарей ерлангом по полному объему - 86с
всё вместе с преобразованием json-bin и чтением 505 секунд


