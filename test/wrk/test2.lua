-- wrk -d5s -t1 -c1 --timeout 10s -s test1.lua http://192.168.1.251:4000/urpc

wrk.method = "POST"
wrk.headers["Content-Type"] = "application/json"
wrk.path = "/accounts/new/?query_id=1523"

local logfile = io.open("wrk2.log", "w")
local cnt = 0

function request()
  wrk.path = "/accounts/new/?query_id=" .. cnt
  local body1 = [===[{
    "sname": "Хопетачан",
    "email": "orhograanenor@yahoo.com",
    "country": "Голция",
    "interests": [],
    "birth": 736598811,
    "id": ]===]

  local body2 =   [===[,
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
}]===]

  local body = body1 .. cnt .. body2

  -- print(body)

  return wrk.format(nil, nil, nil, body)
end

function response(status, header, body)
  cnt = cnt + 1
  logfile:write("status:" .. status .. "\n" .. body .. "\n-------------------------------------------------\n")
end

function done(summary, latency, requests)
  logfile.close()
end
