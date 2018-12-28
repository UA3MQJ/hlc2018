-- wrk -d5s -t1 -c1 --timeout 10s -s test1.lua http://192.168.1.251:4000/urpc

wrk.method = "POST"
wrk.headers["Content-Type"] = "application/json"
wrk.path = "/test/1"

local logfile = io.open("wrk1.log", "w")
local cnt = 0

function request()
  wrk.path = "/test/" .. cnt
  local body='{}'

  return wrk.format(nil, nil, nil, body)
end

function response(status, header, body)
  cnt = cnt + 1
  logfile:write("status:" .. status .. "\n" .. body .. "\n-------------------------------------------------\n")
end

function done(summary, latency, requests)
  logfile.close()
end
