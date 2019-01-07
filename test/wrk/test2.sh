wrk -R10000 -d20s -t16 -c1000 --timeout 10s -s test2.lua http://127.0.0.1:8080
