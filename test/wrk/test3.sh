wrk -R3000 -d10s -t16 -c16 --timeout 20s -s test3.lua http://127.0.0.1:8080
