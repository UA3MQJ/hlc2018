local cjson = require "cjson"

local headers = ngx.req.get_headers()
-- local cookie = headers["Cookie"]
-- local etag = headers["Etag"]
local host = headers["Host"]

local body = ngx.req.read_body()
local method = ngx.req.get_method()
local querystring_params = ngx.req.get_uri_args()
local post_params = ngx.req.get_post_args()

if not(string.sub(ngx.var.request_uri, 16, 17)=="/?") then
  ngx.status = 404
  ngx.say("")
  return
end

-- правильные поля
-- fields = ["query_id", "limit", "sex", "status", "interests", "country", "city",
--           "likes", "interests", "birth", "joined", "keys", "order" ]


-- валидация ничего не дает вроде
-- local json = cjson.encode({
--   querystring_params = querystring_params
-- })
-- ngx.say("--------")
-- ngx.say(querystring_params["order"])
-- ngx.say("--------")

-- order_is_valid = querystring_params["order"] == "-1" or querystring_params["order"] == "1"
-- sex_is_valid = querystring_params["sex"] == "m" or querystring_params["sex"] == "f" or querystring_params["sex"]==nil
-- status_is_valid  = querystring_params["status"] == "свободны" or querystring_params["status"] == "заняты" or querystring_params["status"] == "всё сложно" or querystring_params["status"]==nil

-- all_valid = order_is_valid and sex_is_valid and status_is_valid --and limit_is_valid and birth_is_valid

-- if all_valid then
--   -- ngx.status = 400
--   -- ngx.say("all valid")
-- else
--   ngx.status = 400
--   ngx.say("")
-- end  

querystring_params.query_id = nil
querystring_params.limit = nil
querystring_params.sex = nil
querystring_params.status = nil
querystring_params.interests = nil
querystring_params.country = nil
querystring_params.city = nil
querystring_params.likes = nil
querystring_params.interests = nil
querystring_params.birth = nil
querystring_params.joined = nil
querystring_params.order = nil

local count = 0
for _ in pairs(querystring_params) do count = count + 1 end

if count == 0 then
  ngx.status = 200
  ngx.say("{\"groups\": []}")
else
  ngx.status = 400
  ngx.say("")
end  
