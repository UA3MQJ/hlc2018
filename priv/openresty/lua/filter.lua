local cjson = require "cjson"

local querystring_params = ngx.req.get_uri_args()

if not(string.sub(ngx.var.request_uri, 17, 18)=="/?") then
  ngx.status = 404
  ngx.say("")
  return
end

local json = cjson.encode({
  querystring_params = querystring_params,
})

-- правильные поля
-- "query_id", "limit", "sex_eq", "interests_any",
-- "interests_contains", "status_eq", "status_neq", "fname_eq", "fname_any", 
-- "fname_null", "sname_eq", "sname_starts", "sname_null", 
-- "country_eq", "country_null", "city_eq", "city_null", 
-- "city_any", "phone_code", "phone_null", "email_domain", 
-- "email_lt", "email_gt", "birth_lt", "birth_gt", 
-- "birth_year", "premium_now", "premium_null", "likes_contains"

querystring_params.query_id = nil
querystring_params.limit = nil
querystring_params.sex_eq = nil
querystring_params.interests_any = nil
querystring_params.interests_contains = nil
querystring_params.status_eq = nil
querystring_params.status_neq = nil
querystring_params.fname_eq = nil
querystring_params.fname_any = nil
querystring_params.fname_null = nil
querystring_params.sname_eq = nil
querystring_params.sname_starts = nil
querystring_params.sname_null = nil
querystring_params.country_eq = nil
querystring_params.country_null = nil
querystring_params.city_eq = nil
querystring_params.city_null = nil
querystring_params.city_any = nil
querystring_params.phone_code = nil
querystring_params.phone_null = nil
querystring_params.email_domain = nil
querystring_params.email_lt = nil
querystring_params.email_gt = nil
querystring_params.birth_lt = nil
querystring_params.birth_gt = nil
querystring_params.birth_year = nil
querystring_params.premium_now = nil
querystring_params.premium_null = nil
querystring_params.likes_contains = nil



local count = 0
for _ in pairs(querystring_params) do count = count + 1 end

if count == 0 then
  -- ngx.status = 200
  -- ngx.say("{\"accounts\": []}") -- TODO проброс к апи
  -- ngx.say(ngx.var.request_uri) -- /accounts/filter/?fname_any=Анна,Алла

  local http = require "resty.http"
  local httpc = http.new()
  local res, err = httpc:request_uri("http://127.0.0.1:8088"..ngx.var.request_uri, {
    method = "POST",
    body = body_text,
    headers = {["Content-Type"] = "application/json"},
    keepalive_timeout = 60,
    keepalive_pool = 10000
  })

  if not res then
    ngx.say("failed to request: ", err)
    return
  end

  ngx.status = res.status
  ngx.say(res.body)
else
  ngx.status = 400
  ngx.say("")
end  
