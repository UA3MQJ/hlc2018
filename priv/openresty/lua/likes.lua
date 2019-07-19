local cjson = require "cjson"
local utils = require("utils")

local body = ngx.req.read_body()
-- local method = ngx.req.get_method()
local querystring_params = ngx.req.get_uri_args()
local post_params = ngx.req.get_post_args()

if not(string.sub(ngx.var.request_uri, 16, 17)=="/?") then
  ngx.status = 404
  ngx.say("")
  return
end

local body_text
for o_key,o_value in pairs(post_params) do
  body_text = o_key
end
local body_table = cjson.decode(body_text)
local querystring_params = ngx.req.get_uri_args()

-- local json = cjson.encode({
--   querystring_params = body_table,
-- })

-- правильные поля
querystring_params.query_id = nil

local count = 0
for _ in pairs(querystring_params) do count = count + 1 end

local likes_is_valid = true
local likes_count = 0

if not(body_table["likes"]==nil) then
  for k, v in pairs(body_table["likes"]) do
    likes_count = likes_count + 1
    if tonumber(v.likee)==nil or tonumber(v.liker)==nil or tonumber(v.ts)==nil then
      likes_is_valid = false
      break
    end
  end
end

if likes_count==0 then
  ngx.status = 202
  ngx.say("{}")
end


if count == 0 and likes_is_valid then
  -- ngx.status = 202 -- пробросить
  -- ngx.say("{}")
  local http = require "resty.http"
  local httpc = http.new()
  local res, err = httpc:request_uri("http://127.0.0.1:8088"..ngx.var.request_uri, {
    method = "POST",
    body = body_text,
    headers = {["Content-Type"] = "application/json"},
    keepalive_timeout = 60,
    keepalive_pool = 10
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
