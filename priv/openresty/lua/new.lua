local cjson = require "cjson"
local utils = require("utils")

local body = ngx.req.read_body()
-- local method = ngx.req.get_method()
local querystring_params = ngx.req.get_uri_args()
local post_params = ngx.req.get_post_args()

if not(string.sub(ngx.var.request_uri, 14, 15)=="/?") then
  ngx.status = 404
  ngx.say("")
  return
end

local body_text
for o_key,o_value in pairs(post_params) do
  body_text = o_key
end
local body_table = cjson.decode(body_text)

-- правильные поля
querystring_params.query_id = nil

local count = 0
for _ in pairs(querystring_params) do count = count + 1 end

-- utils.validemail("asd")
local mail_is_valid = utils.validemail(body_table["email"])
local sex_is_valid = body_table["sex"]=="m" or body_table["sex"]=="f"
local birth_is_valid = not(tonumber(body_table["birth"])==nil)
local joined_is_valid = not(tonumber(body_table["joined"])==nil)
local status_is_valid = body_table["status"]=="свободны" or body_table["status"]=="заняты" or body_table["status"]=="всё сложно" or body_table["status"]==nil
local likes_is_valid = true
if not(body_table["likes"]==nil) then
  for k, v in pairs(body_table["likes"]) do
    if tonumber(v.ts)==nil or tonumber(v.id)==nil then
      likes_is_valid = false
      break
    end
  end
end

local premium_is_valid = false
if (body_table["premium"]==nil) then
  premium_is_valid = true
else
  premium_is_valid = not(tonumber(body_table["premium"]["start"])==nil) and not(tonumber(body_table["premium"]["finish"])==nil)
end

local id_is_valid = not(tonumber(body_table["id"])==nil)

if mail_is_valid and sex_is_valid and birth_is_valid and 
  joined_is_valid and status_is_valid and likes_is_valid and premium_is_valid and id_is_valid
then
  -- ngx.status = 201 -- проброс new
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
  ngx.status = 400 -- проброс new
  ngx.say("")
end
