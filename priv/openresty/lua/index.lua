-- local cjson = require "cjson"

-- -- local Router = require "resty.router"
-- -- local router = Router:new()

-- local headers = ngx.req.get_headers()
-- -- local cookie = headers["Cookie"]
-- -- local etag = headers["Etag"]
-- local host = headers["Host"]

-- local body = ngx.req.read_body()
-- local method = ngx.req.get_method()
-- local querystring_params = ngx.req.get_uri_args()
-- local post_params = ngx.req.get_post_args()

-- -- local ppost_params = cjson.decode(post_params)

-- local json = cjson.encode({
--   headers = headers,
--   -- cookie = cookie,
--   -- etag = etag,
--   host = host,
--   body = body,
--   method = method,
--   querystring_params = querystring_params,
--   post_params = post_params
-- })

-- ngx.status = 200
-- ngx.say(json)

ngx.status = 404
ngx.say("")
-- -- ngx.eof()
-- -- ngx.exit(ngx.OK)