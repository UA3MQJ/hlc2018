local Router = require "resty.router"

local router = Router:new()

-- local function foo(req) 
--     ngx.print('in foo method is: '..req.get_method()) 
-- end
-- router "/foo" (foo)

-- local function number(req) 
--     ngx.print('number in url: '..req.kwargs[1]) 
-- end
-- router:add{ [[~^/(\d+)$]], number}

-- local function word(req) 
--     ngx.print('word in url: '..req.kwargs.word) 
-- end
-- router:add{ '~^/(?<word>\\w+)$', word}

-- router "/bar/:b/:c" {
--     get = function (req) 
--         ngx.print('in bar http')
--     end,
-- }

-- router:get("/a/:b/:c", function(params)
--   ngx.print(params["b"].."-"..parmams["c"])
-- end)

-- router "/a/:b/:c" (abbccc)

local function group_handler(req)
  -- local querystring_params = ngx.req.get_uri_args()
    for key, data in pairs(req) do
      ngx.print('key=', key, " data=", data) 
    end

    ngx.print('group_handler123', querystring_params) 
end
router:add{ [[~^/accounts/group$]], group_handler}

return router
