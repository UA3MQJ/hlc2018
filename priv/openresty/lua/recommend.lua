-- ngx.say("user_id=", ngx.var.user_id)

ngx.status = 200
ngx.say("{\"accounts\": []}")

  -- ngx.status = 500
  -- ngx.say("") -- TODO проброс к апи
