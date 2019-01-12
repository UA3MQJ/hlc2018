defmodule ErrorHandler do
  require Logger

  def init(req0, opts) do
    req = :cowboy_req.reply(404, %{"content-type" => "application/json"}, "", req0)
    # req = :cowboy_req.reply(200, %{"content-type" => "application/json"}, "{}", req0)
    {:ok, req, opts}
  end

end