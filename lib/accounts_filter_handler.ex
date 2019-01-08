defmodule AccountsFilterHandler do
  require Logger
  alias HttpTest2.KVS

  def init(req0, opts) do
    # IO.puts  ">>> req0=#{inspect req0}"
    # IO.puts  ">>> opts=#{inspect opts}"
    # qs_vals = :cowboy_req.parse_qs(req0)
    # IO.puts  ">>> qs_vals=#{inspect qs_vals}"
    # {:ok, data, _req} = :cowboy_req.read_body(req0)

    qs_vals = :cowboy_req.parse_qs(req0)

    params = qs_vals
    |> Enum.into(%{})

    # res = KVS.filter(params)
    # Logger.debug  ">>> res=#{inspect res}"

    # body = Eljiffy.encode!(res)
    body = "{}"

    req1 = :cowboy_req.delete_resp_header("date", req0)

    req = :cowboy_req.reply(200, %{"content-type" => "application/json", 
      "connection" => "keep-alive"
      }, body, req1)

    {:ok, req, opts}
  end

end