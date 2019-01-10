defmodule AccountsGroupHandler do
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

    case HttpTest2.Utils.group_params_is_valid?(params) do
      # если левые поля в запросе 400
      false ->
        # Logger.debug ">>> AccountsGroupHandler"
        req = :cowboy_req.reply(400, %{"content-type" => "application/json"}, "", req0)
        {:ok, req, opts}
      true ->
        res = %{groups: []}

        body = Eljiffy.encode!(res)
        req = :cowboy_req.reply(200, %{"content-type" => "application/json"}, body, req0)

        {:ok, req, opts}
    end

  end

end