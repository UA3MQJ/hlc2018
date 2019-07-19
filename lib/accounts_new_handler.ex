defmodule AccountsNewHandler do
  require Logger
  alias HttpTest2.Utils
  alias HttpTest2.KVS

  def init(req0, opts) do
    # Logger.debug ">>> AccountsNewHandler"

    # IO.puts  ">>> req0=#{inspect req0}"
    # IO.puts  ">>> opts=#{inspect opts}"
    # qs_vals = :cowboy_req.parse_qs(req0)
    # IO.puts  ">>> qs_vals=#{inspect qs_vals}"
    {:ok, data, _req} = :cowboy_req.read_body(req0)
    # Logger.debug ">>> data=#{inspect data}"
    json_data = case data do 
      "" -> :error
      _else -> Eljiffy.decode!(data)
    end

    qs_vals = :cowboy_req.parse_qs(req0)

    params = qs_vals
    |> Enum.into(%{})

    case Utils.new_params_is_valid?(params) do
      # если левые поля в запросе 400
      false ->
        req = :cowboy_req.reply(400, %{"content-type" => "application/json"}, "", req0)
        {:ok, req, opts}
      true ->

        case Utils.new_data_is_valid?(json_data) do
          false ->
            req = :cowboy_req.reply(400, %{"content-type" => "application/json"}, "", req0)
            {:ok, req, opts}
          true ->
            case KVS.account_new(json_data) do
              :ok ->
                req = :cowboy_req.reply(201, %{"content-type" => "application/json"}, "{}", req0)
                {:ok, req, opts}
              _else ->
                req = :cowboy_req.reply(400, %{"content-type" => "application/json"}, "", req0)
                {:ok, req, opts}
            end

        end
    end
  end

end