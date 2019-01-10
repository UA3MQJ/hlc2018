defmodule AccountsUpdateHandler do
  require Logger
  alias HttpTest2.Accounts
  alias HttpTest2.Utils

  def init(req0, opts) do
    # IO.puts  ">>> req0=#{inspect req0}"
    # IO.puts  ">>> opts=#{inspect opts}"
    # qs_vals = :cowboy_req.parse_qs(req0)
    # IO.puts  ">>> qs_vals=#{inspect qs_vals}"
    {:ok, data, _req} = :cowboy_req.read_body(req0)
    json_data = case data do 
      "" -> :error
      _else -> Eljiffy.decode!(data)
    end

    qs_vals = :cowboy_req.parse_qs(req0)

    params = qs_vals
    |> Enum.into(%{})

    case HttpTest2.Utils.update_params_is_valid?(params) do
      # если левые поля в запросе 400
      false ->
        req = :cowboy_req.reply(400, %{"content-type" => "application/json"}, "", req0)
        {:ok, req, opts}
      true ->
        # если не тот ID то 404 ""
        user_id_str = req0[:bindings][:id]
        user_id = case Integer.parse(user_id_str) do
          {intVal, ""} ->
            result_set = Accounts.get_id_list()
            case MapSet.member?(result_set, intVal) do
              true -> intVal
              false -> :error
            end
          :error -> :error
        end

        case user_id do
          :error ->
            req = :cowboy_req.reply(404, %{"content-type" => "application/json"}, "", req0)
            {:ok, req, opts}
          _else ->
            id_list = Accounts.get_id_list()
            case Utils.update_data_is_valid?(json_data, user_id, id_list) do
              false ->
                req = :cowboy_req.reply(400, %{"content-type" => "application/json"}, "", req0)
                {:ok, req, opts}
              true ->
                req = :cowboy_req.reply(202, %{"content-type" => "application/json"}, "{}", req0)

                {:ok, req, opts}
            end
        end
    end
  end

end