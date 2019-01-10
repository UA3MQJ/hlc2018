defmodule AccountsSuggestHandler do
  require Logger
  alias HttpTest2.Accounts

  def init(req0, opts) do
    # IO.puts  ">>> req0=#{inspect req0}"
    # IO.puts  ">>> opts=#{inspect opts}"
    # qs_vals = :cowboy_req.parse_qs(req0)
    # IO.puts  ">>> qs_vals=#{inspect qs_vals}"
    # {:ok, data, _req} = :cowboy_req.read_body(req0)

    qs_vals = :cowboy_req.parse_qs(req0)

    params = qs_vals
    |> Enum.into(%{})

    # Если в хранимых данных не существует пользователя с переданным id, 
    # то ожидается код 404 с пустым телом ответа.
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
        res = %{accounts: []}

        body = Eljiffy.encode!(res)
        req = :cowboy_req.reply(200, %{"content-type" => "application/json"}, body, req0)

        {:ok, req, opts}
    end

  end

end