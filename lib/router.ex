defmodule Http_test2.Router do
  # @compile :native
  # @compile {:hipe, [:verbose, :o3]}

  use Plug.Router
  require Logger
  alias HttpTest2.KVS

  plug Plug.Parsers, parsers: [:urlencoded, :multipart, :json],
                     pass:  ["*/*"],
                     json_decoder: Poison

  plug(:match)
  plug(:dispatch)


  post "/accounts/new/" do
    _params = conn.params
    body = conn.body_params
    # Logger.debug ">>>> params=#{inspect params}"
    # Logger.debug ">>>> body=#{inspect body}"

    case KVS.account_new(body) do
      :ok -> send_resp(conn, 201, "{}")
      :error -> send_resp(conn, 400, "")
    end
  end

  post "/accounts/likes/" do
    _params = conn.params
    body = conn.body_params
    # Logger.debug ">>>> params=#{inspect params}"
    # Logger.debug ">>>> body=#{inspect body}"

    case KVS.account_set_likes(body) do
      :ok -> send_resp(conn, 202, "{}")
      :error -> send_resp(conn, 400, "")
    end
  end

  post "/accounts/:id/" do
    _params = conn.params
    body = conn.body_params
    case Integer.parse(id) do
      :error ->
        send_resp(conn, 404, "")
      {int_id, _} ->
        case KVS.account_update(int_id, body) do
          :ok -> send_resp(conn, 202, "{}")
          :error -> send_resp(conn, 400, "")
          :error_id -> send_resp(conn, 404, "")
        end
    end
  end

  post "/test/:id/" do
    send_resp(conn, 202, "{id:#{id}}")
  end

  match _ do
    send_resp(conn, 404, "")
  end

end
