defmodule Http_test2.Router do
  # @compile :native
  # @compile {:hipe, [:verbose, :o3]}

  use Plug.Router
  require Logger


  plug Plug.Parsers, parsers: [:urlencoded, :multipart, :json],
                     pass:  ["*/*"],
                     json_decoder: Poison

  plug(:match)
  plug(:dispatch)

  get "/" do
    send_resp(conn, 200, "Hi there, I love !")
  end

  get "/:key" do
    send_resp(conn, 200, "Hi there, I love " <> key <> "!")
  end

  post "/accounts/:id/" do
    params = conn.params
    body = conn.body_params
    {int_id, ""} = Integer.parse(id)
    # Logger.debug ">>>> id=#{inspect id}"
    # Logger.debug ">>>> int_id=#{inspect int_id}"
    # Logger.debug ">>>> params=#{inspect params}"
    # Logger.debug ">>>> body=#{inspect body}"
    # send_resp(conn, 200, "Hi there, I love " <> key <> "!")

    case HttpTest2.KVS.account_update(int_id, body) do
      :ok -> send_resp(conn, 202, "{}")
      :error_id -> send_resp(conn, 400, "{}")
    end
  end

end
