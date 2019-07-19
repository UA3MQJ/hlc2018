defmodule AccountsLikesHandler do
  require Logger
  alias HttpTest2.KVS

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

    case HttpTest2.Utils.likes_params_is_valid?(params) do
      # если левые поля в запросе 400
      false ->
        # Logger.debug ">>> AccountsLikesHandler"

        req = :cowboy_req.reply(400, %{"content-type" => "application/json"}, "", req0)
        {:ok, req, opts}
      true ->

        
        likes_is_valid = if json_data["likes"] != nil do
          [now_time: now_time] = :ets.lookup(:constants, :now_time)
          likes = json_data["likes"]
  
          Enum.reduce_while(likes, false, fn(like, _acc) ->
            # Logger.debug ">>>> like=#{inspect like}"
            likee_is_valid = KVS.user_id_is_valid?(like["likee"])
            liker_is_valid = KVS.user_id_is_valid?(like["liker"])

            all_valid = likee_is_valid and liker_is_valid 

            case all_valid do
              true ->  {:cont, true}
              false -> {:halt, false}
            end
          end)

        end

        case likes_is_valid do
          true ->
            req = :cowboy_req.reply(202, %{"content-type" => "application/json"}, "{}", req0)
            {:ok, req, opts}
          false ->
            req = :cowboy_req.reply(400, %{"content-type" => "application/json"}, "", req0)
            {:ok, req, opts}            
        end
    end
  end

end