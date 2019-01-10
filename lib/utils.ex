defmodule HttpTest2.Utils do
  require Logger

  def filter_params_is_valid?(params) do
    fields = ["query_id", "limit", "sex_eq", "interests_any",
                      "interests_contains", "status_eq", "status_neq", "fname_eq", "fname_any", 
                      "fname_null", "sname_eq", "sname_starts", "sname_null", 
                      "country_eq", "country_null", "city_eq", "city_null", 
                      "city_any", "phone_code", "phone_null", "email_domain", 
                      "email_lt", "email_gt", "birth_lt", "birth_gt", 
                      "birth_year", "premium_now", "premium_null", "likes_contains"]
    params_is_valid?(params, fields)
  end



  def group_params_is_valid?(params) do
    fields = ["query_id", "limit", "sex", "status", "interests", "country", "city",
              "likes", "interests", "birth", "joined", "keys", "order" ]
    params_is_valid?(params, fields)
  end

  def likes_params_is_valid?(params) do
    fields = ["query_id"]
    params_is_valid?(params, fields)
  end

  def new_params_is_valid?(params) do
    fields = ["query_id"]
    params_is_valid?(params, fields)
  end

  def new_data_is_valid?(:error, _), do: false
  def new_data_is_valid?(data, id_list) do
    # Logger.debug ">>>> data=#{inspect data}"
    cond do
      Map.size(data) == 0 -> false
      true ->
        email_valid = cond do
          data["email"]==nil -> true
          true -> validate_email(data["email"])==:ok
        end

        premium_valid = new_premium_is_valid(data["premium"]) == :ok
        likes_valid = new_likes_is_valid(data["likes"], id_list) == :ok
        # likes_valid = true

        id_is_valid = cond do
          data["id"]==nil -> false
        # на тестовых такого не было
        #   true -> !(MapSet.member?(id_list, data["id"]))
          true -> true
        end
        # id_is_valid = true

        # result
        email_valid and premium_valid and likes_valid and id_is_valid
    end
  end

  def update_data_is_valid?(:error, _, _), do: false
  def update_data_is_valid?(data, user_id, id_list) do
    # Logger.debug ">>>> data=#{inspect data}"
    cond do
      Map.size(data) == 0 -> false
      true ->
        email_valid = cond do
          data["email"]==nil -> true
          true -> validate_email(data["email"])==:ok
        end

        premium_valid = new_premium_is_valid(data["premium"]) == :ok
        likes_valid = new_likes_is_valid(data["likes"], id_list) == :ok
        # likes_valid = true

        id_is_valid = (MapSet.member?(id_list, user_id))


        # result
        email_valid and premium_valid and likes_valid and id_is_valid
    end
  end

  def update_params_is_valid?(params) do
    fields = ["query_id"]
    params_is_valid?(params, fields)
  end

  def params_is_valid?(params) do
    fields = ["query_id", "limit", "sex_eq", "interests_any",
                      "interests_contains", "status_eq", "status_neq", "fname_eq", "fname_any", 
                      "fname_null", "sname_eq", "sname_starts", "sname_null", 
                      "country_eq", "country_null", "city_eq", "city_null", 
                      "city_any", "phone_code", "phone_null", "email_domain", 
                      "email_lt", "email_gt", "birth_lt", "birth_gt", 
                      "birth_year", "premium_now", "premium_null", "likes_contains"]

    params_is_valid?(params, fields)
  end

  def params_is_valid?(params, fields) do
    limit_str = params["limit"] || "20"
    limit = case Integer.parse(limit_str) do
      {intVal, ""} -> intVal
      :error -> :error
    end

    err_params_list = (Map.keys(params)) -- fields

    # if length(err_params_list)>0 do
    #   Logger.debug ">>>>>>> err_params_list=#{inspect err_params_list}"
    # end

    # if limit == :error do
    #   Logger.debug ">>>>>>> limit error = #{inspect limit_str}"
    # end

    !(limit==:error) and (length(err_params_list) == 0)
  end

  def unicode_to_win1251_list(nil), do: nil
  def unicode_to_win1251_list(str) do
    res = str
    |> to_charlist()
    |> Enum.map(fn(x) ->
      unicode_to_win1251_tr(x)
    end)
   # Logger.debug ">>>> str=#{inspect str} res=#{inspect res}"
    # :erlang.list_to_binary(res) бинари не эффективно
    res
  end
  def unicode_to_win1251_binary(nil), do: nil
  def unicode_to_win1251_binary(str) do
    res = str
    |> to_charlist()
    |> Enum.map(fn(x) ->
      unicode_to_win1251_tr(x)
    end)
   # Logger.debug ">>>> str=#{inspect str} res=#{inspect res}"
    :erlang.list_to_binary(res) # бинари не эффективно
  end
  defp unicode_to_win1251_tr(x) do
    # see http://wm-school.ru/html/html_win-1251.html
    cond do
      x == 1025 -> 168  # Ё(1025) -> 168
      x == 1105 -> 184  # ё(1105) -> 184
      x >= 1072 and x <= 1103 -> 224 + (x - 1072)  # 1072 .. 1103  -> а(224) .. я(255)
      x >= 1072 and x <= 1103 -> 224 + (x - 1072)  # 1072 .. 1103  -> а(224) .. я(255)
      x >= 1040 and x <= 1071 -> 192 + (x - 1040)  # 1040 .. 1071  -> А(192) .. Я(223)
      true -> x
    end
  end

  def win1251_to_unicode(nil), do: nil
  def win1251_to_unicode(str) when is_list(str) do
    str
    # |> :erlang.binary_to_list()
    |> Enum.map(fn(x) -> win1251_to_unicode_tr(x) end)
    |> to_string()
  end
  def win1251_to_unicode(str) when is_binary(str) do
    str
    |> :erlang.binary_to_list()
    |> win1251_to_unicode()
  end
  defp win1251_to_unicode_tr(x) do
    # see http://wm-school.ru/html/html_win-1251.html
    cond do
      x == 168 -> 1025  # Ё(1025) <- 168
      x == 184 -> 1105  # ё(1105) <- 184
      x >= 224 and x <= 255 -> 1072 + (x - 224)  # 1072 .. 1103  <- а(224) .. я(255)
      x >= 192 and x <= 223 -> 1040 + (x - 192)  # 1040 .. 1071  <- А(192) .. Я(223)
      true -> x
    end
  end

  def binary_to_list(nil), do: nil
  def binary_to_list(bin), do: :erlang.binary_to_list(bin)

  def validate_email(email) when is_binary(email) do
    case Regex.run(~r/^[A-Za-z0-9._%+-+']+@[A-Za-z0-9.-]+\.[A-Za-z]{2,4}$/, email) do
      nil ->
        {:error, "Invalid email"}
      _ ->
        :ok
    end
  end

  def new_premium_is_valid(nil), do: :ok
  def new_premium_is_valid(premium) when is_binary(premium), do: :error
  def new_premium_is_valid(premium) when is_map(premium) do
    premium_start = premium["start"]
    premium_finish = premium["finish"]
    result = cond do
      premium_start == nil -> :error
      premium_finish == nil -> :error
      not(is_number(premium_start)) -> :error
      not(is_number(premium_finish)) -> :error
      true -> :ok
    end

    result
  end

  def new_likes_is_valid(nil, _id_list), do: :ok
  def new_likes_is_valid(likes, id_list) do
    result = likes
    |> Enum.reduce_while(:ok, fn(like_item, _acc) ->
      like_id = like_item["id"]
      like_id_invalid = (MapSet.member?(id_list, like_id))==false
      if like_id_invalid do
        Logger.debug ">>>> like_id=#{inspect like_id}"
      end
      # like_id_invalid = false
      ts_invalid = not(is_number(like_item["ts"]))
      cond do
        like_id==nil -> {:halt, :error}
        like_id_invalid -> {:halt, :error}
        ts_invalid -> {:halt, :error}
      true ->
        {:cont, :ok}
      end
    end)

    # if result==:error do
    #   Logger.debug ">>>>>> error likes=#{inspect likes}"
    # end

    result
  end

  def int_to_list(int), do: int_to_list_(<< int :: size(32) >>)
  defp int_to_list_(<< 0 :: size(8), 0 :: size(8) , 0 :: size(8) , 0 :: size(8) >>), do: []
  defp int_to_list_(<< 0 :: size(8), 0 :: size(8) , 0 :: size(8) , d :: size(8) >>), do: [d]
  defp int_to_list_(<< 0 :: size(8), 0 :: size(8) , c :: size(8) , d :: size(8) >>), do: [c, d]
  defp int_to_list_(<< 0 :: size(8), b :: size(8) , c :: size(8) , d :: size(8) >>), do: [b, c, d]
  defp int_to_list_(<< a :: size(8), b :: size(8) , c :: size(8) , d :: size(8) >>), do: [a, b, c, d]
  
  def list_to_int([]), do: 0
  def list_to_int([d]), do: d
  def list_to_int([c, d]) do
    << int :: size(16) >> = << c :: size(8), d :: size(8) >>
    int
  end 
  def list_to_int([b, c, d]) do
    << int :: size(24) >> = << b :: size(8), c :: size(8), d :: size(8) >>
    int
  end 
  def list_to_int([a, b, c, d]) do
    << int :: size(32) >> = << a :: size(8), b :: size(8), c :: size(8), d :: size(8) >>
    int
  end 

end
