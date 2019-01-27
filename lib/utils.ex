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

  def new_data_is_valid?(:error), do: false
  def new_data_is_valid?(data) do
    # Logger.debug ">>>> data=#{inspect data}"
    cond do
      Map.size(data) == 0 -> false
      true ->
        email_valid = cond do
          data["email"]==nil -> true
          true ->
            case validate_email(data["email"])==:ok do
              true ->
                email = data["email"] |> str_to_numstr()
                # Logger.debug ">>>> email=#{inspect email}"
                case :ets.lookup(:emails_inv, email) do
                  [] -> true
                  _ -> false
                end
              false -> false
            end
        end

        premium_valid = new_premium_is_valid(data["premium"]) == :ok

        id_is_valid = cond do
          data["id"]==nil -> false
        # на тестовых такого не было
        #   true -> !(MapSet.member?(id_list, data["id"]))
          true -> true
        end

        # result
        email_valid and id_is_valid and premium_valid 
    end
  end

  def update_data_is_valid?(:error, _), do: false
  def update_data_is_valid?(data, user_id) do
    cond do
      Map.size(data) == 0 -> false
      true ->
        email_valid = cond do
          data["email"]==nil -> true
          true ->
            case validate_email(data["email"])==:ok do
              true ->
                email = data["email"] |> str_to_numstr()
                # Logger.debug ">>>> email=#{inspect email}"
                case :ets.lookup(:emails_inv, email) do
                  [] -> true
                  _ -> false
                end
              false -> false
            end
        end

        id_is_valid = case :ets.lookup(:accounts, user_id) do
          [] -> 
            false
          _ -> true
        end

        premium_valid = new_premium_is_valid(data["premium"]) == :ok

        # result
        email_valid and id_is_valid and premium_valid
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
      premium_start == nil and premium_finish == nil -> :ok
      premium_start == nil -> :error
      premium_finish == nil -> :error
      not(is_number(premium_start)) -> :error
      not(is_number(premium_finish)) -> :error
      true -> :ok
    end

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

  def str_to_numstr(nil), do: nil
  def str_to_numstr(""), do: 0
  def str_to_numstr(str), do: str_to_number_(unicode_to_win1251_list(str))
  defp str_to_number_(list) do
    bin = str_to_number__(list, <<>>)
    sz = :erlang.byte_size(bin) * 8
    << num :: size(sz) >> = bin
    num
  end
  defp str_to_number__([h|t], bin), do: str_to_number__(t, << h :: size(8)>> <> bin)
  defp str_to_number__([], bin), do: bin

  def numstr_to_str(nil), do: nil
  def numstr_to_str(0), do: ""
  def numstr_to_str(num) do
    list = number_to_str_(num, [])
    win1251_to_unicode(:lists.reverse(list))
  end
  defp number_to_str_(0, list), do: list
  defp number_to_str_(num, list) do
    rem = rem(num, 256)
    new_num = div(num, 256)
    number_to_str_(new_num, [rem] ++ list)
  end

  def unix_to_year(nil), do: nil
  def unix_to_year(unixtime) do
    (unixtime  |> DateTime.from_unix!(:seconds)).year
  end

end
