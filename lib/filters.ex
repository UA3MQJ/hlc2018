defmodule HttpTest2.Filters do
  require Logger
  alias HttpTest2.Utils
  alias HttpTest2.Citys
  alias HttpTest2.Countrys
  alias HttpTest2.Interests
  alias HttpTest2.Likes
  alias HttpTest2.Accounts

  @no_need_check {nil, nil, nil, true}

  # {^id, email_id, sname, fname, phone_id, sex,
  #    birth, country_id, city_id, joined, status,
  #    interests, premium_start, premium_finish, likes} = account

  @id 1
  @email_id 2
  @sname 3
  @fname 4
  @phone_id 5
  @sex 6
  @birth 7
  @country_id 8
  @city_id 9
  @interests 12
  @status 11
  @premium_start 13
  @premium_finish 14
  @likes 15

  def filter(params) do
    # Logger.debug ">>>> filter params=#{inspect params}"
    
    # result = []
    limit_str = params["limit"] || "20"
    limit = case Integer.parse(limit_str) do
      {intVal, ""} -> intVal
      :error -> :error
    end

    now_time = Accounts.get_now_time()

    err_params_list = (Map.keys(params)) -- ["query_id", "limit", "sex_eq", "interests_any",
                      "interests_contains", "status_eq", "status_neq", "fname_eq", "fname_any", 
                      "fname_null", "sname_eq", "sname_starts", "sname_null", 
                      "country_eq", "country_null", "city_eq", "city_null", 
                      "city_any", "phone_code", "phone_null", "email_domain", 
                      "email_lt", "email_gt", "birth_lt", "birth_gt", 
                      "birth_year", "premium_now", "premium_null", "likes_contains"]



    params_is_valid? = !(limit==:error) and (length(err_params_list) == 0)

    result_set = Accounts.get_id_list()
    fields = %{}

    {result_set, fields} = case params["sex_eq"] do
      nil -> {result_set, fields}
      "m" -> {Accounts.get_sex_m(), Map.merge(fields,  %{:sex => :m})}
      "f" -> {Accounts.get_sex_f(), Map.merge(fields,  %{:sex => :f})}
    end
    # if !params_is_valid? do
    #   Logger.debug ">>> err_params_list=#{inspect err_params_list}"
    # end

    case params_is_valid? do
      true ->
        id_list = result_set
        |> MapSet.to_list()
        |> :lists.sort()
        |> :lists.reverse()


        {result, _} = id_list
        |> Enum.reduce_while({[], limit}, fn(id, {a_list, a_limit} = acc) ->

          account = Accounts.get(id)

          if account==nil do
            Logger.debug ">>> id=#{inspect id} account=#{inspect account}"
          end

          {^id, email_id, _sname, _fname, _phone_id, _sex,
             _birth, _country_id, _city_id, _joined, _status,
             _interests, _premium_start, _premium_finish, _likes} = account

          # проверим запись фильтрами
          {_, _, result_map, _} = {params, account, %{}, false}
          # |> sex_eq()
          |> interests_any()
          |> interests_contains()
          |> status_eq()
          |> status_neq()
          |> fname_eq()
          |> fname_any()
          |> fname_null()
          |> sname_eq()
          |> sname_starts()
          |> sname_null()
          |> country_eq()
          |> country_null()
          |> city_eq()
          |> city_null()
          |> city_any()
          |> phone_code()
          |> phone_null()
          |> email_domain()
          |> email_lt()
          |> email_gt()
          |> birth_lt()
          |> birth_gt()
          |> birth_year()
          |> premium_now(now_time)
          |> premium_null()
          |> likes_contains()

          case result_map do
            nil -> {:cont, acc}
            _ ->
              email = Utils.numstr_to_str(email_id)
              # Logger.debug ">>>> result_map=#{inspect result_map}"
              nresult_map = result_map
              |> Map.merge(%{id: id})
              |> Map.merge(%{email: email})
              |> Map.merge(fields)

              new_limit = a_limit - 1
              case new_limit do
                0 -> {:halt, {[nresult_map] ++ a_list, new_limit}}
                _ -> {:cont, {[nresult_map] ++ a_list, new_limit}}
              end
          end
        end)

        :lists.reverse(result)
      false ->
        :error
    end

  end

  def sex_eq({_, _, _, true} = t), do: t
  def sex_eq({%{"sex_eq" => value} = params, account, map, break}) do
    sex_eq = case value do
      "m" -> :m
      "f" -> :f
    end
    sex = :erlang.element(@sex, account)
    cond do
      (sex_eq==sex) ->
        new_map = Map.merge(map, %{sex: sex})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def sex_eq(t), do: t

  def interests_any({_, _, _, true} = t), do: t
  def interests_any({%{"interests_any" => value} = params, account, map, break}) do
    interests_any = value
    |> String.split(",")
    |> MapSet.new()
    interests = :erlang.element(@interests, account)
    cond do
      (interests==nil) -> {nil, nil, nil, true}
      true ->
        usr_interests = Interests.ids_to_names(interests)
        usr_interests_set = MapSet.new(usr_interests)
        match = MapSet.size(MapSet.intersection(interests_any, usr_interests_set)) > 0
        case match do
          true ->
            # new_map = Map.merge(map, %{interests: usr_interests})
            {params, account, map, break}
          false ->
            @no_need_check
        end             
    end
  end
  def interests_any(t), do: t

  def interests_contains({_, _, _, true} = t), do: t
  def interests_contains({%{"interests_contains" => value} = params, account, map, break}) do
    interests_contains = value
    |> String.split(",")
    |> MapSet.new()
    interests = :erlang.element(@interests, account)

    cond do
      (interests==nil) ->
        @no_need_check
      true ->
        usr_interests = Interests.ids_to_names(interests)
        usr_interests_set = MapSet.new(usr_interests)

        match = MapSet.subset?(interests_contains, usr_interests_set)
        case match do
          true ->
            # new_map = Map.merge(map, %{interests: usr_interests})
            {params, account, map, break}
          false ->
            @no_need_check
        end             
    end
  end
  def interests_contains(t), do: t

  
  def status_eq({_, _, _, true} = t), do: t
  def status_eq({%{"status_eq" => value} = params, account, map, break}) do
    status_eq = case value do
      "свободны" -> 1
      "заняты" -> 2
      "всё сложно" -> 3
    end
    status = :erlang.element(@status, account)
    cond do
      status_eq==status ->
        new_map = Map.merge(map, %{status: params["status_eq"]})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def status_eq(t), do: t

  def status_neq({_, _, _, true} = t), do: t
  def status_neq({%{"status_neq" => value} = params, account, map, break}) do
    status_neq = case value do
      "свободны" -> 1
      "заняты" -> 2
      "всё сложно" -> 3
    end
    status = :erlang.element(@status, account)
    cond do
      not(status_neq==status) ->
        text_status = case status do
          1 -> "свободны"
          2 -> "заняты"
          3 -> "всё сложно"
        end
        new_map = Map.merge(map, %{status: text_status})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def status_neq(t), do: t

  def fname_eq({_, _, _, true} = t), do: t
  def fname_eq({%{"fname_eq" => value} = params, account, map, break}) do
    fname_eq = value
    fname = Utils.numstr_to_str(:erlang.element(@fname, account))
    cond do
      fname == nil -> @no_need_check
      (fname_eq==fname) ->
        new_map = Map.merge(map, %{fname: value})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def fname_eq(t), do: t

  def fname_any({_, _, _, true} = t), do: t
  def fname_any({%{"fname_any" => value} = params, account, map, break}) do
    fname_any = value |> String.split(",") |> MapSet.new()
    fname = Utils.numstr_to_str(:erlang.element(@fname, account))
    cond do
      (fname in fname_any) ->
        new_map = Map.merge(map, %{fname: fname})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def fname_any(t), do: t

  def fname_null({_, _, _, true} = t), do: t
  def fname_null({%{"fname_null" => value} = params, account, map, break}) do
    fname = Utils.numstr_to_str(:erlang.element(@fname, account))
    case value do
      # если указано
      "0" ->
        case fname do
          nil -> @no_need_check
          fname -> {params, account, Map.merge(map, %{fname: fname}), break}
        end
      # не указано
      "1" ->
        case fname do
          nil -> {params, account, map, break}
          _fname -> @no_need_check
        end
      # указана какая-то херня
      _else ->
        @no_need_check
    end
  end
  def fname_null(t), do: t

  def sname_eq({_, _, _, true} = t), do: t
  def sname_eq({%{"sname_eq" => value} = params, account, map, break}) do
    sname_eq = value
    sname = Utils.numstr_to_str(:erlang.element(@sname, account))

    cond do
      (sname_eq==sname) ->
        new_map = Map.merge(map, %{sname: value})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def sname_eq(t), do: t

  def sname_starts({_, _, _, true} = t), do: t
  def sname_starts({%{"sname_starts" => value} = params, account, map, break}) do
    sname_starts = value

    sname = Utils.numstr_to_str(:erlang.element(@sname, account))
    sl_sname = String.slice(sname, 0..(String.length(sname_starts)-1))

    cond do
      sname == nil -> @no_need_check
      (sname_starts==sl_sname) ->
        new_map = Map.merge(map, %{sname: sname})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def sname_starts(t), do: t

  def sname_null({_, _, _, true} = t), do: t
  def sname_null({%{"sname_null" => value} = params, account, map, break}) do
    sname = Utils.numstr_to_str(:erlang.element(@sname, account))
    case value do
      # если указано
      "0" ->
        case sname do
          nil -> @no_need_check
          sname -> {params, account, Map.merge(map, %{sname: sname}), break}
        end
      # не указано
      "1" ->
        case sname do
          nil -> {params, account, map, break}
          _sname -> @no_need_check
        end
      # указана какая-то херня
      _else ->
        @no_need_check
    end
  end
  def sname_null(t), do: t

  def country_eq({_, _, _, true} = t), do: t
  def country_eq({%{"country_eq" => value} = params, account, map, break}) do
    country_eq = value

    country = Countrys.id_to_name(:erlang.element(@country_id, account))
    cond do
      (country_eq==country) ->
        new_map = Map.merge(map, %{country: country})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def country_eq(t), do: t

  def country_null({_, _, _, true} = t), do: t
  def country_null({%{"country_null" => value} = params, account, map, break}) do
    country = Countrys.id_to_name(:erlang.element(@country_id, account))
    case value do
      # если указано
      "0" ->
        case country do
          nil -> @no_need_check
          country -> {params, account, Map.merge(map, %{country: country}), break}
        end
      # не указано
      "1" ->
        case country do
          nil -> {params, account, map, break}
          _country -> @no_need_check
        end
      # указана какая-то херня
      _else ->
        @no_need_check
    end
  end
  def country_null(t), do: t

  def city_eq({_, _, _, true} = t), do: t
  def city_eq({%{"city_eq" => value} = params, account, map, break}) do
    city_eq = value

    city = Citys.id_to_name(:erlang.element(@city_id, account))
    cond do
      (city_eq==city) ->
        new_map = Map.merge(map, %{city: city})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def city_eq(t), do: t

  def city_null({_, _, _, true} = t), do: t
  def city_null({%{"city_null" => value} = params, account, map, break}) do
    city = Citys.id_to_name(:erlang.element(@city_id, account))
    case value do
      # если указано
      "0" ->
        case city do
          nil -> @no_need_check
          city -> {params, account, Map.merge(map, %{city: city}), break}
        end
      # не указано
      "1" ->
        case city do
          nil -> {params, account, map, break}
          _city -> @no_need_check
        end
      # указана какая-то херня
      _else ->
        @no_need_check
    end
  end
  def city_null(t), do: t

  def city_any({_, _, _, true} = t), do: t
  def city_any({%{"city_any" => value} = params, account, map, break}) do
    city_any = value |> String.split(",") |> MapSet.new()
    city = Citys.id_to_name(:erlang.element(@city_id, account))
    cond do
      (city in city_any) ->
        new_map = Map.merge(map, %{city: city})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def city_any(t), do: t

  def phone_code({_, _, _, true} = t), do: t
  def phone_code({%{"phone_code" => value} = params, account, map, break}) do
    phone_code = value

    phone = Utils.numstr_to_str(:erlang.element(@phone_id, account))
    code = case phone do
      nil -> nil
      phone -> phone |> String.split("(") |> tl() |> hd |> String.split(")")|> hd
    end

    cond do
      phone == nil -> @no_need_check
      (code==phone_code) ->
        new_map = Map.merge(map, %{phone: phone})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def phone_code(t), do: t

  def phone_null({_, _, _, true} = t), do: t
  def phone_null({%{"phone_null" => value} = params, account, map, break}) do
    phone = Utils.numstr_to_str(:erlang.element(@phone_id, account))
    case value do
      # если указано
      "0" ->
        case phone do
          nil -> @no_need_check
          phone -> {params, account, Map.merge(map, %{phone: phone}), break}
        end
      # не указано
      "1" ->
        case phone do
          nil -> {params, account, map, break}
          _phone -> @no_need_check
        end
      # указана какая-то херня
      _else ->
        @no_need_check
    end
  end
  def phone_null(t), do: t

  def email_domain({_, _, _, true} = t), do: t
  def email_domain({%{"email_domain" => value} = params, account, map, break}) do
    email_domain = value

    email = Utils.numstr_to_str(:erlang.element(@email_id, account))

    domain = case email do
      nil -> nil
      email ->
        [_em, domain] = String.split(email, "@")
        domain
    end
    
    cond do
      domain == nil ->
        @no_need_check
      (email_domain==domain) ->
        new_map = Map.merge(map, %{email: email})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def email_domain(t), do: t

  def email_lt({_, _, _, true} = t), do: t
  def email_lt({%{"email_lt" => value} = params, account, map, break}) do
    email_lt = value

    email = Utils.numstr_to_str(:erlang.element(@email_id, account))
   
    cond do
      email == nil ->
        @no_need_check
      (email<email_lt) ->
        new_map = Map.merge(map, %{email: email})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def email_lt(t), do: t

  def email_gt({_, _, _, true} = t), do: t
  def email_gt({%{"email_gt" => value} = params, account, map, break}) do
    email_gt = value

    email = Utils.numstr_to_str(:erlang.element(@email_id, account))
   
    cond do
      email == nil ->
        @no_need_check
      (email>email_gt) ->
        new_map = Map.merge(map, %{email: email})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def email_gt(t), do: t

  def birth_lt({_, _, _, true} = t), do: t
  def birth_lt({%{"birth_lt" => value} = params, account, map, break}) do
    birth_lt = value
    birth = :erlang.element(@birth, account)
   
    cond do
      birth == nil ->
        @no_need_check
      (birth<birth_lt) ->
        new_map = Map.merge(map, %{birth: birth})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def birth_lt(t), do: t

  def birth_gt({_, _, _, true} = t), do: t
  def birth_gt({%{"birth_gt" => value} = params, account, map, break}) do
    birth_gt = value
    birth = :erlang.element(@birth, account)
   
    cond do
      birth == nil ->
        @no_need_check
      (birth>birth_gt) ->
        new_map = Map.merge(map, %{birth: birth})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def birth_gt(t), do: t

  def birth_year({_, _, _, true} = t), do: t
  def birth_year({%{"birth_year" => value} = params, account, map, break}) do
    birth_year = value
    int_birth_year = case Integer.parse(birth_year) do
      {intVal, ""} -> intVal
      :error -> nil
    end

    birth = :erlang.element(@birth, account)

    usr_year = case birth do
      nil -> nil
      birth ->
        dt = (birth |> DateTime.from_unix!(:seconds))
        dt.year
    end
   
    cond do
      int_birth_year == nil -> @no_need_check
      birth == nil -> @no_need_check
      (int_birth_year==usr_year) ->
        new_map = Map.merge(map, %{birth: birth})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def birth_year(t), do: t

  def premium_now({_, _, _, true} = t, _), do: t
  def premium_now({%{"premium_now" => _} = params, account, map, break}, now) do
    premium_start = :erlang.element(@premium_start, account)
    premium_finish = :erlang.element(@premium_finish, account)
   
    cond do
      premium_start == nil -> @no_need_check
      premium_finish == nil -> @no_need_check
      (premium_start>=now) and (now<=premium_finish)->
        new_map = Map.merge(map, %{premium: %{start: premium_start, finish: premium_finish}})
        {params, account, new_map, break}
      true ->
        @no_need_check
    end
  end
  def premium_now(t, _), do: t

  def premium_null({_, _, _, true} = t), do: t
  def premium_null({%{"premium_null" => value} = params, account, map, break}) do
    premium_start = :erlang.element(@premium_start, account)
    premium_finish = :erlang.element(@premium_finish, account)
    case value do
      # если указано
      "0" ->
        case premium_start do
          nil -> @no_need_check
          premium_start -> {params, account, Map.merge(map, %{premium: %{start: premium_start, finish: premium_finish}}), break}
        end
      # не указано
      "1" ->
        case premium_start do
          nil -> {params, account, map, break}
          _premium_start -> @no_need_check
        end
      # указана какая-то херня
      _else ->
        @no_need_check
    end
  end
  def premium_null(t), do: t

  def likes_contains({_, _, _, true} = t), do: t
  def likes_contains({%{"likes_contains" => value} = params, account, map, break}) do
    likes_ids = value
    |> String.split(",")
    |> Enum.map(fn(id_str) ->
      case Integer.parse(id_str) do
        {intVal, ""} -> intVal
        :error -> nil
      end
    end)
    id = :erlang.element(@id, account)
    likes = :erlang.element(@likes, account)
    likes_list = Likes.get(id)

    # Logger.debug ">>>>>>>>> likes_list=#{inspect likes_list}"
    match = cond do
      likes_list==nil -> false
      true ->
        Enum.reduce_while(likes_list, false, fn(%{id: usr_id}, _acc) ->
          id_in_likes = usr_id in likes_ids
          case id_in_likes do
            true ->  {:halt, true}
            false -> {:cont, false}
          end
        end)
    end

    cond do
      likes == nil ->
        @no_need_check
      match ->
        {params, account, map, break}
      true ->
        @no_need_check
    end
  end
  def likes_contains(t), do: t


end

