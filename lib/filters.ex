defmodule HttpTest2.Filters do
  require Logger
  alias HttpTest2.Utils

  @no_need_check {nil, nil, nil, true}

  # {^id, email_id, sname, fname, phone_id, sex,
  #    birth, country_id, city_id, joined, status,
  #    interests, premium_start, premium_finish, likes} = account

  @sname 3
  @fname 4
  @sex 6
  @interests 12
  @status 11

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
    interests_any = value |> String.split(",") |> MapSet.new()
    interests = :erlang.element(@interests, account)
    cond do
      (interests==nil) -> {nil, nil, nil, true}
      true ->
        usr_interests = interests
        |> Enum.map(fn(interest_id) ->
          HttpTest2.KVS.interest_from_id(interest_id)
        end)
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
    interests_contains = value |> String.split(",") |> MapSet.new()
    interests = :erlang.element(@interests, account)
    cond do
      (interests==nil) ->
        @no_need_check
      true ->
        usr_interests = interests
        |> Enum.map(fn(interest_id) ->
          HttpTest2.KVS.interest_from_id(interest_id)
        end)
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
    fname_eq = Utils.unicode_to_win1251(value)

    fname = :erlang.element(@fname, account)
    cond do
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
    fname = Utils.win1251_to_unicode(:erlang.element(@fname, account))
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
    fname = Utils.win1251_to_unicode(:erlang.element(@fname, account))
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

end
