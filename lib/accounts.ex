defmodule HttpTest2.Accounts do
  use GenServer
  require Logger


  def start_link do
      :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    Logger.info ">>> accounts init"

    {:ok, {%{}, []}}
  end

  def set(id, account) do
    GenServer.call(__MODULE__, {:set, id, account})
  end

  def set_async(id, account) do
    GenServer.cast(__MODULE__, {:set_async, id, account})
  end

  def sort_ids() do
    GenServer.cast(__MODULE__, :sort_ids)
  end

  def get(id) do
    GenServer.call(__MODULE__, {:get, id})
  end

  def filter(params) do
    GenServer.call(__MODULE__, {:filter, params})
  end

  def handle_call({:set, id, account}, _, {map, id_list} = state) do
    new_map = Map.merge(map, %{id => account})
    new_id_list = [id] ++ id_list
    {:reply, :ok, {new_map, new_id_list}}
  end

  def handle_call({:get, id}, _, {map, id_list} = state) do
    result = map[id]
    {:reply, result, state}
  end

  def handle_call({:filter, params}, _, {map, id_list} = state) do
    # Logger.debug ">>>> filter params=#{inspect params}"
    result = []
    limit_str = params["limit"] || "1"
    limit = case Integer.parse(limit_str) do
      {intVal, ""} -> intVal
      :error -> :error
    end

    interests_any = case params["interests_any"] do
      nil -> nil
      interests_any -> interests_any |> String.split(",") |> MapSet.new()
    end

    interests_contains = case params["interests_contains"] do
      nil -> nil
      interests_contains -> interests_contains |> String.split(",") |> MapSet.new()
    end

    sex_eq = case params["sex_eq"] do
      nil -> nil
      "m" -> :m
      "f" -> :f
    end

    status_eq = case params["status_eq"] do
      nil -> nil
      "свободны" -> 1
      "заняты" -> 2
      "всё сложно" -> 3
    end

    status_neq = case params["status_neq"] do
      nil -> nil
      "свободны" -> 1
      "заняты" -> 2
      "всё сложно" -> 3
    end


    params_is_valid? = !(limit==:error)

    case params_is_valid? do
      true ->
        {result, _} = id_list
        |> Enum.reduce_while({[], limit}, fn(id, {a_list, a_limit} = acc) ->

          account = map[id]
          {^id, email_id, sname, fname, phone_id, sex,
             birth, country_id, city_id, joined, status,
             interests, premium_start, premium_finish, likes} = account

          result_map = %{}
          break = false # ==true дальше можно не проверять

          rule1 = !(sex_eq==nil)
          rule2 = !(interests_any==nil)
          rule3 = !(interests_contains==nil)
          rule4 = !(status_eq==nil)
          rule5 = !(status_neq==nil)

          # sex_eq
          {result_map, break} = cond do
            !rule1 -> {result_map, break}
            rule1 and break -> {%{}, true}
            rule1 and (sex_eq==sex) -> {Map.merge(result_map, %{sex: sex}), break}
            true -> {%{}, true}
          end

          {result_map, break} = cond do
            !rule2 -> {result_map, break}
            rule2 and break -> {result_map, break}
            rule2 and (interests==nil) -> {%{}, true}
            true ->
              usr_interests = interests
              |> Enum.map(fn(interest_id) ->
                HttpTest2.KVS.interest_from_id(interest_id)
              end)

              usr_interests_set = MapSet.new(usr_interests)

              match = MapSet.size(MapSet.intersection(interests_any, usr_interests_set)) > 0

              case match do
                true ->  {Map.merge(result_map, %{interests: usr_interests}), break}
                false -> {%{}, true}
              end             
          end

          {result_map, break} = cond do
            !rule3 -> {result_map, break}
            rule3 and break -> {result_map, break}
            rule3 and (interests==nil) -> {%{}, true}
            true ->
              usr_interests = interests
              |> Enum.map(fn(interest_id) ->
                HttpTest2.KVS.interest_from_id(interest_id)
              end)

              usr_interests_set = MapSet.new(usr_interests)

              match = MapSet.subset?(interests_contains, usr_interests_set)

              case match do
                true ->  {Map.merge(result_map, %{interests: usr_interests}), break}
                false -> {%{}, true}
              end             
          end

          # status_eq
          {result_map, break} = cond do
            !rule4 -> {result_map, break}
            rule4 and break -> {%{}, true}
            rule4 and (status_eq==status) -> {Map.merge(result_map, %{status: params["status_eq"]}), break}
            true -> {%{}, true}
          end

          # status_neq
          {result_map, break} = cond do
            !rule5 -> {result_map, break}
            rule5 and break -> {%{}, true}
            rule5 and (status==nil) -> {%{}, true}
            rule5 and not(status_neq==status) ->
              text_status = case status do
                1 -> "свободны"
                2 -> "заняты"
                3 -> "всё сложно"
              end
              {Map.merge(result_map, %{status: text_status}), break}
            true -> {%{}, true}
          end


          case map_size(result_map) do
            0 -> {:cont, acc}
            _ ->
              result_map = Map.merge(result_map, %{id: id})
              new_limit = a_limit - 1
              case new_limit do
                0 -> {:halt, {[result_map] ++ a_list, new_limit}}
                _ -> {:cont, {[result_map] ++ a_list, new_limit}}
              end
          end
        end)

        {:reply, :lists.reverse(result), state}
      false ->
        {:reply, :error, state}
    end

  end




  def handle_cast({:set_async, id, account}, {map, id_list} = state) do
    new_map = Map.merge(map, %{id => account})
    new_id_list = [id] ++ id_list
    {:noreply, {new_map, new_id_list}}
  end

  def handle_cast(:sort_ids, {map, id_list} = state) do
    time1 = :os.system_time(:millisecond)
    new_id_list = id_list |> :lists.sort() |> :lists.reverse()
    time2 = :os.system_time(:millisecond)
    Logger.info ">>> sort_ids #{time2 - time1} ms"

    {:noreply, {map, new_id_list}}
  end

end
