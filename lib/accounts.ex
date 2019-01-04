defmodule HttpTest2.Accounts do
  use GenServer
  require Logger
  alias HttpTest2.Filters

  def start_link do
      :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    # Logger.info ">>> accounts init"
    [now_time_str, type] = File.read!("priv/data/options.txt") |> String.split("\n")
    
    now_time = case Integer.parse(now_time_str) do
      {intVal, ""} -> intVal
      :error -> nil
    end

    {:ok, {%{}, [], now_time}}
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

  def handle_call({:set, id, account}, _, {map, id_list, now} = state) do
    new_map = Map.merge(map, %{id => account})
    new_id_list = [id] ++ id_list
    {:reply, :ok, {new_map, new_id_list, now}}
  end

  def handle_call({:get, id}, _, {map, id_list, now} = state) do
    result = map[id]
    {:reply, result, state}
  end


  def handle_call({:filter, params}, _, {map, id_list, now} = state) do
    # Logger.debug ">>>> filter params=#{inspect params}"
    # result = []
    limit_str = params["limit"] || "10"
    limit = case Integer.parse(limit_str) do
      {intVal, ""} -> intVal
      :error -> :error
    end

    err_params_list = (Map.keys(params)) -- ["query_id", "limit", "sex_eq", "interests_any",
                      "interests_contains", "status_eq", "status_neq", "fname_eq", "fname_any", 
                      "fname_null", "sname_eq", "sname_starts", "sname_null", 
                      "country_eq", "country_null", "city_eq", "city_null", 
                      "city_any", "phone_code", "phone_null", "email_domain", 
                      "email_lt", "email_gt", "birth_lt", "birth_gt", 
                      "birth_year", "premium_now", "premium_null", "likes_contains"]



    params_is_valid? = !(limit==:error) and (length(err_params_list) == 0)

    # if !params_is_valid? do
    #   Logger.debug ">>> err_params_list=#{inspect err_params_list}"
    # end

    case params_is_valid? do
      true ->
        {result, _} = id_list
        |> Enum.reduce_while({[], limit}, fn(id, {a_list, a_limit} = acc) ->

          account = map[id]
          {^id, email_id, sname, fname, phone_id, sex,
             birth, country_id, city_id, joined, status,
             interests, premium_start, premium_finish, likes} = account

          # проверим запись фильтрами
          {_, _, result_map, _} = {params, account, %{}, false}
          |> Filters.sex_eq()
          |> Filters.interests_any()
          |> Filters.interests_contains()
          |> Filters.status_eq()
          |> Filters.status_neq()
          |> Filters.fname_eq()
          |> Filters.fname_any()
          |> Filters.fname_null()
          |> Filters.sname_eq()
          |> Filters.sname_starts()
          |> Filters.sname_null()
          |> Filters.country_eq()
          |> Filters.country_null()
          |> Filters.city_eq()
          |> Filters.city_null()
          |> Filters.city_any()
          |> Filters.phone_code()
          |> Filters.phone_null()
          |> Filters.email_domain()
          |> Filters.email_lt()
          |> Filters.email_gt()
          |> Filters.birth_lt()
          |> Filters.birth_gt()
          |> Filters.birth_year()
          |> Filters.premium_now(now)
          |> Filters.premium_null()
          |> Filters.likes_contains()

          case result_map do
            nil -> {:cont, acc}
            _ ->
              email = HttpTest2.KVS.email_from_id(email_id)
              # Logger.debug ">>>> result_map=#{inspect result_map}"
              nresult_map = result_map
              |> Map.merge(%{id: id})
              |> Map.merge(%{email: email})

              new_limit = a_limit - 1
              case new_limit do
                0 -> {:halt, {[nresult_map] ++ a_list, new_limit}}
                _ -> {:cont, {[nresult_map] ++ a_list, new_limit}}
              end
          end
        end)

        {:reply, :lists.reverse(result), state}
      false ->
        {:reply, :error, state}
    end

  end

  def handle_cast({:set_async, id, account}, {map, id_list, now} = _state) do
    new_map = Map.merge(map, %{id => account})
    new_id_list = [id] ++ id_list
    {:noreply, {new_map, new_id_list}}
  end

  def handle_cast(:sort_ids, {map, id_list, now} = _state) do
    time1 = :os.system_time(:millisecond)
    new_id_list = id_list |> :lists.sort() |> :lists.reverse()
    time2 = :os.system_time(:millisecond)
    Logger.info ">>> sort_ids #{time2 - time1} ms"

    {:noreply, {map, new_id_list, now}}
  end

end
