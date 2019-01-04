defmodule HttpTest2.Accounts do
  use GenServer
  require Logger
  alias HttpTest2.Filters

  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    # Logger.info ">>> accounts init"
    :ets.new(:accounts, [:named_table, :public, :set, {:keypos, 1}])

    [now_time_str, type] = File.read!("priv/data/options.txt") |> String.split("\n")
    
    now_time = case Integer.parse(now_time_str) do
      {intVal, ""} -> intVal
      :error -> nil
    end

    {:ok, {%{}, [], now_time}}
  end

  def set(id, account) do
    # Logger.debug ">>>> account=#{inspect account}"
    true = :ets.insert(:accounts, account)
    GenServer.cast(__MODULE__, {:set, id})
  end

  # def set_async(id, account) do
  #   GenServer.cast(__MODULE__, {:set_async, id, account})
  # end

  def sort_ids() do
    GenServer.cast(__MODULE__, :sort_ids)
  end

  def get(id) do
    case :ets.lookup(:accounts, id) do
      [] -> nil
      [account] -> account
    end
  end

  def get_id_list(), do: GenServer.call(__MODULE__, :get_id_list)


  def filter(params) do
    {id_list, now} = get_id_list()
    Filters.filter(params, id_list, now)
  end

  # def handle_call({:set, id, account}, _, {map, id_list, now} = state) do
  #   new_map = Map.merge(map, %{id => account})
  #   new_id_list = [id] ++ id_list
  #   {:reply, :ok, {new_map, new_id_list, now}}
  # end

  # def handle_call({:get, id}, _, {map, id_list, now} = state) do
  #   result = map[id]
  #   {:reply, result, state}
  # end

  # def handle_call(:get_state, _, state) do
  #   {:reply, state, state}
  # end

  def handle_call(:get_id_list, _, {_map, id_list, now} = state) do
    {:reply, {id_list, now}, state}
  end 

  # def handle_cast({:set_async, id, account}, {map, id_list, now} = _state) do
  #   new_map = Map.merge(map, %{id => account})
  #   new_id_list = [id] ++ id_list
  #   {:noreply, {new_map, new_id_list}}
  # end

  def handle_cast({:set, id}, {map, id_list, now} = _state) do
    new_id_list = [id] ++ id_list
    {:noreply, {map, new_id_list, now}}
  end


  def handle_cast(:sort_ids, {map, id_list, now} = _state) do
    time1 = :os.system_time(:millisecond)
    new_id_list = id_list |> :lists.sort() |> :lists.reverse()
    time2 = :os.system_time(:millisecond)
    Logger.info ">>> sort_ids #{time2 - time1} ms"

    {:noreply, {map, new_id_list, now}}
  end

end
