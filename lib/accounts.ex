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
    :ets.new(:index, [:named_table, :public, :set, {:keypos, 1}])

    true = :ets.insert(:index, {:status, :not_ready})

    [now_time_str, _type] = File.read!("priv/data/options.txt") |> String.split("\n")
    
    now_time = case Integer.parse(now_time_str) do
      {intVal, ""} -> intVal
      :error -> nil
    end
    true = :ets.insert(:index, {:now_time, now_time})

    ids = %{id_list: [], sex_m: [], sex_f: []}
    {:ok, {%{}, ids, now_time}}
  end

  def set_id(id) do
    GenServer.cast(__MODULE__, {:set, id})
  end

  def set(id, account) do
    true = :ets.insert(:accounts, account)
    GenServer.cast(__MODULE__, {:set, id})
    sex = :erlang.element(6, account)
    case sex do
      :m -> GenServer.cast(__MODULE__, {:set_sex_m, id})
      :f -> GenServer.cast(__MODULE__, {:set_sex_f, id})
    end
  end


  def sort_ids() do
    GenServer.call(__MODULE__, :sort_ids)
  end

  def get(id) do
    case :ets.lookup(:accounts, id) do
      [] -> nil
      [account] -> account
    end
  end

  def get_id_list() do
    case :ets.lookup(:index, :status) do
      [{:status, :not_ready}] -> sort_ids()
      [{:status, :ready}] -> :ok
    end

    case :ets.lookup(:index, :sort_ids) do
      [] -> nil
      [{:sort_ids, id_list}] -> id_list
    end    
  end

  def get_sex_m() do
    case :ets.lookup(:index, :status) do
      [{:status, :not_ready}] -> sort_ids()
      [{:status, :ready}] -> :ok
    end

    case :ets.lookup(:index, :sex_m) do
      [] -> nil
      [{:sex_m, id_list}] -> id_list
    end    
  end

  def get_sex_f() do
    case :ets.lookup(:index, :status) do
      [{:status, :not_ready}] -> sort_ids()
      [{:status, :ready}] -> :ok
    end

    case :ets.lookup(:index, :sex_f) do
      [] -> nil
      [{:sex_f, id_list}] -> id_list
    end    
  end
  def get_now_time() do
    case :ets.lookup(:index, :now_time) do
      [] -> nil
      [{:now_time, now_time}] -> now_time
    end    
  end

  def filter(params) do
    Filters.filter(params)
  end


  def handle_cast({:set, id}, {map, %{id_list: id_list} = ids, now} = _state) do
    true = :ets.insert(:index, {:status, :not_ready})
    new_id_list = [id] ++ id_list
    {:noreply, {map, %{ids | id_list: new_id_list}, now}}
  end

  def handle_cast({:set_sex_m, id}, {map, %{sex_m: sex_m} = ids, now} = _state) do
    true = :ets.insert(:index, {:status, :not_ready})
    new_sex_m = [id] ++ sex_m
    {:noreply, {map, %{ids | sex_m: new_sex_m}, now}}
  end

  def handle_cast({:set_sex_f, id}, {map, %{sex_f: sex_f} = ids, now} = _state) do
    true = :ets.insert(:index, {:status, :not_ready})
    new_sex_f = [id] ++ sex_f
    {:noreply, {map, %{ids | sex_f: new_sex_f}, now}}
  end


  def handle_call(:sort_ids, _, {map, %{id_list: id_list, sex_m: sex_m, sex_f: sex_f} = ids, now} = _state) do
    time1 = :os.system_time(:millisecond)
    new_id_list = id_list |> :lists.reverse() |> :lists.sort() |> :lists.reverse()
    time2 = :os.system_time(:millisecond)

    time1 = :os.system_time(:millisecond)
    new_sex_m = sex_m |> :lists.reverse() |> :lists.sort() |> :lists.reverse()
    time2 = :os.system_time(:millisecond)

    time1 = :os.system_time(:millisecond)
    new_sex_f = sex_f |> :lists.reverse() |> :lists.sort() |> :lists.reverse()
    time2 = :os.system_time(:millisecond)

    true = :ets.insert(:index, {:sort_ids, MapSet.new(new_id_list)})
    true = :ets.insert(:index, {:sex_m, MapSet.new(new_sex_m)})
    true = :ets.insert(:index, {:sex_f, MapSet.new(new_sex_f)})
    true = :ets.insert(:index, {:status, :ready})

    {:reply, :ok, {map, %{ids | id_list: new_id_list}, now}}
  end

end
