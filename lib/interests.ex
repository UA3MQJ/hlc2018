defmodule HttpTest2.Interests do
  use GenServer
  require Logger
  alias HttpTest2.Utils

  def start_link do
      :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    # Logger.info ">>> interests init"
    :ets.new(:interests, [:named_table, :public, :ordered_set, {:keypos, 1}])

    {:ok, {1, Retrieval.new(with_id: true)}}
  end

  def name_to_id(nil),  do: nil
  def name_to_id(name), do: GenServer.call(__MODULE__, {:name_to_id, name})

  def names_to_ids(nil),  do: nil
  def names_to_ids(names_list), do: GenServer.call(__MODULE__, {:names_to_ids, names_list})

  def id_to_name(nil), do: nil
  def id_to_name(id) do
    case :ets.lookup(:interests, id) do
      [] -> nil
      [{^id, name}] -> Utils.win1251_to_unicode(name)
    end
  end

  def ids_to_names(nil), do: nil
  def ids_to_names(id_list) do
    Enum.map(id_list, fn(id) ->
      case :ets.lookup(:interests, id) do
        [] -> nil
        [{^id, name}] -> Utils.win1251_to_unicode(name)
      end
    end)
  end
  
  def get_trie(), do: GenServer.call(__MODULE__, :get_trie)

  # callbacks 

  def handle_call({:name_to_id, name}, _, {new_id, trie} = state) do
    case Retrieval.contains?(trie, name) do
      false ->
        true = :ets.insert(:interests, {new_id, Utils.unicode_to_win1251_list(name)})
        {:reply, new_id, {new_id + 1, Retrieval.insert(trie, name, new_id)}}
      id ->
        {:reply, id, state}
    end
  end

  def handle_call({:names_to_ids, names_list}, _, {new_id, trie} = state) do
    {new_new_id, new_trie, result} = Enum.reduce(names_list, {new_id, trie, []}, fn(win_name, {acc_new_id, acc_trie, acc_res}) ->
      name = Utils.win1251_to_unicode(win_name)
      case Retrieval.contains?(acc_trie, name) do
        false ->
          true = :ets.insert(:interests, {acc_new_id, Utils.unicode_to_win1251_list(name)})
          {acc_new_id + 1, Retrieval.insert(acc_trie, name, acc_new_id), [acc_new_id] ++ acc_res}
        id ->
          {acc_new_id, acc_trie, [id] ++ acc_res}
      end
    end)

    {:reply, :lists.reverse(result), {new_new_id, new_trie}}
  end

  def handle_call(:get_trie, _, {new_id, trie} = state) do
    {:reply, trie, state}
  end

end
