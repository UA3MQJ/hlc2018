defmodule HttpTest2.Citys do
  use GenServer
  require Logger
  alias HttpTest2.Utils

  def start_link do
      :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    # Logger.info ">>> citys init"
    :ets.new(:citys, [:named_table, :public, :ordered_set, {:keypos, 1}])

    {:ok, {1, :gb_trees.empty()}}
  end

  def name_to_id(nil),  do: nil
  def name_to_id(name), do: GenServer.call(__MODULE__, {:name_to_id, name})

  def id_to_name(nil), do: nil
  def id_to_name(id) do
    case :ets.lookup(:citys, id) do
      [] -> nil
      [{^id, name}] -> Utils.numstr_to_str(name)
    end
  end
  
  def get_trie(), do: GenServer.call(__MODULE__, :get_trie)

  # callbacks 

  def handle_call({:name_to_id, name}, _, {new_id, trie} = state) do
    # Logger.debug ">>>> win_name=#{inspect win_name}"

    list_name = :erlang.binary_to_list(name)
    numstr_name = Utils.str_to_numstr(name)
    # case :sets.is_element(name, trie) do
    case :gb_trees.take_any(numstr_name, trie) do
      # false ->
      :error ->
        true = :ets.insert(:citys, {new_id, numstr_name})
        {:reply, new_id, {new_id + 1, :gb_trees.enter(numstr_name, new_id, trie)}}
      # id ->
      {id, _} ->
        {:reply, id, state}
    end
  end

  def handle_call(:get_trie, _, {_new_id, trie} = state) do
    {:reply, trie, state}
  end

end
