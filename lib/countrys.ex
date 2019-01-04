defmodule HttpTest2.Countrys do
  use GenServer
  require Logger
  alias HttpTest2.Utils

  def start_link do
      :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    # Logger.info ">>> countrys init"
    :ets.new(:countrys, [:named_table, :public, :set, {:keypos, 1}])

    {:ok, {1, Retrieval.new(with_id: true)}}
  end

  def name_to_id(nil),  do: nil
  def name_to_id(name), do: GenServer.call(__MODULE__, {:name_to_id, name})

  def id_to_name(nil), do: nil
  def id_to_name(id) do
    case :ets.lookup(:countrys, id) do
      [] -> nil
      [{^id, name}] -> Utils.win1251_to_unicode(name)
    end
  end
  
  def get_trie(), do: GenServer.call(__MODULE__, :get_trie)

  # callbacks 

  def handle_call({:name_to_id, name}, _, {new_id, trie} = state) do
    case Retrieval.contains?(trie, name) do
      false ->
        true = :ets.insert(:countrys, {new_id, Utils.unicode_to_win1251(name)})
        {:reply, new_id, {new_id + 1, Retrieval.insert(trie, name, new_id)}}
      id ->
        {:reply, id, state}
    end
  end

  def handle_call(:get_trie, _, {new_id, trie} = state) do
    {:reply, trie, state}
  end  
end
