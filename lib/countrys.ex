defmodule HttpTest2.Countrys do
  use GenServer
  require Logger


  def start_link do
      :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    Logger.info ">>> Countrys init"

    {:ok, {1, Retrieval.new(with_id: true)}}
  end

  def get_id(name) do
    GenServer.call(__MODULE__, {:get_id, name})
  end

  def get_trie() do
    GenServer.call(__MODULE__, :get_trie)
  end

  def handle_call({:get_id, name}, _, {new_id, trie} = state) do
    case Retrieval.contains?(trie, name) do
      false ->
        {:reply, {:new, new_id}, {new_id + 1, Retrieval.insert(trie, name, new_id)}}
      id ->
        {:reply, {:old, id}, state}
    end
  end

  def handle_call(:get_trie, _, {new_id, trie} = state) do
    {:reply, trie, state}
  end
  
end
