defmodule HttpTest2.Emails do
  use GenServer
  require Logger


  def start_link do
      :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    Logger.info ">>> Emails init"

    {:ok, {1, Retrieval.new(with_id: true)}}
  end

  def get_id(name) do
    GenServer.call(__MODULE__, {:get_id, name})
  end

  def handle_call({:get_id, name}, _, {new_id, trie} = state) do
    case Retrieval.contains?(trie, name) do
      false ->
        {:reply, {:new, new_id}, {new_id + 1, Retrieval.insert(trie, name, new_id)}}
      id ->
        {:reply, {:old, id}, state}
    end
  end


end
