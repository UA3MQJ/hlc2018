defmodule HttpTest2.Likes do
  use GenServer
  require Logger


  def start_link do
      :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    Logger.info ">>> Likes init"

    {:ok, %{}}
  end

  def set_likes(id, likes) do
    GenServer.call(__MODULE__, {:set_likes, id, likes})
  end

  def get_likes(id) do
    GenServer.call(__MODULE__, {:get_likes, id})
  end

  def handle_call({:set_likes, id, likes}, _, state) do
    {:reply, :ok, Map.merge(state, %{id => likes})}
  end

  def handle_call({:get_likes, id}, _, state) do
    {:reply, state[id], state}
  end


end
