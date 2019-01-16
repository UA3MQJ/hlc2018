defmodule HttpTest2.Likes do
  use GenServer
  require Logger


  def start_link do
      :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    # Logger.info ">>> Likes init"
    :ets.new(:likes, [:named_table, :public, :ordered_set])
    :ets.new(:liked, [:named_table, :public, :ordered_set])

    {:ok, %{}}
  end

  def set(_, nil), do: nil
  def set(user_id, likes) do
    # Logger.debug ">>>> likes=#{inspect likes}"
    # true = :ets.insert(:likes, {user_id, likes})
    bin = Enum.reduce(likes, <<>>, fn({id, ts}, acc) ->
      set_liked_by(id, user_id, ts)
      << id :: size(32), ts :: size(32) >> <> acc
    end)

    true = :ets.insert(:likes, {user_id, bin})

    length(likes)
  end

  # добавление в таблицу кого лайкали и кто
  # ключ=кого => значение - кто лайкал и когда.
  # liked_id - кого пролайкали, liker - кто, ts - когда
  def set_liked_by(liked_id, liker_id, ts) do
    GenServer.cast(__MODULE__, {:set_liked_by, liked_id, liker_id, ts})
  end
  def handle_cast({:set_liked_by, liked_id, liker_id, ts}, state) do
    # new_like_bin = << liker_id :: size(32), ts :: size(32) >>
    # case :ets.lookup(:liked, liked_id) do
    #   [] ->
    #     true = :ets.insert(:liked, {liked_id, new_like_bin})
    #   [{^liked_id, likes_bin}] ->
    #     true = :ets.insert(:liked, {liked_id, new_like_bin <> likes_bin})
    # end

    {:noreply, state}
  end

  def get(user_id), do: user_likes(user_id)

  # кого лайкал этот юзер
  def user_likes(user_id) do
    case :ets.lookup(:likes, user_id) do
      [] -> nil
      [{^user_id, bin_likes}] -> _decode_bin_likes([], bin_likes)
    end
  end

  # кто лайкал этого юзера
  def user_liked(user_id) do
    case :ets.lookup(:liked, user_id) do
      [] -> nil
      [{^user_id, bin_likes}] -> _decode_bin_likes([], bin_likes)
    end
  end

  def _decode_bin_likes(arr, <<>>), do: arr
  def _decode_bin_likes(arr, <<id :: 32, ts :: 32 , tail :: binary >>) do
    _decode_bin_likes([%{id: id, ts: ts}] ++ arr, tail)
  end


end
