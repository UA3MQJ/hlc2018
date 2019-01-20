defmodule HttpTest2.Likes do
  use GenServer
  require Logger
  require Record

  Record.defrecord :likes, [:id, :bin]
  Record.defrecord :liked, [:id, :bin]

  def start_link do
      :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    # Logger.info ">>> Likes init"
    :ok = :mnesia.start()

    {:atomic, :ok} = :mnesia.create_table(:likes,
      [
        type: :set,
        attributes: Keyword.keys(likes(likes())),
        ram_copies: [node()]
      ]
    )

    {:atomic, :ok} = :mnesia.create_table(:liked,
      [
        type: :set,
        attributes: Keyword.keys(likes(likes())),
        ram_copies: [node()]
      ]
    )


    # :ets.new(:likes, [:named_table, :public, :ordered_set])
    # :ets.new(:liked, [:named_table, :public, :ordered_set])

    {:ok, %{}}
  end

  def set(_, nil), do: nil
  def set(user_id, likes) do
    bin = Enum.reduce(likes, <<>>, fn({id, ts}, acc) ->
      set_liked_by(id, user_id, ts)
      << id :: size(32), ts :: size(32) >> <> acc
    end)

    # true = :ets.insert(:likes, {user_id, bin})
    new_likes = likes(id: user_id, bin: bin)
    {:atomic, res} = :mnesia.transaction(fn() ->
      :ok = :mnesia.write(new_likes)
    end)

    length(likes)
  end

  # добавление в таблицу кого лайкали и кто
  # ключ=кого => значение - кто лайкал и когда.
  # liked_id - кого пролайкали, liker - кто, ts - когда
  def set_liked_by(liked_id, liker_id, ts) do
    # GenServer.cast(__MODULE__, {:set_liked_by, liked_id, liker_id, ts})
    new_like_bin = << liker_id :: size(32), ts :: size(32) >>
    {:atomic, res} = :mnesia.transaction(fn() ->
      case :mnesia.read({:liked, liked_id}) do
        [] ->
          new_liked = liked(id: liked_id, bin: new_like_bin)
          :ok = :mnesia.write(new_liked)
        res ->
          [{:liked, _id, liked_bin}] = res
          new_liked = liked(id: liked_id, bin: new_like_bin <> liked_bin)
          :ok = :mnesia.write(new_liked)
      end
      
    end)
  end

  def get(user_id), do: user_likes(user_id)

  # кого лайкал этот юзер
  def user_likes(user_id) do
    # case :ets.lookup(:likes, user_id) do
    #   [] -> nil
    #   [{^user_id, bin_likes}] -> _decode_bin_likes([], bin_likes)
    # end
  end

  # кто лайкал этого юзера
  def user_liked(user_id) do
    # case :ets.lookup(:liked, user_id) do
    #   [] -> nil
    #   [{^user_id, bin_likes}] -> _decode_bin_likes([], bin_likes)
    # end
  end

  def _decode_bin_likes(arr, <<>>), do: arr
  def _decode_bin_likes(arr, <<id :: 32, ts :: 32 , tail :: binary >>) do
    _decode_bin_likes([%{id: id, ts: ts}] ++ arr, tail)
  end


end
