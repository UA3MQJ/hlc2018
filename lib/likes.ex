defmodule HttpTest2.Likes do
  use GenServer
  require Logger


  def start_link do
      :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    # Logger.info ">>> Likes init"
    :ets.new(:likes, [:named_table, :public, :bag])

    {:ok, %{}}
  end

  def set(_, nil), do: nil
  def set(user_id, likes) do
    {bin_likes, count} = likes
    |> Enum.reduce({<<>>, 0}, fn(like, {bin_acc, count}) ->
      id = like["id"]
      ts = like["ts"]
      {<< id :: 32, ts :: 32>> <> bin_acc, count + 1}
    end)
    true = :ets.insert(:likes, {user_id, bin_likes})
    count
  end

  def get(user_id) do
    case :ets.lookup(:likes, user_id) do
      [] -> nil
      [{^user_id, bin_likes}] -> _decode_bin_likes([], bin_likes)
    end
  end

  def _decode_bin_likes(arr, <<>>), do: arr
  def _decode_bin_likes(arr, <<id :: 32, ts :: 32 , tail :: binary >>) do
    _decode_bin_likes([%{id: id, ts: ts}] ++ arr, tail)
  end


end
