defmodule HttpTest2.Interests do
  use GenServer
  require Logger
  alias HttpTest2.Utils

  def start_link do
      :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    :ets.new(:interests, [:named_table, :public, :ordered_set, {:keypos, 1}])
    :ets.new(:interests_inv, [:named_table, :public, :ordered_set, {:keypos, 1}])
    :ets.new(:interests_id_gen, [:named_table, :public, :ordered_set, {:keypos, 1}])
    true = :ets.insert(:interests_id_gen, {:id, 0})
    {:ok, {1, :gb_trees.empty()}}
  end

  def name_to_id(nil),  do: nil
  def name_to_id(name) do
    numstr_name = Utils.str_to_numstr(name)
    case :ets.lookup(:interests_inv, numstr_name) do
      [] -> # если такого еще нет
        # генерим новый ИД
        new_id = :ets.update_counter(:interests_id_gen, :id, {2, 1})
        # может уже кто-то добавил
        case :ets.insert_new(:interests_inv, {numstr_name, new_id}) do
          true ->
            true = :ets.insert(:interests, {new_id, numstr_name})
            new_id
          false -> # кто-то уже успел добавить
            [{^numstr_name, id}] = :ets.lookup(:interests_inv, numstr_name) # читаем полученный ИД (не факт, что наш)
            id
        end        
      [{^numstr_name, id}] -> id
    end
  end

  def id_to_name(nil), do: nil
  def id_to_name(id) do
    case :ets.lookup(:interests, id) do
      [] -> nil
      [{^id, numstr_name}] -> Utils.numstr_to_str(numstr_name)
    end
  end

  def names_to_ids(nil), do: nil
  def names_to_ids(names_list), do: Enum.map(names_list, fn(name) -> name_to_id(name) end)


  def ids_to_names(nil), do: nil
  def ids_to_names(id_list), do: Enum.map(id_list, fn(id) -> id_to_name(id) end)

end
