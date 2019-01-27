defmodule HttpTest2.Emails do
  use GenServer
  require Logger
  alias HttpTest2.Utils

  def start_link do
      :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    # Logger.info ">>> Emails init"
    # :ets.new(:emails, [:named_table, :public, :set, {:keypos, 1}])

    :ets.new(:emails, [:named_table, :public, :ordered_set, {:keypos, 1}])
    :ets.new(:emails_inv, [:named_table, :public, :ordered_set, {:keypos, 1}])
    :ets.new(:emails_id_gen, [:named_table, :public, :ordered_set, {:keypos, 1}])
    true = :ets.insert(:emails_id_gen, {:id, 0})


    {:ok, {1, Retrieval.new(with_id: true)}}
  end

  def name_to_id(nil),  do: nil
  def name_to_id(name) do
    numstr_name = Utils.str_to_numstr(name)
    case :ets.lookup(:emails_inv, numstr_name) do
      [] -> # если такого еще нет
        # генерим новый ИД
        new_id = :ets.update_counter(:emails_id_gen, :id, {2, 1})
        # может уже кто-то добавил
        case :ets.insert_new(:emails_inv, {numstr_name, new_id}) do
          true ->
            true = :ets.insert(:emails, {new_id, numstr_name})
            new_id
          false -> # кто-то уже успел добавить
            [{^numstr_name, id}] = :ets.lookup(:emails_inv, numstr_name) # читаем полученный ИД (не факт, что наш)
            id
        end        
      [{^numstr_name, id}] -> id
    end
  end

  def id_to_name(nil), do: nil
  def id_to_name(id) do
    case :ets.lookup(:emails, id) do
      [] -> nil
      [{^id, numstr_name}] -> Utils.numstr_to_str(numstr_name)
    end
  end

end
