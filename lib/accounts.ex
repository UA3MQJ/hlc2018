defmodule HttpTest2.Accounts do
  use GenServer
  require Logger
  alias HttpTest2.Filters
  require Qlc
  require Record

  Record.defrecord :accounts, [
    :id,        # - уникальный внешний идентификатор пользователя. 
                #   Устанавливается тестирующей системой и используется 
                #   затем, для проверки ответов сервера. Тип - 32-разрядное
                #   целое число.
    :email,     # - адрес электронной почты пользователя. 
                #   Тип - unicode-строка длиной до 100 символов. 
                #   Гарантируется уникальность.
    :fname,     # - имя и фамилия соответственно. Тип - unicode-строки 
    :sname,     #   длиной до 50 символов. Поля опциональны и могут 
                #   отсутствовать в конкретной записи.
    :phone,     # - номер мобильного телефона. Тип - unicode-строка длиной 
                #   до 16 символов. Поле является опциональным, но для 
                #   указанных значений гарантируется уникальность.
                #   Заполняется довольно редко.
    :sex,       # - unicode-строка "m" означает мужской пол, а "f" - женский.
    :birth,     # - дата рождения, записанная как число секунд от начала 
                #   UNIX-эпохи по UTC (другими словами - это timestamp). 
                #   Ограничено снизу 01.01.1950 и сверху 01.01.2005-ым.
    :birth_year,
    :country,   # - страна проживания. Тип - unicode-строка длиной до 50 
                #   символов. Поле опционально.
    :city,      # - город проживания. Тип - unicode-строка длиной до 50 
                #   символов. Поле опционально и указывается редко. 
                #   Каждый город расположен в определённой стране.
    :joined,    # - дата регистрации в системе. Тип - timestamp с 
                #   ограничениями: снизу 01.01.2011, сверху 01.01.2018.
    :joined_year,
    :status,    # - текущий статус пользователя в системе. Тип - одна строка 
                #   из следующих вариантов: "свободны", "заняты", "всё сложно". 
                #   Не обращайте внимание на странные окончания :)
    :interests, # - интересы пользователя в обычной жизни. 
                #   Тип - массив unicode-строк, возможно пустой. Строки не 
                #   превышают по длине 100 символов.
    :premium_start,   
    :premium_finish,   
                # - начало и конец премиального периода в системе 
                #   (когда пользователям очень хотелось найти "вторую 
                #   половинку" и они делали денежный вклад). В json это поле 
                #   представлено вложенным объектом с полями start и finish, 
                #   где записаны timestamp-ы с нижней границей 01.01.2018.
    :likes      # - массив известных симпатий пользователя, возможно пустой. 
                #   Все симпатии идут вразнобой и каждая представляет собой 
                #   объект из следующих полей:
                #     id - идентификатор другого аккаунта, к которому симпатия. Аккаунт по id в исходных данных всегда существует. В данных может быть несколько лайков с одним и тем же id.
                #     ts - время, то есть timestamp, когда симпатия была записана в систему.
  ]

  def qlc1() do
    # qlc_handle =  Qlc.q("[ element(1, P) || P <- ets:table(citys)]", [])  
    # qlc_handle =  Qlc.q("[ {element(1, P), element(2, P)} || P <- mnesia:table(accounts)]", [])  
    # f = fn() ->
    #   Qlc.e(qlc_handle)
    # end
    # :mnesia.transaction(f)
    # accounts(accounts())

    f1 = fn() ->
      qlc_handle = :qlc.string_to_handle('[ {element(1, P), element(2, P)} || P <- mnesia:table(accounts), element(2, P) > (30000 - 100)].', [])
      :qlc.eval(qlc_handle)
    end

    order_fn = fn(a, b) ->
      :erlang.element(2, a) > :erlang.element(2, b)
    end

    f2 = fn() ->
      qlc_handle = :qlc.string_to_handle('[ {element(1, P), element(2, P)} || P <- mnesia:table(accounts), element(2, P) > (30000 - 100)].', [])
      :qlc.eval(:qlc.sort(qlc_handle, [{:order, order_fn}]))
    end

    f3 = fn() ->
      qlc_handle = :qlc.string_to_handle('[ P || P <- gb_table:table(gb_trees:empty())].', [])
      :qlc.info(qlc_handle)
    end


    :mnesia.transaction(f3)
  end

  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    # Logger.info ">>> accounts init"
    # :ets.new(:accounts, [:named_table, :public, :ordered_set, {:keypos, 1}])
    :ets.new(:index, [:named_table, :public, :ordered_set, {:keypos, 1}])
    :ets.new(:groups, [:named_table, :public, :ordered_set, {:keypos, 1}])

    {:atomic, :ok} = :mnesia.create_table(:accounts,
      [
        type: :ordered_set,
        attributes: Keyword.keys(accounts(accounts())),
        ram_copies: []
      ]
    )

    :mnesia.add_table_index(:accounts, :sex)
    :mnesia.add_table_index(:accounts, :status)

    true = :ets.insert(:index, {:status, :not_ready})

    [now_time_str, _type] = File.read!("priv/data/options.txt") |> String.split("\n")
    
    now_time = case Integer.parse(now_time_str) do
      {intVal, ""} -> intVal
      :error -> nil
    end
    true = :ets.insert(:index, {:now_time, now_time})

    ids = %{id_list: [], sex_m: [], sex_f: []}
    {:ok, {%{}, ids, now_time}}
  end

  def set_id(id) do
    GenServer.cast(__MODULE__, {:set, id})
  end

  def set(id, account) do
    # true = :ets.insert(:accounts, account)
    res = :mnesia.dirty_write(account)

    # GenServer.cast(__MODULE__, {:set, id})
    # sex = :erlang.element(6, account)
    # case sex do
    #   :m -> GenServer.cast(__MODULE__, {:set_sex_m, id})
    #   :f -> GenServer.cast(__MODULE__, {:set_sex_f, id})
    # end
  end


  def sort_ids() do
    GenServer.call(__MODULE__, :sort_ids)
  end

  def get(id) do
    case :mnesia.dirty_read(:accounts, id) do
      [] -> nil
      [account] -> account
    end
  end

  def get_id_list() do
    case :ets.lookup(:index, :status) do
      [{:status, :not_ready}] -> sort_ids()
      [{:status, :ready}] -> :ok
    end

    case :ets.lookup(:index, :sort_ids) do
      [] -> nil
      [{:sort_ids, id_list}] -> id_list
    end    
  end

  def get_sex_m() do
    case :ets.lookup(:index, :status) do
      [{:status, :not_ready}] -> sort_ids()
      [{:status, :ready}] -> :ok
    end

    case :ets.lookup(:index, :sex_m) do
      [] -> nil
      [{:sex_m, id_list}] -> id_list
    end    
  end

  def get_sex_f() do
    case :ets.lookup(:index, :status) do
      [{:status, :not_ready}] -> sort_ids()
      [{:status, :ready}] -> :ok
    end

    case :ets.lookup(:index, :sex_f) do
      [] -> nil
      [{:sex_f, id_list}] -> id_list
    end    
  end
  def get_now_time() do
    case :ets.lookup(:index, :now_time) do
      [] -> nil
      [{:now_time, now_time}] -> now_time
    end    
  end

  def filter(params) do
    Filters.filter(params)
  end


  def handle_cast({:set, id}, {map, %{id_list: id_list} = ids, now} = _state) do
    true = :ets.insert(:index, {:status, :not_ready})
    new_id_list = [id] ++ id_list
    {:noreply, {map, %{ids | id_list: new_id_list}, now}}
  end

  def handle_cast({:set_sex_m, id}, {map, %{sex_m: sex_m} = ids, now} = _state) do
    true = :ets.insert(:index, {:status, :not_ready})
    new_sex_m = [id] ++ sex_m
    {:noreply, {map, %{ids | sex_m: new_sex_m}, now}}
  end

  def handle_cast({:set_sex_f, id}, {map, %{sex_f: sex_f} = ids, now} = _state) do
    true = :ets.insert(:index, {:status, :not_ready})
    new_sex_f = [id] ++ sex_f
    {:noreply, {map, %{ids | sex_f: new_sex_f}, now}}
  end


  def handle_call(:sort_ids, _, {map, %{id_list: id_list, sex_m: sex_m, sex_f: sex_f} = ids, now} = _state) do
    time1 = :os.system_time(:millisecond)
    new_id_list = id_list |> :lists.reverse() |> :lists.sort() |> :lists.reverse()
    time2 = :os.system_time(:millisecond)

    time1 = :os.system_time(:millisecond)
    new_sex_m = sex_m |> :lists.reverse() |> :lists.sort() |> :lists.reverse()
    time2 = :os.system_time(:millisecond)

    time1 = :os.system_time(:millisecond)
    new_sex_f = sex_f |> :lists.reverse() |> :lists.sort() |> :lists.reverse()
    time2 = :os.system_time(:millisecond)

    true = :ets.insert(:index, {:sort_ids, MapSet.new(new_id_list)})
    true = :ets.insert(:index, {:sex_m, MapSet.new(new_sex_m)})
    true = :ets.insert(:index, {:sex_f, MapSet.new(new_sex_f)})
    true = :ets.insert(:index, {:status, :ready})

    {:reply, :ok, {map, %{ids | id_list: new_id_list}, now}}
  end

end
