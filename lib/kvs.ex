defmodule HttpTest2.KVS do
  use GenServer
  require Logger
  require Record
  alias HttpTest2.Utils
  alias HttpTest2.Citys
  alias HttpTest2.Countrys
  alias HttpTest2.Phones
  alias HttpTest2.Emails
  alias HttpTest2.Interests
  alias HttpTest2.Likes
  alias HttpTest2.Accounts

  # Record.defrecord :accounts, [
  #   :id,        # - уникальный внешний идентификатор пользователя. 
  #               #   Устанавливается тестирующей системой и используется 
  #               #   затем, для проверки ответов сервера. Тип - 32-разрядное
  #               #   целое число.
  #   :email,     # - адрес электронной почты пользователя. 
  #               #   Тип - unicode-строка длиной до 100 символов. 
  #               #   Гарантируется уникальность.
  #   :fname,     # - имя и фамилия соответственно. Тип - unicode-строки 
  #   :sname,     #   длиной до 50 символов. Поля опциональны и могут 
  #               #   отсутствовать в конкретной записи.
  #   :phone,     # - номер мобильного телефона. Тип - unicode-строка длиной 
  #               #   до 16 символов. Поле является опциональным, но для 
  #               #   указанных значений гарантируется уникальность.
  #               #   Заполняется довольно редко.
  #   :sex,       # - unicode-строка "m" означает мужской пол, а "f" - женский.
  #   :birth,     # - дата рождения, записанная как число секунд от начала 
  #               #   UNIX-эпохи по UTC (другими словами - это timestamp). 
  #               #   Ограничено снизу 01.01.1950 и сверху 01.01.2005-ым.
  #   :country,   # - страна проживания. Тип - unicode-строка длиной до 50 
  #               #   символов. Поле опционально.
  #   :city,      # - город проживания. Тип - unicode-строка длиной до 50 
  #               #   символов. Поле опционально и указывается редко. 
  #               #   Каждый город расположен в определённой стране.
  #   :joined,    # - дата регистрации в системе. Тип - timestamp с 
  #               #   ограничениями: снизу 01.01.2011, сверху 01.01.2018.
  #   :status,    # - текущий статус пользователя в системе. Тип - одна строка 
  #               #   из следующих вариантов: "свободны", "заняты", "всё сложно". 
  #               #   Не обращайте внимание на странные окончания :)
  #   :interests, # - интересы пользователя в обычной жизни. 
  #               #   Тип - массив unicode-строк, возможно пустой. Строки не 
  #               #   превышают по длине 100 символов.
  #   :premium_start,   
  #   :premium_finish,   
  #               # - начало и конец премиального периода в системе 
  #               #   (когда пользователям очень хотелось найти "вторую 
  #               #   половинку" и они делали денежный вклад). В json это поле 
  #               #   представлено вложенным объектом с полями start и finish, 
  #               #   где записаны timestamp-ы с нижней границей 01.01.2018.
  #   :likes      # - массив известных симпатий пользователя, возможно пустой. 
  #               #   Все симпатии идут вразнобой и каждая представляет собой 
  #               #   объект из следующих полей:
  #               #     id - идентификатор другого аккаунта, к которому симпатия. Аккаунт по id в исходных данных всегда существует. В данных может быть несколько лайков с одним и тем же id.
  #               #     ts - время, то есть timestamp, когда симпатия была записана в систему.
  # ]


  def start_link do
      :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    # Logger.info ">>> KVS init * jaxon + stream + flow *"
    send(self(), :init)
    {:ok, nil}
  end

  # delayed init
  def handle_info(:init, _state) do
    :observer.start()
    # :timer.sleep(5000)

    time1 = :os.system_time(:millisecond)

    read_file_sync2("accounts")
    Accounts.sort_ids()

    time2 = :os.system_time(:millisecond)
    Logger.info ">>> read_file #{time2 - time1} ms"

    [now_time_str, type] = File.read!("priv/data/options.txt") |> String.split("\n")
    
    now_time = case Integer.parse(now_time_str) do
      {intVal, ""} -> intVal
      :error -> nil
    end

    Logger.debug ">>> now_time=#{inspect now_time}"


    {:noreply, %{now_time: now_time, type: type}}
  end

    # :erlang.processes()
    # |> Enum.map(fn(pid) ->
    #   :erlang.garbage_collect(pid)
    # end)

  def read_file_sync2(file) do
    priv_path =  "priv/data"
    file_list = Path.wildcard("#{priv_path}/#{file}_*.json") # read()

    # Logger.debug ">>>> file_list=#{inspect file_list}"


    # https://elixirforum.com/t/flow-stages-from-flow-map/16845
    time10 = :os.system_time(:millisecond)

    file_list
    |> Flow.from_enumerable(stages: 8, max_demand: 1)
    |> Flow.flat_map(fn(file_name) ->
        time1 = :os.system_time(:millisecond)
        
        # чтение файла кусками
        file_name
        |> File.stream!([], 1000)
        |> Jaxon.Stream.query([:root, "accounts", :all])
        |> Enum.map(fn(user) ->
          # Logger.debug ">>> uid=#{inspect uid}"
          account_set(user)
          # :ok
        end)

        time2 = :os.system_time(:millisecond)
        Logger.debug ">>> file file_name=#{inspect file_name} read #{time2 - time1} ms"
        [file_name]
    end)
    |> Flow.run()

    time20 = :os.system_time(:millisecond)
    Logger.debug ">>> flow read #{time20 - time10} ms"

  end

  def get_user(id) do
    case Accounts.get(id) do
      nil -> nil
      account ->
        {^id, email, sname, fname, phone, sex,
         birth, country_id, city_id, joined, status,
         interests, premium_start, premium_finish, likes} = account

         premium_val = cond do
           premium_start==nil and premium_finish==nil -> nil
           true -> %{start: premium_start, finish: premium_finish}
         end

        %{
          id: id,
          email: Utils.win1251_to_unicode(email),
          sname: Utils.win1251_to_unicode(sname),
          fname: Utils.win1251_to_unicode(fname),
          phone: Utils.win1251_to_unicode(phone),
          sex: sex,
          birth: birth,
          country: Countrys.id_to_name(country_id),
          city: Citys.id_to_name(city_id),
          joined: joined,
          status: status,
          interests: Interests.ids_to_names(interests),
          premium: premium_val,
          likes: Likes.get(id)
        }

    end

  end

  # добавить пользователя без валидации
  def account_set(user) do
    account = {
      user["id"],
      Utils.unicode_to_win1251(user["email"]),
      Utils.unicode_to_win1251(user["sname"]),
      Utils.unicode_to_win1251(user["fname"]),
      Utils.unicode_to_win1251(user["phone"]),
      tr_sex(user["sex"]),
      user["birth"],
      Countrys.name_to_id(user["country"]),
      Citys.name_to_id(user["city"]),
      user["joined"],
      tr_status(user["status"]),
      Interests.names_to_ids(user["interests"]),
      user["premium"]["start"],
      user["premium"]["finish"],
      Likes.set(user["id"], user["likes"])
    }

    # сохранить телефон и почту в trie
    # исключительно, чтоб потом выявлять уникальность.
    # _ = Phones.get_id(user["phone"])
    # _ = Emails.get_id(user["email"])

    Accounts.set(user["id"], account)

    :ok
  end

  def account_new(user) do

    :ok
  end

  def account_update(id, data) do

    :ok
  end

  def account_set_likes(likes_data) do

    :ok
  end

  def filter(params) do
    # time1 = :os.system_time(:millisecond)
    accounts = Accounts.filter(params)
    # time2 = :os.system_time(:millisecond)
    # Logger.info ">>> filter #{time2 - time1} ms"

    # {map, id_list, now} = Accounts.get_state()
    # accounts = HttpTest2.Filters.filter(params, map, id_list, now)

    case accounts do
      :error -> :error
      accounts -> %{accounts: accounts}
    end
    
  end

# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------


  defp tr_sex("m"), do: :m
  defp tr_sex("f"), do: :f
  defp tr_sex(_), do: :error
  
  defp untr_sex(:m), do: "m"
  defp untr_sex(:f), do: "f"
  defp untr_sex(_), do: nil

  defp tr_status("свободны"), do: 1
  defp tr_status("заняты"), do: 2
  defp tr_status("всё сложно"), do: 3
  defp tr_status(_), do: :error

  defp untr_status(1), do: "свободны"
  defp untr_status(2), do: "заняты"
  defp untr_status(3), do: "всё сложно"
  defp untr_status(_), do: nil

  def new_likes_is_valid(nil), do: :ok
  def new_likes_is_valid(likes) do
    result = likes
    |> Enum.reduce_while(:ok, fn(like_item, _acc) ->
      like_id = like_item["id"]
      like_id_invalid = get_user(like_id) == []
      ts_invalid = not(is_number(like_item["ts"]))
      cond do
        like_id==nil -> {:halt, :error}
        like_id_invalid -> {:halt, :error}
        ts_invalid -> {:halt, :error}
      true ->
        {:cont, :ok}
      end
    end)

    # if result==:error do
    #   Logger.debug ">>>>>> error likes=#{inspect likes}"
    # end

    result
  end

  def new_premium_is_valid(nil), do: :ok
  def new_premium_is_valid(premium) when is_binary(premium), do: :error
  def new_premium_is_valid(premium) when is_map(premium) do
    premium_start = premium["start"]
    premium_finish = premium["finish"]
    result = cond do
      premium_start == nil -> :error
      premium_finish == nil -> :error
      not(is_number(premium_start)) -> :error
      not(is_number(premium_finish)) -> :error
      true -> :ok
    end

    result
  end

  def validate_email(email) when is_binary(email) do
    case Regex.run(~r/^[A-Za-z0-9._%+-+']+@[A-Za-z0-9.-]+\.[A-Za-z]{2,4}$/, email) do
      nil ->
        {:error, "Invalid email"}
      _ ->
        :ok
    end
  end

end
