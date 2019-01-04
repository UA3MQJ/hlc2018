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
    :country,   # - страна проживания. Тип - unicode-строка длиной до 50 
                #   символов. Поле опционально.
    :city,      # - город проживания. Тип - unicode-строка длиной до 50 
                #   символов. Поле опционально и указывается редко. 
                #   Каждый город расположен в определённой стране.
    :joined,    # - дата регистрации в системе. Тип - timestamp с 
                #   ограничениями: снизу 01.01.2011, сверху 01.01.2018.
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

    create_db()
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
  def create_db() do
    :ets.new(:accounts, [:named_table, :public, :set, {:keypos, 1}])
    :ets.new(:citys, [:named_table, :public, :set, {:keypos, 1}])
    :ets.new(:countrys, [:named_table, :public, :set, {:keypos, 1}])
    :ets.new(:emails, [:named_table, :public, :set, {:keypos, 1}])
    :ets.new(:phones, [:named_table, :public, :set, {:keypos, 1}])
    :ets.new(:interests, [:named_table, :public, :set, {:keypos, 1}])
    :ets.new(:likes, [:named_table, :public, :bag])
  end


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
        {^id, email_id, sname, fname, phone_id, sex,
         birth, country_id, city_id, joined, status,
         interests, premium_start, premium_finish, likes} = account

         premium_val = cond do
           premium_start==nil and premium_finish==nil -> nil
           true -> %{start: premium_start, finish: premium_finish}
         end

        %{
          id: id,
          email: email_from_id(email_id),
          sname: Utils.win1251_to_unicode(sname),
          fname: Utils.win1251_to_unicode(fname),
          phone: phone_from_id(phone_id),
          sex: sex,
          birth: birth,
          country: country_from_id(country_id),
          city: city_from_id(city_id),
          joined: joined,
          status: status,
          interests: untr_interests(interests),
          premium: premium_val,
          likes: untr_likes(id, likes)
        }

    end

  end

  # добавить пользователя без валидации
  def account_set(user) do
    account = {
      user["id"],
      get_email_id(user["email"]),
      Utils.unicode_to_win1251(user["sname"]),
      Utils.unicode_to_win1251(user["fname"]),
      get_phone_id(user["phone"]),
      tr_sex(user["sex"]),
      user["birth"],
      get_country_id(user["country"]),
      get_city_id(user["city"]),
      user["joined"],
      tr_status(user["status"]),
      tr_interests(user["interests"]),
      user["premium"]["start"],
      user["premium"]["finish"],
      tr_likes(user["id"], user["likes"])
    }

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

  # city
  def get_city_id(nil), do: nil
  def get_city_id(name) do
    case Citys.get_id(name) do
      {:new, id} ->
        true = :ets.insert(:citys, {id, Utils.unicode_to_win1251(name)})
        id
      {:old, id} ->
        id
    end
    # trie = Citys.get_trie()
    # case Retrieval.contains?(trie, name) do
    #   false ->
    #     case Citys.get_id(name) do
    #       {:new, id} ->
    #         true = :ets.insert(:citys, {id, Utils.unicode_to_win1251(name)})
    #         id
    #       {:old, id} ->
    #         id
    #     end
    #   id ->
    #     id
    # end
  end

  def city_from_id(nil), do: nil
  def city_from_id(id) do
    case :ets.lookup(:citys, id) do
      [] -> nil
      [{^id, name}] -> Utils.win1251_to_unicode(name)
    end
  end

  # country
  def get_country_id(nil), do: nil
  def get_country_id(name) do
    case Countrys.get_id(name) do
      {:new, id} ->
        true = :ets.insert(:countrys, {id, Utils.unicode_to_win1251(name)})
        id
      {:old, id} ->
        id
    end
    # trie = Countrys.get_trie() # передать дерево в процесс
    # case Retrieval.contains?(trie, name) do
    #   false ->
    #     case Countrys.get_id(name) do
    #       {:new, id} ->
    #         true = :ets.insert(:countrys, {id, Utils.unicode_to_win1251(name)})
    #         id
    #       {:old, id} ->
    #         id
    #     end
    #   id ->
    #     id
    # end
  end

  def country_from_id(nil), do: nil
  def country_from_id(id) do
    case :ets.lookup(:countrys, id) do
      [] -> nil
      [{^id, name}] -> Utils.win1251_to_unicode(name)
    end
  end

  # email
  def get_email_id(nil), do: nil
  def get_email_id(name) do
    # Emails.get_id(name)
    Utils.unicode_to_win1251(name)
  end

  def email_from_id(nil), do: nil
  def email_from_id(id) do
    Utils.win1251_to_unicode(id)
  end

  # phone
  def get_phone_id(nil), do: nil
  def get_phone_id(name) do
    Phones.get_id(name)
    Utils.unicode_to_win1251(name)
  end

  def phone_from_id(nil), do: nil
  def phone_from_id(id) do
    Utils.win1251_to_unicode(id)
  end

  defp tr_interests(nil), do: nil
  defp tr_interests(interests) do
    interests
    |> Enum.map(fn(interest) -> get_interest_id(interest) end)
  end

  def untr_interests(nil), do: nil
  def untr_interests(interests) do
    interests
    |> Enum.map(fn(interest_id) -> interest_from_id(interest_id) end)
  end

  # interests
  def get_interest_id(nil), do: nil
  def get_interest_id(name) do
    case Interests.get_id(name) do
      {:new, id} ->
        true = :ets.insert(:interests, {id, Utils.unicode_to_win1251(name)})
        id
      {:old, id} ->
        id
    end
  end

  def interest_from_id(nil), do: nil
  def interest_from_id(id) do
    case :ets.lookup(:interests, id) do
      [] -> nil
      [{^id, name}] -> Utils.win1251_to_unicode(name)
    end
  end

# ------------------------------------------------------------------------
  def transform_map(map) do
     map
     |> Enum.map(fn(key_value) -> tr_kv(key_value) end)
     |> Enum.into(%{})
  end
  defp tr_kv({key, value}), do: tr_kv(key, value)
  defp tr_kv("status", status), do: {:status, tr_status(status)}
  defp tr_kv("sex", sex), do: {:sex, tr_sex(sex)}
  defp tr_kv("id", value), do: {:id, value}
  defp tr_kv("email", value), do: {:email, Utils.unicode_to_win1251(String.downcase(value))}
  defp tr_kv("fname", value), do: {:fname, Utils.unicode_to_win1251(value)}
  defp tr_kv("sname", value), do: {:sname, Utils.unicode_to_win1251(value)}
  defp tr_kv("phone", value), do: {:phone, Utils.unicode_to_win1251(value)}
  defp tr_kv("birth", value), do: {:birth, value}
  defp tr_kv("country", value), do: {:country, Utils.unicode_to_win1251(value)}
  defp tr_kv("city", value), do: {:city, Utils.unicode_to_win1251(value)}
  defp tr_kv("joined", value), do: {:joined, value}
  defp tr_kv("interests", value) do
    new_value = value
    |> Enum.map(fn(interest) ->
      Utils.unicode_to_win1251(interest)
    end)
    {:interests, new_value}
  end
  defp tr_kv("premium", value) do
    {:premium, %{start: value["start"], finish: value["finish"]}}
  end
  defp tr_kv("likes", value) do
    new_value = value
    |> Enum.map(fn(like) ->
      {like["id"], like["ts"]}
    end)
    {:likes, new_value}
  end
  defp tr_kv(key, value) do
    # Logger.debug ">>> unknown key=#{inspect key}"
    {key, value}
  end

  defp tr_likes(_, nil), do: nil
  defp tr_likes(user_id, likes) do
    new_likes = likes
    |> Enum.reduce(<<>>, fn(like, acc) ->
      id = like["id"]
      ts = like["ts"]
      << id :: 32, ts :: 32>> <> acc
    end)
    :ok = Likes.set_likes(user_id, new_likes)
    :likes
  end

  def untr_likes(_, nil), do: nil
  def untr_likes(user_id, likes) do
    likes = Likes.get_likes(user_id)
    _untr_likes([], likes)
    # |> Enum.map(fn({id, ts}) -> %{id: id, ts: ts} end)
  end
  def _untr_likes(arr, <<>>), do: arr
  def _untr_likes(arr, <<id :: 32, ts :: 32 , tail :: binary >>) do
    _untr_likes([%{id: id, ts: ts}] ++ arr, tail)
  end

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
