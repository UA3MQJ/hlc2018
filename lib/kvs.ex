defmodule HttpTest2.KVS do
  use GenServer
  require Logger
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
    :premium,   # - начало и конец премиального периода в системе 
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

  Record.defrecord :countrys, [
    :id,
    :name
  ]

  Record.defrecord :citys, [
    :id,
    :name
  ]

  Record.defrecord :emails, [
    :email, :user_id
  ]

  Record.defrecord :phones, [
    :phone, :user_id
  ]

  Record.defrecord :autoinc, [
    :table_id, :next_id
  ]

  def start_link do
      :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_) do
    Logger.info ">>> KVS init"
    send(self(), :init)
    {:ok, nil}
  end

  # delayed init
  def handle_info(:init, _state) do
    :observer.start()
    :timer.sleep(5000)

    accounts_fields  = Keyword.keys(accounts(accounts()))
    # Logger.info ">>> accounts_fields=#{inspect accounts_fields}"
    autoinc_fields  = Keyword.keys(autoinc(autoinc()))
    # Logger.info ">>> autoinc_fields=#{inspect autoinc_fields}"
    countrys_fields  = Keyword.keys(countrys(countrys()))
    # Logger.info ">>> countrys_fields=#{inspect countrys_fields}"
    citys_fields  = Keyword.keys(citys(citys()))
    # Logger.info ">>> citys_fields=#{inspect citys_fields}"
    emails_fields  = Keyword.keys(emails(emails()))
    # Logger.info ">>> citys_fields=#{inspect citys_fields}"
    phones_fields  = Keyword.keys(phones(phones()))
    # Logger.info ">>> citys_fields=#{inspect citys_fields}"


    # :ok = :mnesia.create_schema([node()])
    :ok = :mnesia.start()

    {:atomic, :ok} = :mnesia.create_table(:autoinc,
      [
        type: :set,
        attributes: autoinc_fields,
        ram_copies: [node()]
      ]
    )

    {:atomic, :ok} = :mnesia.create_table(:countrys,
      [
        type: :set,
        attributes: countrys_fields,
        ram_copies: [node()]
      ]
    )

    :mnesia.add_table_index(:countrys, :name)

    {:atomic, :ok} = :mnesia.create_table(:citys,
      [
        type: :set,
        attributes: citys_fields,
        ram_copies: [node()]
      ]
    )

    :mnesia.add_table_index(:citys, :name)
    :mnesia.add_table_index(:citys, :country_id)

    {:atomic, :ok} = :mnesia.create_table(:accounts,
      [
        type: :set,
        attributes: accounts_fields,
        # disc_copies: [node()],
        ram_copies: [node()]
        # disc_only_copies: [node()]
      ]
    )

    {:atomic, :ok} = :mnesia.create_table(:emails,
      [
        type: :set,
        attributes: emails_fields,
        # disc_copies: [node()],
        ram_copies: [node()]
        # disc_only_copies: [node()]
      ]
    )

    {:atomic, :ok} = :mnesia.create_table(:phones,
      [
        type: :set,
        attributes: phones_fields,
        # disc_copies: [node()],
        ram_copies: [node()]
        # disc_only_copies: [node()]
      ]
    )

    :mnesia.transaction(fn() ->
      :ok = :mnesia.write(autoinc(table_id: :countrys, next_id: 1))
      :ok = :mnesia.write(autoinc(table_id: :citys, next_id: 1))
    end)

    time1 = :os.system_time(:millisecond)
    # accounts = read_file_async("accounts")
    read_file_sync("accounts")
    time2 = :os.system_time(:millisecond)
    Logger.info ">>> read_file #{time2 - time1} ms"

    accounts_size = :mnesia.table_info(:accounts, :size)
    accounts_memory = :mnesia.table_info(:accounts, :memory)
    countrys_size = :mnesia.table_info(:countrys, :size)
    countrys_memory = :mnesia.table_info(:countrys, :memory)
    citys_size = :mnesia.table_info(:citys, :size)
    citys_memory = :mnesia.table_info(:citys, :memory)
    emails_size = :mnesia.table_info(:emails, :size)
    emails_memory = :mnesia.table_info(:emails, :memory)
    phones_size = :mnesia.table_info(:phones, :size)
    phones_memory = :mnesia.table_info(:phones, :memory)

    Logger.info ">>> table accounts rows=#{inspect accounts_size} memory=#{inspect accounts_memory}"
    Logger.info ">>> table countrys rows=#{inspect countrys_size} memory=#{inspect countrys_memory}"
    Logger.info ">>> table citys rows=#{inspect citys_size} memory=#{inspect citys_memory}"
    Logger.info ">>> table emails rows=#{inspect emails_size} memory=#{inspect emails_memory}"
    Logger.info ">>> table phones rows=#{inspect phones_size} memory=#{inspect phones_memory}"


    {:noreply, nil}
  end

    # :erlang.processes()
    # |> Enum.map(fn(pid) ->
    #   :erlang.garbage_collect(pid)
    # end)


  def read_file_sync(file) do
    priv_path =  "priv/data"
    file_list = Path.wildcard("#{priv_path}/#{file}_*.json") # read()
    operate_file_list(file_list, file)
    :erlang.garbage_collect(self())
  end

  def operate_file_list([], _) do
    :erlang.garbage_collect(self())
  end
  def operate_file_list([head|tail], obj_name) do
    # read_file_jiffy(head, obj_name)
    read_file_jaxon_stream(head, obj_name)
    # read_file_jaxon_streamflow(head, obj_name)

    :erlang.garbage_collect(self())

    operate_file_list(tail, obj_name)
  end

  def read_file_jaxon_stream(file_name, obj_name) do
    # Logger.info ">>> file_name=#{inspect file_name} obj_name=#{inspect obj_name}"
    time1 = :os.system_time(:millisecond)
    file_name
    |> File.stream!([], 900)
    |> Jaxon.Stream.query([:root, obj_name, :all])
    |> Enum.map(fn(user) ->
      # Logger.debug ">>> user=#{inspect user}"
      account_set(user)
    end)

    time2 = :os.system_time(:millisecond)
    Logger.info ">>> file_name=#{inspect file_name} stream load #{time2 - time1} ms"
    :erlang.garbage_collect(self())
  end

  def read_file_jaxon_streamflow(file_name, obj_name) do
    # Logger.info ">>> file_name=#{inspect file_name} obj_name=#{inspect obj_name}"
    time1 = :os.system_time(:millisecond)
    file_name
    |> File.stream!([], 900)
    |> Jaxon.Stream.query([:root, obj_name, :all])
    |> Flow.from_enumerable()
    |> Flow.map(fn(user) ->
      # Logger.debug ">>> user=#{inspect user}"
      account_set(user)
    end)
    |> Flow.run()

    time2 = :os.system_time(:millisecond)
    Logger.info ">>> file_name=#{inspect file_name} stream+flow load #{time2 - time1} ms"
    :erlang.garbage_collect(self())
  end

  # read and json decode
  def read_file_jiffy(file_name, obj_name) do
    # Logger.info ">>> file_name=#{inspect file_name}"
    time1 = :os.system_time(:millisecond)
    file_data = File.read!(file_name)
    # Logger.debug ">>>> file_data=#{inspect file_data}"
    time2 = :os.system_time(:millisecond)
    # Logger.info ">>> file_name=#{inspect file_name} read #{time2 - time1} ms"

    map = file_data
    # |> Poison.decode!()
    |> Eljiffy.decode!()
    time3 = :os.system_time(:millisecond)

    # :ok
    # map[obj_name]
    operate_data(map[obj_name])
    time4 = :os.system_time(:millisecond)
    Logger.info ">>> file_name=#{inspect file_name} read #{time2 - time1} ms; json parse #{time3 - time2} ms; store to dbase #{time4 - time3} ms"
  end

  def operate_data([]) do
    :erlang.garbage_collect(self())
  end
  def operate_data([head_user|tail]) do
    account_set(head_user)
    operate_data(tail)
  end

  def get_user(id) do
    {:atomic, acc} = :mnesia.transaction(fn() ->
      case :mnesia.read({:accounts, id}) do
        [] -> []
        res ->
          [account] = res

          acc = account_to_map(account)
          country   = acc[:country]
          city      = acc[:city]

          acc = case city do
            nil ->
              acc
            city_id ->
             [{:citys, _, city_name}] = :mnesia.read({:citys, city_id})
             Map.merge(acc, %{city: city_name})
          end

          acc = case country do
            nil ->
              acc
            country_id ->
             [{:countrys, _, country_name}] = :mnesia.read({:countrys, country_id})
             Map.merge(acc, %{country: country_name})
          end

          acc
      end
    end)
    acc
  end

  # добавить пользователя без валидации
  def account_set(user) do
    # Logger.debug ">>>>>>>>>> user=#{inspect user}"
    {:atomic, res} = :mnesia.transaction(fn() ->
      user_map = atomic_map(user)
      
      account = user_map
      |> Map.merge(%{:city_id => get_city_id(user_map[:city])})
      |> Map.merge(%{:country_id => get_country_id(user_map[:country])})
      |> map_to_account()

      # добавляем почту
      if user_map[:email] do
        email = emails(email: user_map[:email], user_id: user_map[:id])
        :ok = :mnesia.write(email)
      end

      # добавляем телефон
      if user_map[:phone] do
        phone = phones(phone: user_map[:phone], user_id: user_map[:id])
        :ok = :mnesia.write(phone)
      end

      # добавляем в accounts
      :ok = :mnesia.write(account)

      :ok
    end)

    res
  end

  def account_new(user) do
    # Logger.debug ">>>>>>>>>> user=#{inspect user}"
    {:atomic, res} = :mnesia.transaction(fn() ->
      # проверяем валидность
      user_map = atomic_map(user)
      email_not_unique = case :mnesia.read({:emails, user_map[:email]}) do
        [] -> false
        _res -> true
      end
      phone_not_unique = case :mnesia.read({:phones, user_map[:phone]}) do
        [] -> false
        _res -> true
      end
      likes_not_valid = new_likes_is_valid(user_map[:likes]) == :error
      joined_invalid = user_map[:joined]!= nil and not(is_number(user_map[:joined]))
      birth_invalid = user_map[:birth]!= nil and not(is_number(user_map[:birth]))
      premium_invalid = new_premium_is_valid(user_map[:premium]) == :error

      cond do
        user_map[:sex]==:error -> :error
        user_map[:status]==:error -> :error
        email_not_unique -> :error
        phone_not_unique -> :error
        likes_not_valid -> :error
        joined_invalid -> :error
        birth_invalid -> :error
        premium_invalid -> :error
        true ->
          # добавляем в accounts
          account_set(user)
      end      
    end)

    res
  end

  def account_update(id, data) do
    {:atomic, res} = :mnesia.transaction(fn() ->

      usr = get_user(id)
      case usr do
        [] -> :error_id
        old_data ->
          data_map = atomic_map(data)
          email_not_unique = case :mnesia.read({:emails, data_map[:email]}) do
            [] -> false
            _res -> true
          end
          phone_not_unique = case :mnesia.read({:phones, data_map[:phone]}) do
            [] -> false
            _res -> true
          end
          likes_not_valid = new_likes_is_valid(data_map[:likes]) == :error
          joined_invalid = data_map[:joined]!= nil and not(is_number(data_map[:joined]))
          birth_invalid = data_map[:birth]!= nil and not(is_number(data_map[:birth]))
          premium_invalid = new_premium_is_valid(data_map[:premium]) == :error

          cond do
            data_map[:sex]==:error -> :error
            data_map[:status]==:error -> :error
            likes_not_valid -> :error
            joined_invalid -> :error
            birth_invalid -> :error
            premium_invalid -> :error
            data_map[:phone]!=nil and old_data[:phone]!=data_map[:phone] and phone_not_unique -> :error
            data_map[:email]!=nil and old_data[:email]!=data_map[:email] and email_not_unique -> :error
            data_map[:email]!=nil and old_data[:email]!=data_map[:email] and validate_email(data_map[:email])!=:ok -> :error
            true ->
              new_data = Map.merge(old_data, data_map)
              account_set(new_data)
          end
      end
    end)

    res
  end

  def account_set_likes(likes_data) do
    {:atomic, res} = :mnesia.transaction(fn() ->
      likes_data["likes"]
      |> Enum.reduce_while(:ok, fn(like_item, _acc) ->
        likee_id = like_item["likee"]
        liker_id = like_item["liker"]
        likee_id_invalid = get_user(likee_id) == []
        liker_id_invalid = get_user(liker_id) == []
        ts_invalid = not(is_number(like_item["ts"]))
        cond do
          likee_id==nil or liker_id==nil -> {:halt, :error}
          likee_id_invalid -> {:halt, :error}
          liker_id_invalid -> {:halt, :error}
          ts_invalid -> {:halt, :error}
        true ->
          old_data = get_user(likee_id)
          old_likes = old_data[:likes] || []
          new_likes = [%{"id" => like_item["liker"], "ts" => like_item["ts"]}] ++ old_likes
          new_data = Map.merge(old_data, %{:likes => new_likes})
          res = account_set(new_data)
          {:cont, res}
        end
      end)

    end)

    res
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
  def get_city_id(city) do
    {:atomic, res} = :mnesia.transaction(fn() ->
      case city do
        nil -> nil
        city ->
          res = :mnesia.match_object({:citys, :'_', city})
          case res do
            [] ->
              [{:autoinc, :citys, new_city_id}] = :mnesia.match_object({:autoinc, :citys, :'$2'})
              :ok = :mnesia.write(citys(id: new_city_id, name: city))
              :ok = :mnesia.write(autoinc(table_id: :citys, next_id: new_city_id + 1))
              new_city_id
            [{:citys, city_id, _}] ->
              city_id
          end
      end
    end)

    res
  end

  def get_country_id(country) do
    {:atomic, res} = :mnesia.transaction(fn() ->
      case country do
        nil -> nil
        country -> 
          res = :mnesia.match_object({:countrys, :'_', country})
          case res do
            [] ->
              [{:autoinc, :countrys, new_country_id}] = :mnesia.match_object({:autoinc, :countrys, :'$2'})
              :ok = :mnesia.write(countrys(id: new_country_id, name: country))
              :ok = :mnesia.write(autoinc(table_id: :countrys, next_id: new_country_id + 1))
              new_country_id
            [{:countrys, country_id, _}] ->
              country_id
          end
      end
    end)

    res
  end

  def map_to_account(user) do
    accounts(id: user[:id], email: user[:email], fname: user[:fname],
             sname: user[:sname], phone: user[:phone], sex: user[:sex],
             birth: user[:birth], country: user[:country_id], city: user[:city_id],
             joined: user[:joined], status: user[:status], interests: user[:interests],
             premium: user[:premium], likes: user[:likes])
  end

  def account_to_map(account_rec) do
    {:accounts, id, email, fname, sname, phone,
     sex, birth, country, city, joined, status, 
     interests, premium, likes} = account_rec

     acc = %{id: id, email: email, fname: fname, sname: sname, phone: phone,
     sex: untr_sex(sex), birth: birth, country: country, city: city, joined: joined, 
     status: untr_status(status), interests: interests, premium: premium, likes: likes}

     acc
     |> Enum.filter(fn({_, v}) -> v != nil end)
     |> Enum.into(%{})
  end

  def atomic_map(map) do
     map
     |> Enum.map(fn(key_value) -> tr_kv(key_value) end)
     |> Enum.into(%{})
  end
  defp tr_kv({key, value}), do: tr_kv(key, value)
  defp tr_kv("status", status), do: {:status, tr_status(status)}
  defp tr_kv("sex", sex), do: {:sex, tr_sex(sex)}
  defp tr_kv("id", value), do: {:id, value}
  defp tr_kv("email", value), do: {:email, value}
  defp tr_kv("fname", value), do: {:fname, value}
  defp tr_kv("sname", value), do: {:sname, value}
  defp tr_kv("phone", value), do: {:phone, value}
  defp tr_kv("birth", value), do: {:birth, value}
  defp tr_kv("country", value), do: {:country, value}
  defp tr_kv("city", value), do: {:city, value}
  defp tr_kv("joined", value), do: {:joined, value}
  defp tr_kv("interests", value), do: {:interests, value}
  defp tr_kv("premium", value), do: {:premium, value}
  defp tr_kv("likes", value), do: {:likes, value}
  defp tr_kv(key, value) do
    # Logger.debug ">>> unknown key=#{inspect key}"
    {key, value}
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
