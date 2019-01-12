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
    # :observer.start()
    # :timer.sleep(5000)

    time1 = :os.system_time(:millisecond)

    read_file_sync2("accounts")
    Accounts.sort_ids()

    time2 = :os.system_time(:millisecond)
    IO.puts ">>> read_file #{time2 - time1} ms"

    [now_time_str, type] = File.read!("priv/data/options.txt") |> String.split("\n")
    
    now_time = case Integer.parse(now_time_str) do
      {intVal, ""} -> intVal
      :error -> nil
    end

    IO.puts ">>> now_time=#{inspect now_time}"

    :erlang.processes()
    |> Enum.map(fn(pid) ->
      :erlang.garbage_collect(pid)
    end)

    IO.puts "* System info *"
    :erlang.memory
    |> Enum.map(fn({k,v}) ->
      IO.puts " - #{inspect k} - #{inspect v}"
    end)

    IO.puts "* ETS info *"
    table_stat(:accounts)
    table_stat(:citys)
    table_stat(:countrys)
    table_stat(:interests)
    table_stat(:likes)

    # отключить логгер
    # Logger.remove_backend(:console)
    # Logger.add_backend(:console)

    {:noreply, %{now_time: now_time, type: type}}
  end

  defp table_stat(name) do
    size = :ets.info(name, :memory) * :erlang.system_info(:wordsize)
    rows = :ets.info(name, :size)
    IO.puts " - table #{inspect name} rows #{inspect rows} size #{inspect size} bytes"
  end

    # :erlang.processes()
    # |> Enum.map(fn(pid) ->
    #   :erlang.garbage_collect(pid)
    # end)

  def read_file_sync2(file) do
    priv_path =  "priv/data"
    file_list = Path.wildcard("#{priv_path}/#{file}_*.json") # read()

    # Logger.debug ">>>> file_list=#{inspect file_list}"


    time10 = :os.system_time(:millisecond)
    
    # file_list
    # |> Flow.from_enumerable(stages: 4, max_demand: 1)
    # |> Flow.flat_map(fn(json_file_name) ->
    #     time1 = :os.system_time(:millisecond)
        
    #     file_name = String.slice(json_file_name, 0..-5) <> "bin"

    #     port = Port.open({:spawn_executable, "./src/json_reader/jsonreader/jsonreader"},
    #                      [:binary, :stream, :exit_status, args: [json_file_name, file_name]])

    #     exit_status = receive do
    #       {^port, {:exit_status, exit_status}} ->
    #         #Port.close(port)
    #         # Logger.debug ">>> exit_status 0"
    #         exit_status
    #     end
        
    #     send(port, {self(), :close})

    #     time2 = :os.system_time(:millisecond)
    #     # Logger.debug ">>> json->bin json_file_name=#{inspect json_file_name} read #{time2 - time1} ms"

    #     [exit_status]
    # end)
    # |> Flow.run()
    
    time20 = :os.system_time(:millisecond)
    IO.puts ">>> json->bin convert #{time20 - time10} ms"

    # ------------------------------------------------------

    # https://elixirforum.com/t/flow-stages-from-flow-map/16845
    time10 = :os.system_time(:millisecond)

    file_list
    |> Flow.from_enumerable(stages: 4, max_demand: 1)
    |> Flow.flat_map(fn(json_file_name) ->
        time1 = :os.system_time(:millisecond)
        
        file_name = String.slice(json_file_name, 0..-5) <> "bin"

        # чтение файла 
        file_name
        {:ok, file} = File.open(file_name, [:read, :binary])
        binary = IO.binread(file, :all)
        File.close(file)

        # Logger.debug ">>> file_name=#{inspect file_name} json_file_name=#{inspect json_file_name}"
        parse_bin(binary)

        time2 = :os.system_time(:millisecond)
        # Logger.debug ">>> bin file file_name=#{inspect file_name} read #{time2 - time1} ms"

        [file_name]
    end)
    |> Flow.run()

    time20 = :os.system_time(:millisecond)
    IO.puts ">>> flow read #{time20 - time10} ms"

  end

  def parse_bin(<<>>), do: :ok
  def parse_bin(binary) do
        << id :: size(32), 
           email_size :: size(8), email :: bytes-size(email_size), 
           sname_size :: size(8), sname :: bytes-size(sname_size), 
           fname_size :: size(8), fname :: bytes-size(fname_size), 
           phone_size :: size(8), phone :: bytes-size(phone_size), 
           sex :: size(8), 
           birth :: size(32), 
           country_size :: size(8), country :: bytes-size(country_size), 
           city_size :: size(8), city :: bytes-size(city_size), 
           joined :: size(32), 
           status :: size(8), 
           tail :: binary >> = binary
            # account_set(user)

        email = parse_str(email_size, email)
        sname = parse_str(sname_size, sname)
        fname = parse_str(fname_size, fname)
        phone = parse_str(phone_size, phone)
        sex = case sex==0 do
          true -> :m
          false -> :f
        end
        country = parse_str(country_size, country)
        city = parse_str(city_size, city)

        {interests, tail2} = parse_interests(tail)

        << premium_start :: size(32),
           premium_finish :: size(32), tail3 :: binary >> = tail2

        premium_start = if premium_start==0, do: nil, else: premium_start
        premium_finish = if premium_finish==0, do: nil, else: premium_finish

        {likes, tail4} = parse_likes(tail3)

        # email = nil
        # sname = nil
        # fname = nil
        # phone = nil
        # interests = nil
        likes = nil

        # все поля при отдаче в хранилище должны быть в unicode
        # и ответы тоже
        city = city |> Utils.win1251_to_unicode()
        country = country |> Utils.win1251_to_unicode()

        email = email |> Utils.win1251_to_unicode()
        sname = sname |> Utils.win1251_to_unicode()
        fname = fname |> Utils.win1251_to_unicode()
        phone = phone |> Utils.win1251_to_unicode()

        account = %{
          id: id,
          email: email, sname: sname, fname: fname,
          phone: phone, sex: sex, country: country, city: city,
          interests: interests, likes: likes,
          status: status, birth: birth, joined: joined,
          premium_start: premium_start, premium_finish: premium_finish
        }
        account_set_bin(account)

        # # ---------------------------------------------------------
        # u_email = email |> HttpTest2.Utils.win1251_to_unicode()
        # u_sname = sname |> HttpTest2.Utils.win1251_to_unicode()
        # u_fname = fname |> HttpTest2.Utils.win1251_to_unicode()
        # u_phone = phone |> HttpTest2.Utils.win1251_to_unicode()
        # u_country = country |> HttpTest2.Utils.win1251_to_unicode()
        # u_city = city |> HttpTest2.Utils.win1251_to_unicode()


        # u_interests = case interests do
        #   nil -> nil
        #   interests ->
        #     Enum.map(interests, fn(win) ->
        #       HttpTest2.Utils.win1251_to_unicode(win)
        #     end)
        # end

        # # Logger.debug ">>>> email=#{inspect u_email} sname=#{inspect u_sname} fname=#{inspect u_fname} phone=#{inspect u_phone} sex=#{inspect sex} birth=#{inspect birth} country=#{inspect u_country} city=#{inspect u_city} joined=#{inspect joined}  status=#{inspect status}  interests=#{inspect u_interests}  premium_start=#{inspect premium_start}  premium_finish=#{inspect premium_finish} likes=#{inspect likes}"
        # Logger.debug ">>>> id=#{inspect id}  likes=#{inspect likes}"
    
    parse_bin(tail4)
  end
  def parse_str(0, str), do: nil
  def parse_str(_, str), do: str

  def parse_interests(<<0 :: size(8), tail :: binary >>), do: {nil, tail}
  def parse_interests(<<interests_count :: size(8), tail :: binary >>) do
    _parse_interests([], interests_count, tail)
  end
  defp _parse_interests(arr, 0, tail), do: {arr, tail}
  defp _parse_interests(arr, interests_count, tail) do
    << interest_size :: size(8), interest :: bytes-size(interest_size), new_tail :: binary >> = tail
    _parse_interests([interest] ++ arr, interests_count - 1, new_tail)
  end

  def parse_likes(<<0 :: size(8), tail :: binary >>), do: {nil, tail}
  def parse_likes(<<likes_count :: size(8), tail :: binary >>) do
    _parse_likes(<<>>, likes_count, tail)
  end
  defp _parse_likes(arr, 0, tail), do: {arr, tail}
  defp _parse_likes(arr, likes_count, tail) do
    << like :: bytes-size(8), new_tail :: binary >> = tail
    _parse_likes(like <> arr, likes_count - 1, new_tail)
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
          email: Utils.numstr_to_str(email),
          sname: Utils.numstr_to_str(sname),
          fname: Utils.numstr_to_str(fname),
          phone: Utils.numstr_to_str(phone),
          sex: sex,
          birth: birth,
          country: Countrys.id_to_name(country_id),
          city: Citys.id_to_name(city_id),
          joined: joined,
          status: untr_status(status),
          interests: Interests.ids_to_names(interests),
          premium: premium_val,
          likes: Likes.get(id)
        }

    end

  end

  # добавить пользователя без валидации
  def account_set_bin(user) do
    account = {
      user[:id],
      Utils.str_to_numstr(user[:email]),
      Utils.str_to_numstr(user[:sname]),
      Utils.str_to_numstr(user[:fname]),
      Utils.str_to_numstr(user[:phone]),
      user[:sex],
      user[:birth],
      Countrys.name_to_id(user[:country]),
      Citys.name_to_id(user[:city]),
      user[:joined],
      user[:status],
      Interests.names_to_ids(user[:interests]),
      user[:premium_start],
      user[:premium_finish],
      Likes.set(user[:id], user[:likes])
    }

    Accounts.set(user[:id], account)

    :ok
  end

  def account_set(user) do
    account = {
      user["id"],
      Utils.unicode_to_win1251_list(user["email"]),
      Utils.unicode_to_win1251_list(user["sname"]),
      Utils.unicode_to_win1251_list(user["fname"]),
      Utils.unicode_to_win1251_list(user["phone"]),
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

end
