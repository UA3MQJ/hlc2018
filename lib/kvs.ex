defmodule HttpTest2.KVS do
  use GenServer
  require Logger

  alias HttpTest2.Utils
  alias HttpTest2.Citys
  alias HttpTest2.Countrys
  alias HttpTest2.Phones
  alias HttpTest2.Emails
  alias HttpTest2.Interests
  alias HttpTest2.Likes
  alias HttpTest2.Accounts

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
    IO.puts ">>> read_file #{time2 - time1} ms"

    [now_time_str, type] = File.read!("priv/data/options.txt") |> String.split("\n")
    
    now_time = case Integer.parse(now_time_str) do
      {intVal, ""} -> intVal
      :error -> nil
    end

    IO.puts ">>> now_time=#{inspect now_time}"

    :ets.new(:constants, [:named_table, :public, :ordered_set, {:keypos, 1}])
    true = :ets.insert(:constants, {:now_time, now_time})

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
    ets_table_stat(:accounts)
    ets_table_stat(:citys)
    ets_table_stat(:citys_inv)
    ets_table_stat(:countrys)
    ets_table_stat(:countrys_inv)
    ets_table_stat(:interests)
    ets_table_stat(:interests_inv)
    ets_table_stat(:emails)
    ets_table_stat(:emails_inv)
    ets_table_stat(:groups)
    IO.puts "* Mnesia info *"
    mnesia_table_stat(:accounts)

    mnesia_table_stat(:likes)
    mnesia_table_stat(:liked)

    Logger.info(">>> READY")

    # отключить логгер
    # Logger.remove_backend(:console)
    # Logger.add_backend(:console)


    {:noreply, %{now_time: now_time, type: type}}
  end

  defp ets_table_stat(name) do
    size = :ets.info(name, :memory) * :erlang.system_info(:wordsize)
    rows = :ets.info(name, :size)
    IO.puts " - table #{inspect name} rows #{inspect rows} size #{inspect size} bytes"
  end

  defp mnesia_table_stat(name) do
    size = :mnesia.table_info(name, :memory) * :erlang.system_info(:wordsize)
    rows = :mnesia.table_info(name, :size)
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

        # все поля при отдаче в хранилище должны быть в unicode
        # и ответы тоже

        account = %{
          id: id,
          email: email, sname: sname, fname: fname,
          phone: phone, sex: sex, country: country, city: city,
          interests: interests, likes: likes,
          status: status, birth: birth, joined: joined,
          premium_start: premium_start, premium_finish: premium_finish
        }
        account_set_bin(account)
    
    parse_bin(tail4)
  end
  def parse_str(0, str), do: nil
  def parse_str(_, str), do: Utils.win1251_to_unicode(str)

  def parse_interests(<<0 :: size(8), tail :: binary >>), do: {nil, tail}
  def parse_interests(<<interests_count :: size(8), tail :: binary >>) do
    _parse_interests([], interests_count, tail)
  end
  defp _parse_interests(arr, 0, tail), do: {arr, tail}
  defp _parse_interests(arr, interests_count, tail) do
    << interest_size :: size(8), interest :: bytes-size(interest_size), new_tail :: binary >> = tail
    unicode_interest = Utils.win1251_to_unicode(interest)
    _parse_interests([unicode_interest] ++ arr, interests_count - 1, new_tail)
  end

  def parse_likes(<<0 :: size(8), tail :: binary >>), do: {nil, tail}
  def parse_likes(<<likes_count :: size(8), tail :: binary >>) do
    _parse_likes([], likes_count, tail)
  end
  defp _parse_likes(arr, 0, tail), do: {arr, tail}
  defp _parse_likes(arr, likes_count, tail) do
    << like :: bytes-size(8), new_tail :: binary >> = tail
    << id :: size(32), ts :: size(32) >> = like
    _parse_likes([{id, ts}] ++ arr, likes_count - 1, new_tail)
  end


  def get_user(id) do
    case Accounts.get(id) do
      nil -> nil
      account ->
        {:accounts, ^id, email, sname, fname, phone, sex,
         birth, birth_year, country_id, city_id, joined, joined_year, status,
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
          birth: birth, birth_year: birth_year,
          country: Countrys.id_to_name(country_id),
          city: Citys.id_to_name(city_id),
          joined: joined, joined_year: joined_year,
          status: untr_status(status),
          interests: Interests.ids_to_names(interests),
          premium: premium_val,
          likes: Likes.get(id)
        }

    end

  end

  # добавить пользователя без валидации
  def account_set_bin(user) do
    country_id = Countrys.name_to_id(user[:country])
    city_id = Citys.name_to_id(user[:city])
    birth_year = Utils.unix_to_year(user[:birth])
    joined_year = Utils.unix_to_year(user[:joined])
    interests_list = Interests.names_to_ids(user[:interests])
    Emails.name_to_id(user[:email])

    account = {
      :accounts,
      user[:id],
      Utils.str_to_numstr(user[:email]),
      Utils.str_to_numstr(user[:sname]),
      Utils.str_to_numstr(user[:fname]),
      Utils.str_to_numstr(user[:phone]),
      user[:sex],
      user[:birth],
      birth_year,
      country_id,
      city_id,
      user[:joined],
      joined_year,
      user[:status],
      interests_list,
      user[:premium_start],
      user[:premium_finish],
      Likes.set(user[:id], user[:likes])
    }

    Accounts.set(user[:id], account)

    # # делаем группы
    # # sex, status, interests, country, city в селект
    # # sex, status, interests, country, city, birth, joined. likes - в where
    # case interests_list do
    #   nil ->
    #     key = {user[:sex], user[:status], nil, country_id, city_id, birth_year, joined_year}
    #     :ets.update_counter(:groups, key, {2, 1}, {key, 0})
    #   interests_list ->
    #     Enum.map(interests_list, fn(interest_id) ->
    #       key = {user[:sex], user[:status], interest_id, country_id, city_id, birth_year, joined_year}
    #       :ets.update_counter(:groups, key, {2, 1}, {key, 0})
    #     end)
    # end

    :ok
  end

  def user_id_is_valid?(id) do
    result = :mnesia.dirty_read({:accounts, id})
    result != []
  end

  def account_new(user) do
    full_valid = user_id_is_valid?(user["id"])

    case !full_valid do
      true ->
        country_id = Countrys.name_to_id(user["country"])
        city_id = Citys.name_to_id(user["city"])
        birth_year = Utils.unix_to_year(user["birth"])
        joined_year = Utils.unix_to_year(user["joined"])
        interests_list = Interests.names_to_ids(user["interests"])
        Emails.name_to_id(user["email"])

        account = {
          :accounts,
          user["id"],
          Utils.str_to_numstr(user["email"]),
          Utils.str_to_numstr(user["sname"]),
          Utils.str_to_numstr(user["fname"]),
          Utils.str_to_numstr(user["phone"]),
          tr_sex(user["sex"]),
          user["birth"],
          birth_year,
          country_id,
          city_id,
          user["joined"],
          joined_year,
          tr_status(user["status"]),
          interests_list,
          user["premium"]["start"],
          user["premium"]["finish"],
          Likes.set(user["id"], user["likes"])
        }

        Accounts.set(user[:id], account)

        :ok
      _else ->
        # Logger.debug ">>>>> invalid user id=" <> inspect(user["id"])
        :error        
    end

  end

  def account_update(id, data) do
    # Logger.debug ">>>> update id=#{inspect id}"
    account = get_user(id)
    # Logger.debug ">>>> update id=#{inspect id} account=#{inspect account}"


    # Logger.debug ">>>> update data=#{inspect data}"

    email = data["email"] || account[:email]
    sname = data["sname"] || account[:sname]
    fname = data["fname"] || account[:fname]
    phone = data["phone"] || account[:phone]
    sex = case data["sex"] do
      nil -> account[:sex]
      data_sex -> tr_sex(data_sex)
    end
    birth = data["birth"] || account[:birth]
    birth_year = Utils.unix_to_year(birth)
    country = data["country"] || account[:country]
    country_id = Countrys.name_to_id(country)
    city = data["city"] || account[:city]
    city_id = Citys.name_to_id(city)
    joined = data["joined"] || account[:joined]
    joined_year = Utils.unix_to_year(joined)
    status = case data["status"] do
      nil -> tr_status(account[:status])
      data_status -> tr_status(data_status)
    end
    interests = data["interests"] || account[:interests]
    interests_list = Interests.names_to_ids(interests)
    premium_start = data["premium"]["start"] || account[:premium][:start]
    premium_finish = data["premium"]["finish"] || account[:premium][:finish]
    likes = data["likes"] || account[:likes]

    # Emails.name_to_id(user["email"])
    if data["email"]!=nil do
      em_old = account[:email]
      em_new = data["email"]
      # Logger.debug ">>>>> ch email em_old=#{inspect em_old} em_new=#{inspect em_new}"
      Emails.name_to_id(em_new)
      Emails.delete(em_old)
    end

    account = {
      :accounts,
      id,
      Utils.str_to_numstr(email),
      Utils.str_to_numstr(sname),
      Utils.str_to_numstr(fname),
      Utils.str_to_numstr(phone),
      sex,
      birth,
      birth_year,
      country_id,
      city_id,
      joined,
      joined_year,
      status,
      interests_list,
      premium_start,
      premium_finish,
      Likes.set(id, likes)
    }

    Accounts.set(id, account)

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
