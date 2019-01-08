defmodule HttpTest2 do
  # @compile :native
  # @compile {:hipe, [:verbose, :o3]}
  require Logger
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    
    port = Application.get_env(:http_test2, :cowboy_port, 80)

    dispatch = :cowboy_router.compile([
      {:_,
       [
         # {"/accounts/filter", AccountsFilterHandler, []}
         {:_, AccountsFilterHandler, []}
       ]}
    ])

# https://ninenines.eu/docs/en/cowboy/2.6/manual/cowboy_http/
# https://ninenines.eu/docs/en/ranch/1.7/manual/ranch_tcp/

    {:ok, _} = :cowboy.start_clear(
      :my_http_listener,
      [{:port, port}],
      %{:env => %{:dispatch => dispatch},
        :max_connections => :infinity,
        :num_acceptors => 1,
        :idle_timeout => 600_001,
        :max_keepalive => 500_000 }
    )

    children = [
      # worker(HttpTest2.Citys, []),
      # worker(HttpTest2.Countrys, []),
      # worker(HttpTest2.Emails, []),
      # worker(HttpTest2.Phones, []),
      # worker(HttpTest2.Interests, []),
      # worker(HttpTest2.Likes, []),
      # worker(HttpTest2.Accounts, []),
      # worker(HttpTest2.KVS, []),
      # Plug.Adapters.Cowboy.child_spec(:http, Http_test2.Router, [], port: port)
    ]

    # Logger.info(">>> Started application")
    IO.puts ">>> Started application"

    # spawn(fn() ->
    #   :timer.sleep(2000)
    #   {res, _} = System.cmd("wrk", ["-d10s", "-t16", "-c16", "--timeout", "10s", "-s", "./test/wrk/test2.lua", "http://127.0.0.1:80"])
    #   IO.puts res
    # end)

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
