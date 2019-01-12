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
         {"/accounts/filter", AccountsFilterHandler, []},
         {"/accounts/group", AccountsGroupHandler, []},
         {"/accounts/new", AccountsNewHandler, []},
         {"/accounts/likes", AccountsLikesHandler, []},
         {"/accounts/:id/recommend", AccountsRecommendHandler, []},
         {"/accounts/:id/suggest", AccountsSuggestHandler, []},
         {"/accounts/:id", AccountsUpdateHandler, []},


         {:_, ErrorHandler, []}
       ]}
    ])

# https://ninenines.eu/docs/en/cowboy/2.6/manual/cowboy_http/
# https://ninenines.eu/docs/en/ranch/1.7/manual/ranch_tcp/

    {:ok, _} = :cowboy.start_clear(
      :my_http_listener,
      [{:port, port}, {:num_acceptors, 100}],
      %{:env => %{:dispatch => dispatch},
        :max_connections => :infinity,
        # :num_acceptors => 5,
        :idle_timeout => 600_001,
        :max_keepalive => 500_000 }
    )

    children = [
      worker(HttpTest2.Citys, []),
      worker(HttpTest2.Countrys, []),
      worker(HttpTest2.Emails, []),
      worker(HttpTest2.Phones, []),
      worker(HttpTest2.Interests, []),
      worker(HttpTest2.Likes, []),
      worker(HttpTest2.Accounts, []),
      worker(HttpTest2.KVS, []),
      # Plug.Adapters.Cowboy.child_spec(:http, Http_test2.Router, [], port: port)
    ]

    # Logger.info(">>> Started application")
    IO.puts ">>> Started application"

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
