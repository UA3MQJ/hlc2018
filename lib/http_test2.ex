defmodule HttpTest2 do
  # @compile :native
  # @compile {:hipe, [:verbose, :o3]}
  require Logger
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    
    port = Application.get_env(:http_test2, :cowboy_port, 80)

    children = [
      worker(HttpTest2.Citys, []),
      worker(HttpTest2.Countrys, []),
      worker(HttpTest2.Emails, []),
      worker(HttpTest2.Phones, []),
      worker(HttpTest2.Interests, []),
      worker(HttpTest2.Likes, []),
      worker(HttpTest2.Accounts, []),
      worker(HttpTest2.KVS, []),
      Plug.Adapters.Cowboy.child_spec(:http, Http_test2.Router, [], port: port)
    ]

    Logger.info(">>> Started application")
    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
