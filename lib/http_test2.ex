defmodule HttpTest2 do
  # @compile :native
  # @compile {:hipe, [:verbose, :o3]}
  require Logger
  use Application

  def start(_type, _args) do
    children = [
      Plug.Adapters.Cowboy.child_spec(:http, Http_test2.Router, [], port: 8080)
    ]

    Logger.info("Started application")
    Supervisor.start_link(children, strategy: :one_for_one)

  end
end
