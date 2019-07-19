defmodule HttpTest2.Mixfile do
  use Mix.Project

  def project do
    [app: :http_test2,
     version: "0.1.0",
     elixir: "~> 1.6",
     compilers: [:elixir_make] ++ Mix.compilers(),
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [applications: [:logger, :cowboy],
     mod: {HttpTest2, []}]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [
     {:cowboy, "~>2.6"},
     # {:plug, "~>1.3.4"},
     # {:plug_cowboy, "~>1.0"},
     # {:cowboy, "~>1.1.2"},
     {:poison, "~> 2.0"},
     {:eljiffy, "~> 1.1.0"},
     {:jaxon, "~> 1.0"},
     {:flow, "~> 0.14"},
     {:distillery, "~> 1.0"},
     {:exprof, "~> 0.2.3"},
     {:elixir_make, "~> 0.4", runtime: false},
     {:retrieval, git: "https://github.com/UA3MQJ/retrieval.git"},
     {:qlc, "~> 1.0"},
    ]
  end
end
