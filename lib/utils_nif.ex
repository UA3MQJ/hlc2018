# (c) TenderPro inc., 2018
# https://tender.pro, cf@tender.pro, +7(495)215-14-38
# Authors:
# Alexander Golubov <golubov@tender.pro>

defmodule UtilsNif do
  @on_load :init

  def init() do
    nif_filename =
      :http_test2
      |> Application.app_dir("priv/utils_nif")
      |> to_charlist()
    
    :ok = :erlang.load_nif(nif_filename, 0)
  end

  def test() do
    test()
  end

  def test2() do
    666
  end

  defp utils(_, _) do
    exit(:nif_library_not_loaded)
  end

end
