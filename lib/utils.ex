defmodule HttpTest2.Utils do

  def unicode_to_win1251(nil), do: nil
  def unicode_to_win1251(str) do
    res = str
    |> to_charlist()
    |> Enum.map(fn(x) ->
      unicode_to_win1251_tr(x)
    end)
   # Logger.debug ">>>> str=#{inspect str} res=#{inspect res}"
    :erlang.list_to_binary(res)
  end
  defp unicode_to_win1251_tr(x) do
    # see http://wm-school.ru/html/html_win-1251.html
    cond do
      x == 1025 -> 168  # Ё(1025) -> 168
      x == 1105 -> 184  # ё(1105) -> 184
      x >= 1072 and x <= 1103 -> 224 + (x - 1072)  # 1072 .. 1103  -> а(224) .. я(255)
      x >= 1072 and x <= 1103 -> 224 + (x - 1072)  # 1072 .. 1103  -> а(224) .. я(255)
      x >= 1040 and x <= 1071 -> 192 + (x - 1040)  # 1040 .. 1071  -> А(192) .. Я(223)
      true -> x
    end
  end

  def win1251_to_unicode(nil), do: nil
  def win1251_to_unicode(str) do
    str
    |> :erlang.binary_to_list()
    |> Enum.map(fn(x) -> win1251_to_unicode_tr(x) end)
    |> to_string()
  end
  defp win1251_to_unicode_tr(x) do
    # see http://wm-school.ru/html/html_win-1251.html
    cond do
      x == 168 -> 1025  # Ё(1025) <- 168
      x == 184 -> 1105  # ё(1105) <- 184
      x >= 224 and x <= 255 -> 1072 + (x - 224)  # 1072 .. 1103  <- а(224) .. я(255)
      x >= 192 and x <= 223 -> 1040 + (x - 192)  # 1040 .. 1071  <- А(192) .. Я(223)
      true -> x
    end
  end

end
