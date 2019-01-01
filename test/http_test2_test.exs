defmodule HttpTest2Test do
  use ExUnit.Case
  alias HttpTest2.Utils
  doctest HttpTest2

  test "unicode_to_win1251" do
    test_str1 = "abc123йцукенгшщзхъфывапролджэячсмитьбю"
    assert test_str1 == test_str1
                        |> Utils.unicode_to_win1251() 
                        |> Utils.win1251_to_unicode()

    test_str2 = "abc123ЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ"
    assert test_str2 == test_str2
                        |> Utils.unicode_to_win1251() 
                        |> Utils.win1251_to_unicode()
  end
end
