defmodule ArrTest do
  require Logger

  # new
  # arr = ArrTest.new(1024, 1024*1024); nil - выделит гигабайт
  # выделилось 1048772 кБ в binary alloc (1024.191 мБ)
  # время      696'268'029 ns - 0,696 секунд

  # get
  # data = arr |> ArrTest.get(1024, 1000);nil
  # get - avg        1'800 ns - 0,000'001'800 секунд

  # set
  # new_data = :binary.copy(<<255>>, 1024)
  # arr = arr |> ArrTest.set(new_data, 666_666); nil
  # crash dump

  # new
  # arr = ArrTest.new(128, 1024*1024); nil
  # время       89'672'706 ns - 0,090 секунд

  # get
  # data = arr |> ArrTest.get(128, 1000);nil
  # get - avg        1'800 ns - 0,000'001'800 секунд

  # set
  # new_data = :binary.copy(<<255>>, 128)
  # arr = arr |> ArrTest.set(new_data, 666_666); nil
  # set - avg        1'600 ns - 0,000'001'600 секунд

  def new(element_size, element_count) when is_number(element_size) and is_number(element_count) do
    a=:erlang.monotonic_time(:nanosecond) # - 1/1000000000 секунды
    result = :binary.copy(:binary.copy(<<0>>, element_size), element_count)
    b=:erlang.monotonic_time(:nanosecond) # - 1/1000000000 секунды
    Logger.debug ">>>>>> new time #{b-a} ns"
    result
  end
  def new(binary_element, element_count) when is_binary(binary_element) and is_number(element_count) do
    a=:erlang.monotonic_time(:nanosecond) # - 1/1000000000 секунды
    result = :binary.copy(binary_element, element_count)
    b=:erlang.monotonic_time(:nanosecond) # - 1/1000000000 секунды
    Logger.debug ">>>>>> new time #{b-a} ns"
    result
  end

  def get(<<bin :: binary>>, element_size, position) do
    a=:erlang.monotonic_time(:nanosecond) # - 1/1000000000 секунды
    bin_size = byte_size(bin)
    head_size = position * element_size
    tail_size = bin_size - head_size - element_size

    << _head :: bytes-size(head_size),
       element :: bytes-size(element_size),
       _tail :: bytes-size(tail_size) >> = bin

    b=:erlang.monotonic_time(:nanosecond) # - 1/1000000000 секунды
    Logger.debug ">>>>>> get time #{b-a} ns"

    element
  end

  def set(<<bin :: binary>>, <<element :: binary>>, position) do
    a=:erlang.monotonic_time(:nanosecond) # - 1/1000000000 секунды
    bin_size = byte_size(bin)
    element_size = byte_size(element)
    head_size = position * element_size
    tail_size = bin_size - head_size - element_size

    << head :: bytes-size(head_size),
       _old_element :: bytes-size(element_size),
       tail :: bytes-size(tail_size) >> = bin

    b=:erlang.monotonic_time(:nanosecond) # - 1/1000000000 секунды
    Logger.debug ">>>>>> set time #{b-a} ns"

    head <> element <> tail
  end

end
