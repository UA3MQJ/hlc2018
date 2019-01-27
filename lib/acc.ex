defmodule Acc do
  require Logger
  def table(n) do
    Logger.debug ">>>> acc.table n=#{inspect n}"

    tf = fn () -> n + 1 end
    # :qlc.table(tf, [{info_fun, InfoFun},
    #                 {format_fun, FormatFun},
    #                 {lookup_fun, LookupFun},
    #                 {key_equality,'=='}])
    info_fun = fn(key) ->
      case key do
        :num_of_objects -> 100
        :keypos -> 1
        :is_sorted_key -> true
        :is_unique_objects -> true
        _ -> :undefined
      end
    end

    lookup_fun = fn(1, _key) ->
      []
    end    

    format_fun = fn(x) ->
      Logger.debug ">>>> format_fun x=#{inspect x}"
    end

    :qlc.table(tf, [{:info_fun, info_fun},
                    {:format_fun, format_fun},
                    {:lookup_fun, lookup_fun},
                    {:key_equality, :'=='}])
  end
end
