
defmodule DynamicTestHandler do
 
  def init(req0, opts) do
    # IO.puts  ">>> req0=#{inspect req0}"
    # IO.puts  ">>> opts=#{inspect opts}"
    # qs_vals = :cowboy_req.parse_qs(req0)
    # IO.puts  ">>> qs_vals=#{inspect qs_vals}"
    # {:ok, data, _req} = :cowboy_req.read_body(req0)


    req = :cowboy_req.reply(
      400,
      %{"content-type" => "application/json"},
      "{}", req0)

    {:ok, req, opts}
  end

end