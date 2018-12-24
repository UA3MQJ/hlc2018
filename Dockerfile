FROM ubuntu:16.04
LABEL authors="Alexey Bolshakov <ua3mqj@gmail.com>, Sergey Samokhvalov <onlyforthesky@gmail.com>"

ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8
ENV ERL_AFLAGS="+A 1 +S 2:2 +SDcpu 2:2"

RUN (echo 'deb http://packages.erlang-solutions.com/ubuntu xenial contrib' >> /etc/apt/sources.list) \
  && (apt-key adv --fetch-keys  http://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc) \
  && apt-get update \
  && apt-get install -y wget elixir \
  && mix local.hex --force \
  && mix local.rebar --force

RUN mkdir elixir \
  && mkdir elixir/config \
  && mkdir elixir/lib \
  && mkdir elixir/rel \
  && mkdir elixir/test

ADD mix.exs elixir
ADD config/config.exs elixir/config
ADD lib/router.ex elixir/lib
ADD lib/http_test2.ex elixir/lib
ADD rel/config.exs elixir/rel

RUN cd elixir && mix deps.get && mix compile

EXPOSE 8080

CMD cd elixir && iex -S mix