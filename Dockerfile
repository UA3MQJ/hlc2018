FROM elixir:1.7.4
LABEL authors="Alexey Bolshakov <ua3mqj@gmail.com>"
MAINTAINER UA3MQJ <UA3MQJ@gmail.com>
ADD . /app
WORKDIR /app
RUN mkdir -p /app/priv
RUN mkdir -p /app/priv/data
RUN apt-get update
RUN apt-get install -y unzip
RUN apt-get install -y tree
RUN mix local.hex --force
RUN mix local.rebar --force
RUN mix deps.get
RUN mix compile
EXPOSE 80
CMD ./start.sh