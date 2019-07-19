FROM elixir:1.7.4
LABEL authors="Alexey Bolshakov <ua3mqj@gmail.com>"
MAINTAINER UA3MQJ <UA3MQJ@gmail.com>
ADD . /app
WORKDIR /app
RUN mkdir -p /app/priv
RUN mkdir -p /app/priv/data
RUN apt-get update
RUN apt-get install -y ca-certificates wget
RUN apt-get install -y software-properties-common
RUN wget https://openresty.org/package/pubkey.gpg
RUN apt-key add pubkey.gpg
RUN add-apt-repository -y "deb http://openresty.org/package/debian $(lsb_release -sc) openresty"
RUN apt-get update
RUN apt-get install -y openresty
RUN opm get pintsized/lua-resty-http
RUN apt-get install -y unzip
RUN apt-get install -y qt5-default
RUN ./jsonreadermake.sh
RUN mix local.hex --force
RUN mix local.rebar --force
RUN mix deps.get
RUN mix compile
EXPOSE 80
CMD ./start.sh