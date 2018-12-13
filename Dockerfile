FROM fpco/stack-build:lts-12.22 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc
FROM ubuntu:16.04
RUN mkdir -p /opt/servant-box
WORKDIR /opt/servant-box
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-12.22/8.4.4/bin .
# $PORT is set by Heroku
CMD /opt/servant-box/server --port $PORT
