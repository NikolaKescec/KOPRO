FROM erlang
ADD . /usr/dist
WORKDIR /usr/dist
ENTRYPOINT ["erl", "-noshell", "-setcookie", "k", "-pa", "/usr/dist"]