FROM haskell:latest

WORKDIR /opt/snaked

COPY stack.yaml /opt/snaked
COPY package.yaml /opt/snaked
RUN stack install --only-dependencies

COPY . /opt/snaked

RUN stack install

# CMD ["/usr/local/bin/stack", "exec", "example-exe"]
