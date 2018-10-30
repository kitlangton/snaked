FROM fpco/stack-build:lts-12.16 as builder

WORKDIR /opt/snaked

COPY stack.yaml /opt/snaked
COPY package.yaml /opt/snaked
RUN stack install --only-dependencies

COPY . /opt/snaked
RUN stack install

FROM centos:7

COPY --from=builder /root/.local/bin/ /root/.local/bin/
