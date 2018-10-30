dist:
	# rm -rf ./bin
	# mkdir ./bin
	docker build . -t snaked
	docker create --name snaked-build snaked:latest
	docker cp snaked-build:/root/.local/bin .
	docker rm snaked-build

build:
	stack build

install: build
	stack install

.PHONY: dist, install
