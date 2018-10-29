dist:
	docker build . -t snaked
	docker run --name snaked snaked:latest
	docker cp snaked:/root/.local/bin ./bin
