.PHONY: run clean build all
all: build run
run:
	bash -c "cabal run"
clean:
	bash -c "cabal clean"
build:
	bash -c "cabal build"

.PHONY: init
init:
	docker-compose up --build -d
	docker exec -it ubuntu zsh
install:
	chmod +x ./install.sh
	./install.sh
uninstall:
	chmod +x ./uninstall.sh
	./uninstall.sh