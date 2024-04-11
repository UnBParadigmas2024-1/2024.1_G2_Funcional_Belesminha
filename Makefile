.PHONY: run clean build all
all: build run
run:
	sudo cabal run
clean:
	sudo cabal clean
build:
	sudo cabal build

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