.PHONY: all run clean build docker-build install uninstall
all: build run

run:
	bash -c "cabal run"
clean:
	bash -c "cabal clean"
build:
	bash -c "cabal build"
docker-build:
	docker-compose up
install:
	chmod +x ./install.sh
	./install.sh
uninstall: clean
	chmod +x ./uninstall.sh
	./uninstall.sh