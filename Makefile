.PHONY: all run clean build docker-build install uninstall
all: build run

run:
	bash -c "cabal run"
clean:
	bash -c "cabal clean"
build:
	bash -c "cabal build"
docker-build:
	docker build . -t app-build
	docker create --name build app-build
	docker cp build:/build/dist-newstyle .
	docker cp build:/build/dist-newstyle/build/x86_64-linux/ghc-8.8.4/x20241-G2-Funcional-Belesminha-0.1.0.0/x/x20241-G2-Funcional-Belesminha-exec.out/build/x20241-G2-Funcional-Belesminha-exec.out/x20241-G2-Funcional-Belesminha-exec.out ./Belesminha.out
	./Belesminha.out
	docker rm build	
install:
	chmod +x ./install.sh
	./install.sh
uninstall: clean
	chmod +x ./uninstall.sh
	./uninstall.sh