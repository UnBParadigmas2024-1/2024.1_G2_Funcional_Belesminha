.PHONY: init
init:
	docker-compose up -d
	docker exec -it ubuntu zsh