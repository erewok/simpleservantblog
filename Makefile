all: setup build

setup: client-setup server-setup

build: client-build server-build

client-setup:
	(cd frontend ; elm package install -y)

client-build:
	(cd frontend ; make)

server-setup:
	stack setup
	stack test --only-dependencies

server-build:
	stack build

server-start: server-build
	stack exec server

server-start-reserve:
	stack exec -- simpleservantblog-exe

fast-test:
	seito
	(cd client ; make run-tests)
	(cd client ; make)

slow-test:
	stack test
	(cd client ; make run-tests)
	(cd client ; make)
