all: setup build server-start

setup: client-setup server-setup

build: server-build client-build

client-setup:
	(cd client ; elm package install -y)

client-build:
	(cd client ; make)

server-setup:
	stack setup
	stack test --only-dependencies

server-build:
	stack build

server-start: build
	stack exec simpleservantblog-exe

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
