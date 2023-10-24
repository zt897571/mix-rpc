init:
	cd erlang2 && rebar3 update && cd ../
	cd golang && go mod tidy

erlang:
	cd erlang2 && rebar3 release

golang:
	cd golang/app/testnode && go build -o ./bin/testnode main.go

start_erlang:
	cd erlang2 && rebar3 release && cd _build/default/rel/test/bin && test console

proto:
	protoc --gogoslick_out=./golang/proto -I=proto ./proto/*.proto

test:
	cd erlang2 && rebar3 ct && cd ../
	cd golang && go test -gcflags=all=-l -v ./... -cover

golang_test:
	cd golang && go test -gcflags=all=-l -v ./... -cover

clean_win:
	if exist erlang2/_build/ (rd /s /q erlang2\_build\)

.PHONY: erlang golang proto test clean_win golang_test

