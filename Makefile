init:
	cd erlang && rebar3 update && cd ../
	cd golang && go mod tidy

erlang:
	cd erlang && rebar3 release

golang:
	cd golang/app/testnode && go build -o ./bin/testnode main.go

start_erlang:
	cd erlang && rebar3 release && cd _build/default/rel/erlang/bin && erlang console

proto:
	protoc --gogoslick_out=./golang/proto -I=proto ./proto/*.proto

test:
	cd  erlang && rebar3 ct && cd ../
	cd golang && go test -gcflags=all=-l -v ./... -cover

clean_win:
	if exist erlang/_build/ (rd /s /q erlang\_build\)

.PHONY: erlang golang proto test clean_win

