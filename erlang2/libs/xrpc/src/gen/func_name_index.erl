%% Code generated by protoc-gen-go-xrpc. DO NOT EDIT.
%% template: erl_name_index.tmpl

-module(func_name_index).

-export([get_function_name/1]).


get_function_name('OnCallNodeTest') ->
   on_call_node_test;
get_function_name('OnCastNodeTest') ->
   on_cast_node_test;
get_function_name('OnCallNodeGetPidList') ->
   on_call_node_get_pid_list;
get_function_name('OnCastNodeGetPidList') ->
   on_cast_node_get_pid_list;
get_function_name(FuncName) ->
   FuncName.