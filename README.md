# Example Erlang OTP application

## About

This is an example of a "simple" Erlang OTP application. The idea is to add the parts step by step, first simple independent modules, then adding tests, types and rearranging everything into an OTP application with behaviours, supervisors etc...

## How to build

Currently no rebar help or anything. Just compile the modules directly from the shell:

`c(ModuleName)`.

Eunit:

`Module:test` (internal module tests) or `eunit:test(Module)` (internal and external module tests).

1. Library module
2. Internal eunit tests for module
3. Library module with external eunit tests
