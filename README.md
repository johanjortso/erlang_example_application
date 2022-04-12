# Example Erlang OTP application

## About

This is an example of a "simple" Erlang OTP application. The idea is to add the parts step by step, first simple independent modules, then adding tests, types and rearranging everything into an OTP application with behaviours, supervisors etc...

## How to build

Currently no rebar help or anything. Just compile the modules directly from the shell:

`c(ModuleName)`.

Eunit:

`Module:test` (internal module tests) or `eunit:test(Module)` (internal and external module tests).

Eunit:

`c(Module:test).` (internal module tests) or `eunit:test(Module)` (internal and external module tests)

## Progression

1. Library module
2. Internal eunit tests for module
3. Library module with external eunit tests
4. ???
5. Typer/Dialyzer
6. Example server
7. Common test
8. Edoc
9. OTP Application: directory structure, app, supervisor
10. Dynamic child worker?
11. Rebar
12. External dependencies through rebar
