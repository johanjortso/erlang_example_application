-module(my1st_SUITE).

-export([all/0, mod_exists/1]).

%-compile(export_all).

all() -> [mod_exists].

mod_exists(_) ->
    %erltest = Module,
    %ok.
    % code:compile("ex.erl").
    Module = erltest,
    code:purge(Module),
    {module, Module} = code:load_file(Module),

    ok.

% ct_run -dir .
% or:

%  $ ct_run -suite check_log_SUITE
% To use the Erlang shell to run our test, you can evaluate the following call:

%  ct:run_test([{dir, "."}]).

% ct:run_test([{suite, "check_log_SUITE"}]).
% ct:run_test([{suite, "my1st_SUITE"}]).

