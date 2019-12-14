-module(geas_rebar3).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, geas).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================

%% Called when rebar3 first boots, before even parsing the arguments
%% or commands to be run. Purely initiates the provider, and nothing
%% else should be done here.
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},          % The 'user friendly' name of the task
            {module, ?MODULE},          % The module implementation of the task
            {bare, true},               % The task can be run by the user, always true
            {deps, ?DEPS},              % The list of dependencies
            {example, "rebar geas"},    % How to use the plugin
            {opts, []},                 % list of options understood by the plugin
            {short_desc, "Geas rebar3 plugin"},
            {desc, "See https://github.com/crownedgrouse/geas"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% Run the code for the plugin. The command line argument are parsed
%% and dependencies have been run.
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  %Conf = [], % TODO
	%case catch geas:compat(".", print, Conf) of
  case catch geas:compat(".") of
		{error, Reason} 
            -> {error, Reason} ;
		_  -> geas:guilty("."),
          Exit = get(geas_exit_code),
          Msg  = case Exit of
                    1 -> "Current version is incompatible with release window";
                    2 -> "Release window do not match required semver version range";
                    3 -> "Incompatible BEAM file, may need recompilation";
                    4 -> "Incompatible BEAM maximum opcode, may need recompilation";
                    _ -> "Unexpected geas exit code"
                 end,
          case Exit of
            0 -> {ok, State};
            _ -> rebar_api:error("~ts", [Msg]),
                 erlang:halt(Exit)
          end
	end.

%% When an exception is raised or a value returned as
%% `{error, {?MODULE, Reason}}` will see the `format_error(Reason)`
%% function called for them, so a string can be formatted explaining
%% the issue.
-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
