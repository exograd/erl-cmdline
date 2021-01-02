%% Copyright (c) 2021 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(cmdline_config).

-export([validate/1,
         find_option/2, arguments/1, find_trailing_arguments/1, commands/1]).

-export_type([optional_string/0,
              config/0, entry/0,
              validation_error/0]).

-type optional_string() :: string() | undefined.

-type config() :: [entry()].

-type entry() ::
        {flag,
         Short :: optional_string(), Long :: optional_string(),
         Description :: string()}
      | {option,
         Short :: optional_string(), Long :: optional_string(),
         Value :: string(), Default :: optional_string(),
         Description :: string()}
      | {argument,
         Name :: string(),
         Description :: string()}
      | {trailing_arguments,
         Name :: string(),
         Description :: string()}
      | {command,
         Name :: string(),
         Description :: string()}.

-type validation_error() :: {invalid_entry, term()}
                          | trailing_arguments_and_commands.

-spec validate(config()) -> ok | {error, validation_error()}.
validate(Config) ->
  try
    lists:map(fun validate_entry/1, Config),
    validate_extra_arguments(Config),
    ok
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec validate_extra_arguments(config()) -> ok.
validate_extra_arguments(Config) ->
  %% We cannot have both trailing arguments (which by definition consume all
  %% arguments left) and a command.
  HasTrailingArgs = lists:keymember(trailing_arguments, 1, Config),
  HasCommands = lists:keymember(command, 1, Config),
  (HasTrailingArgs and HasCommands) andalso
    throw({error, trailing_arguments_and_commands}),
  ok.

-spec validate_entry(entry()) -> ok.
validate_entry({flag, _, _, _}) ->
  ok;
validate_entry({option, _, _, _, _, _}) ->
  ok;
validate_entry({argument, _, _}) ->
  ok;
validate_entry({trailing_arguments, _, _}) ->
  ok;
validate_entry({command, _, _}) ->
  ok;
validate_entry(Entry) ->
  throw({error, {invalid_entry, Entry}}).

-spec find_option(string(), config()) -> {ok, entry()} | error.
find_option(_, []) ->
  error;
find_option(Name, [Entry = {flag, Short, Long, _} | _]) when
    Name =:= Short; Name =:= Long ->
  {ok, Entry};
find_option(Name, [Entry = {option, Short, Long, _, _, _} | _]) when
    Name =:= Short; Name =:= Long ->
  {ok, Entry};
find_option(Name, [_ | Config]) ->
  find_option(Name, Config).

-spec arguments(config()) -> [entry()].
arguments(Config) ->
  arguments(Config, []).

-spec arguments(config(), [entry()]) -> [entry()].
arguments([], Acc) ->
  lists:reverse(Acc);
arguments([Entry = {argument, _, _} | Config], Acc) ->
  arguments(Config, [Entry | Acc]);
arguments([_ | Config], Acc) ->
  arguments(Config, Acc).

-spec find_trailing_arguments(config()) -> {ok, entry()} | error.
find_trailing_arguments([]) ->
  error;
find_trailing_arguments([Entry = {trailing_arguments, _, _} | _]) ->
  {ok, Entry};
find_trailing_arguments([_ | Config]) ->
  find_trailing_arguments(Config).

-spec commands(config()) -> [entry()].
commands(Config) ->
  commands(Config, []).

-spec commands(config(), [entry()]) -> [entry()].
commands([], Acc) ->
  lists:reverse(Acc);
commands([Entry = {command, _, _} | Config], Acc) ->
  commands(Config, [Entry | Acc]);
commands([_ | Config], Acc) ->
  commands(Config, Acc).
