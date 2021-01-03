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
         options/1, find_option/2, sort_options/1,
         arguments/1, find_trailing_arguments/1,
         commands/1, sort_commands/1]).

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

-spec options(config()) -> [entry()].
options(Config) ->
  lists:filter(fun
                 ({flag, _, _, _}) ->
                   true;
                 ({option, _, _, _, _, _}) ->
                   true;
                 (_) ->
                   false
               end, Config).

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

-spec sort_options([entry()]) -> [entry()].
sort_options(Options) ->
  lists:sort(fun (O1, O2) ->
                 option_sort_key(O1) =< option_sort_key(O2)
             end, Options).

-spec option_sort_key(entry()) -> term().
option_sort_key({flag, Short, _, _}) when is_list(Short) ->
  Short;
option_sort_key({flag, undefined, Long, _}) when is_list(Long) ->
  Long;
option_sort_key({option, Short, _, _, _, _}) when is_list(Short) ->
  Short;
option_sort_key({option, undefined, Long, _, _, _}) when is_list(Long) ->
  Long.

-spec arguments(config()) -> [entry()].
arguments(Config) ->
  lists:filter(fun (E) -> element(1, E) =:= argument end, Config).

-spec find_trailing_arguments(config()) -> {ok, entry()} | error.
find_trailing_arguments(Config) ->
  case lists:keyfind(trailing_arguments, 1, Config) of
    false ->
      error;
    Entry ->
      {ok, Entry}
  end.

-spec commands(config()) -> [entry()].
commands(Config) ->
  lists:filter(fun (E) -> element(1, E) =:= command end, Config).

-spec sort_commands([entry()]) -> [entry()].
sort_commands(Commands) ->
  lists:keysort(2, Commands).
