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

-export([validate/1, add_help/1,
         options/1, find_option/2, sort_options/1,
         arguments/1, find_trailing_arguments/1,
         commands/1, sort_commands/1]).

-export_type([config/0, entry/0,
              flag/0, option/0, argument/0, trailing_arguments/0, command/0,
              validation_error/0]).

-type config() :: [entry()].

-type entry() :: flag()
               | option()
               | argument()
               | trailing_arguments()
               | command().

-type flag() ::
        {flag,
         Short :: cmdline:optional_string(), Long :: cmdline:optional_string(),
         Description :: string()}.

-type option() ::
        {option,
         Short :: cmdline:optional_string(), Long :: cmdline:optional_string(),
         Value :: string(), Default :: cmdline:optional_string(),
         Description :: string()}.

-type argument() ::
        {argument, Name :: string(), Description :: string()}.

-type trailing_arguments() ::
        {trailing_arguments, Name :: string(), Description :: string()}.

-type command() ::
        {command, Name :: string(), Description :: string()}.

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

-spec add_help(config()) -> config().
add_help(Config) ->
  HelpOption = {flag, "h", "help", "print help and exit"},
  Config2 = [HelpOption | Config],
  case commands(Config) of
    [] ->
      Config2;
    _ ->
      HelpCommand = {command, "help", "print help and exit"},
      [HelpCommand | Config2]
  end.

-spec options(config()) -> [flag() | option()].
options(Config) ->
  lists:filter(fun
                 ({flag, _, _, _}) ->
                   true;
                 ({option, _, _, _, _, _}) ->
                   true;
                 (_) ->
                   false
               end, Config).

-spec find_option(string(), config()) -> {ok, flag() | option()} | error.
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

-spec sort_options([flag() | option()]) -> [flag() | option()].
sort_options(Options) ->
  lists:sort(fun (O1, O2) ->
                 option_sort_key(O1) =< option_sort_key(O2)
             end, Options).

-spec option_sort_key(flag() | option()) -> term().
option_sort_key({flag, Short, _, _}) when is_list(Short) ->
  Short;
option_sort_key({flag, undefined, Long, _}) when is_list(Long) ->
  Long;
option_sort_key({option, Short, _, _, _, _}) when is_list(Short) ->
  Short;
option_sort_key({option, undefined, Long, _, _, _}) when is_list(Long) ->
  Long.

-spec arguments(config()) -> [argument()].
arguments(Config) ->
  lists:filter(fun (E) -> element(1, E) =:= argument end, Config).

-spec find_trailing_arguments(config()) -> {ok, trailing_arguments()} | error.
find_trailing_arguments(Config) ->
  case lists:keyfind(trailing_arguments, 1, Config) of
    false ->
      error;
    Entry ->
      {ok, Entry}
  end.

-spec commands(config()) -> [command()].
commands(Config) ->
  lists:filter(fun (E) -> element(1, E) =:= command end, Config).

-spec sort_commands([command()]) -> [command()].
sort_commands(Commands) ->
  lists:keysort(2, Commands).
