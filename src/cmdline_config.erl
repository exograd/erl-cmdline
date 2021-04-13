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
         Description :: unicode:chardata()}.

-type option() ::
        {option,
         Short :: cmdline:optional_string(), Long :: cmdline:optional_string(),
         Value :: unicode:chardata(), Default :: cmdline:optional_string(),
         Description :: unicode:chardata()}.

-type argument() ::
        {argument, Name :: unicode:chardata(),
         Description :: unicode:chardata()}.

-type trailing_arguments() ::
        {trailing_arguments, Name :: unicode:chardata(),
         Description :: unicode:chardata()}.

-type command() ::
        {command, Name :: unicode:chardata(),
         Description :: unicode:chardata()}.

-type validation_error() :: {invalid_entry, term()}
                          | trailing_arguments_and_commands.

-spec validate(config()) -> {ok, config()} | {error, validation_error()}.
validate(Config) ->
  try
    Config2 = lists:map(fun validate_entry/1, Config),
    validate_extra_arguments(Config2),
    {ok, Config2}
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

-spec validate_entry(entry()) -> entry().
validate_entry({flag, Short0, Long0, Description0}) ->
  Short = cmdline_text:text_to_binary(Short0),
  Long = cmdline_text:text_to_binary(Long0),
  Description = cmdline_text:text_to_binary(Description0),
  {flag, Short, Long, Description};
validate_entry({option, Short0, Long0, Value0, Default0, Description0}) ->
  Short = cmdline_text:text_to_binary(Short0),
  Long = cmdline_text:text_to_binary(Long0),
  Value = cmdline_text:text_to_binary(Value0),
  Default = cmdline_text:text_to_binary(Default0),
  Description = cmdline_text:text_to_binary(Description0),
  {option, Short, Long, Value, Default, Description};
validate_entry({argument, Name0, Description0}) ->
  Name = cmdline_text:text_to_binary(Name0),
  Description = cmdline_text:text_to_binary(Description0),
  {argument, Name, Description};
validate_entry({trailing_arguments, Name0, Description0}) ->
  Name = cmdline_text:text_to_binary(Name0),
  Description = cmdline_text:text_to_binary(Description0),
  {trailing_arguments, Name, Description};
validate_entry({command, Name0, Description0}) ->
  Name = cmdline_text:text_to_binary(Name0),
  Description = cmdline_text:text_to_binary(Description0),
  {command, Name, Description};
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

-spec find_option(unicode:chardata(), config()) ->
        {ok, flag() | option()} | error.
find_option(Name0, Config) ->
  Name = cmdline_text:text_to_binary(Name0),
  find_option_1(Name, Config).

-spec find_option_1(binary(), config()) -> {ok, flag() | option()} | error.
find_option_1(_, []) ->
  error;
find_option_1(Name, [Entry = {flag, Short, Long, _} | _]) when
    Name =:= Short; Name =:= Long ->
  {ok, Entry};
find_option_1(Name, [Entry = {option, Short, Long, _, _, _} | _]) when
    Name =:= Short; Name =:= Long ->
  {ok, Entry};
find_option_1(Name, [_ | Config]) ->
  find_option_1(Name, Config).

-spec sort_options([flag() | option()]) -> [flag() | option()].
sort_options(Options) ->
  lists:sort(fun (O1, O2) ->
                 option_sort_key(O1) =< option_sort_key(O2)
             end, Options).

-spec option_sort_key(flag() | option()) -> term().
option_sort_key({flag, Short, _, _}) when Short =/= undefined ->
  Short;
option_sort_key({flag, undefined, Long, _}) when Long =/= undefined ->
  Long;
option_sort_key({option, Short, _, _, _, _}) when Short =/= undefined ->
  Short;
option_sort_key({option, undefined, Long, _, _, _}) when Long =/= undefined ->
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
