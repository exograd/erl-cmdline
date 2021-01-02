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

-module(cmdline).

-export([parse/3,
         has_option/2, get_option/2, get_option/3, get_argument/2,
         format_error/1]).

-export_type([config/0, config_entry/0,
              cmdline/0, options/0, arguments/0,
              error/0]).

-type config() :: [config_entry()].

-type config_entry() ::
        {flag,
         Short :: optional_string(), Long :: optional_string(),
         Description :: string()}
      | {option,
         Short :: optional_string(), Long :: optional_string(),
         Value :: string(), Default :: optional_string(),
         Description :: string()}
      | {argument,
         Name :: string(),
         Description :: string()}.

-type cmdline() :: #{options := options(),
                     arguments := arguments()}.

-type options() :: #{string() := string() | boolean()}.
-type arguments() :: #{string() := string()}.

-type error() :: term().

-type optional_string() :: string() | undefined.

-spec parse(string(), [string()], config()) ->
        {ok, cmdline()} | {error, error()}.
parse(Arg0, Args, Config0) ->
  Config = maybe_add_help_flag(Config0),
  Cmdline0 = #{options => #{},
               arguments => #{}},
  Cmdline = add_default_options(Cmdline0, Config),
  try
    {ok, parse_options(Arg0, Args, Config, Cmdline)}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec parse_options(string(), [string()], config(), cmdline()) -> cmdline().
parse_options(_Arg0, [], Config, Cmdline) ->
  finalize(Cmdline, Config);
parse_options(Arg0, ["--" | Args], Config, Cmdline) ->
  parse_arguments(1, Arg0, Args, Config, Cmdline);
parse_options(Arg0, [[$- | [$- | Name]] | Args], Config, Cmdline) ->
  parse_option(Name, Arg0, Args, Config, Cmdline);
parse_options(_Arg0, ["-" | _Args], _Config, _Cmdline) ->
  throw({error, truncated_short_option});
parse_options(Arg0, [[$- | Name] | Args], Config, Cmdline) ->
  parse_option(Name, Arg0, Args, Config, Cmdline);
parse_options(Arg0, Args, Config, Cmdline) ->
  parse_arguments(1, Arg0, Args, Config, Cmdline).

-spec parse_option(string(), string(), [string()], config(), cmdline()) ->
        cmdline().
parse_option(Name, Arg0, Args, Config, Cmdline) ->
  case find_option(Name, Config) of
    {ok, {flag, Short, Long, _}} ->
      Cmdline2 = add_flag(Short, Long, Cmdline),
      parse_options(Arg0, Args, Config, Cmdline2);
    {ok, {option, Short, Long, _, _, _}} ->
        case Args of
          [] ->
            throw({error, {missing_option_value, Name}});
          [Value | Args2] ->
            Cmdline2 = add_option(Short, Long, Value, Cmdline),
            parse_options(Arg0, Args2, Config, Cmdline2)
        end;
    error ->
      throw({error, {unknown_option, Name}})
  end.

-spec parse_arguments(pos_integer(), string(), [string()], config(),
                      cmdline()) -> cmdline().
parse_arguments(N, Arg0, [Value | Args], Config, Cmdline) ->
  case find_argument(N, Config) of
    {ok, {argument, Name, _}} ->
      Cmdline2 = add_argument(Name, Value, Cmdline),
      parse_arguments(N+1, Arg0, Args, Config, Cmdline2);
    error ->
      throw({error, unhandled_arguments})
  end;
parse_arguments(_N, _Arg0, [], Config, Cmdline) ->
  finalize(Cmdline, Config).

-spec finalize(cmdline(), config()) -> cmdline().
finalize(Cmdline, Config) ->
  check_arguments(Cmdline, Config),
  Cmdline.

-spec check_arguments(cmdline(), config()) -> ok.
check_arguments(#{arguments := Arguments}, Config) ->
  case arguments(Config) of
    ArgumentEntries when length(ArgumentEntries) > map_size(Arguments) ->
      throw({error, missing_arguments});
    _ ->
      ok
  end.

-spec has_option(string(), cmdline()) -> boolean().
has_option(Name, #{options := Options}) ->
  maps:is_key(Name, Options).

-spec get_option(string(), cmdline()) -> string().
get_option(Name, Cmdline) ->
  get_option(Name, Cmdline, undefined).

-spec get_option(string(), cmdline(), optional_string()) -> string().
get_option(Name, #{options := Options}, Default) ->
  maps:get(Name, Options, Default).

-spec get_argument(string(), cmdline()) -> string().
get_argument(Name, #{arguments := Arguments}) ->
  maps:get(Name, Arguments).

-spec find_option(string(), config()) -> {ok, config_entry()} | error.
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

-spec add_default_options(cmdline(), config()) -> cmdline().
add_default_options(Cmdline, []) ->
  Cmdline;
add_default_options(Cmdline,
                    [{option, Short, Long, _, Default, _} | Config]) ->
  Cmdline2 = add_option(Short, Long, Default, Cmdline),
  add_default_options(Cmdline2, Config);
add_default_options(Cmdline, [_ | Config]) ->
  add_default_options(Cmdline, Config).

-spec add_flag(optional_string(), optional_string(), cmdline()) -> cmdline().
add_flag(Short, Long, Cmdline) ->
  Cmdline2 = maybe_add_flag(Short, Cmdline),
  Cmdline3 = maybe_add_flag(Long, Cmdline2),
  Cmdline3.

-spec maybe_add_flag(optional_string(), cmdline()) -> cmdline().
maybe_add_flag(undefined, Cmdline) ->
  Cmdline;
maybe_add_flag(Name, Cmdline = #{options := Options}) ->
  Cmdline#{options => Options#{Name => true}}.

-spec add_option(optional_string(), optional_string(), string(), cmdline()) ->
        cmdline().
add_option(Short, Long, Value, Cmdline) ->
  Cmdline2 = maybe_add_option(Short, Value, Cmdline),
  Cmdline3 = maybe_add_option(Long, Value, Cmdline2),
  Cmdline3.

-spec add_argument(string(), string(), cmdline()) -> cmdline().
add_argument(Name, Value, Cmdline = #{arguments := Arguments}) ->
  Cmdline#{arguments => Arguments#{Name => Value}}.

-spec maybe_add_option(optional_string(), string(), cmdline()) -> cmdline().
maybe_add_option(undefined, _, Cmdline) ->
  Cmdline;
maybe_add_option(Name, Value, Cmdline = #{options := Options}) ->
  Cmdline#{options => Options#{Name => Value}}.

-spec find_argument(pos_integer(), config()) -> {ok, config_entry()} | error.
find_argument(N, Config) ->
  case arguments(Config) of
    Arguments when length(Arguments) >= N ->
      {ok, lists:nth(N, Arguments)};
    _ ->
      error
  end.

-spec arguments(config()) -> config().
arguments(Config) ->
  arguments(Config, []).

-spec arguments(config(), [config_entry()]) -> config().
arguments([], Acc) ->
  lists:reverse(Acc);
arguments([Entry = {argument, _, _} | Config], Acc) ->
  arguments(Config, [Entry | Acc]);
arguments([_ | Config], Acc) ->
  arguments(Config, Acc).

-spec maybe_add_help_flag(config()) -> config().
maybe_add_help_flag(Config) ->
  F = fun
        ({flag, _, "help", _}) -> true;
        (_) -> false
      end,
  case lists:search(F, Config) of
    {value, _} ->
      Config;
    false ->
      Option = {flag, "h", "help", "print help and exit"},
      [Option | Config]
  end.

-spec format_error(error()) -> string().
format_error(truncated_short_option) ->
  "truncated short option";
format_error({unknown_option, Name}) ->
  io_lib:format("unknown option \"~ts\"", [Name]);
format_error({missing_option_value, Name}) ->
  io_lib:format("missing value for option \"~ts\"", [Name]);
format_error(missing_arguments) ->
  "missing argument(s)";
format_error(unhandled_arguments) ->
  "unhandled argument(s)".
