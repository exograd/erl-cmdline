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

-export([parse/3, parse/4, usage/1, usage/2,
         is_option_set/2, option/2, option/3, argument/2,
         trailing_arguments/1, command/1, command_arguments/1,
         format_error/1]).

-export_type([config/0, cmdline/0, options/0, arguments/0,
              parsing_options/0, error/0,
              optional_string/0]).

-type config() :: cmdline_config:config().

-type cmdline() :: #{config := cmdline_config:config(),
                     parsing_options := parsing_options(),
                     arg0 := string(),
                     options := options(),
                     arguments := arguments(),
                     trailing_arguments => [string()],
                     command => string(),
                     command_arguments => [string()]}.

-type options() :: #{string() := string() | boolean()}.
-type arguments() :: #{string() := string()}.

-type parsing_options() :: #{handle_help => boolean(),
                             short_circuit_options => [string()]}.

-type error() :: truncated_short_option
               | {unknown_option, string()}
               | {missing_option_value, string()}
               | missing_arguments
               | unhandled_arguments
               | {unknown_command, string()}.

-type optional_string() :: string() | undefined.

-spec parse(string(), [string()], config()) ->
        {ok, cmdline()} | {error, error()}.
parse(Arg0, Args, Config) ->
  parse(Arg0, Args, Config, #{}).

-spec parse(string(), [string()], config(), parsing_options()) ->
        {ok, cmdline()} | {error, error()}.
parse(Arg0, Args, Config0, Options) ->
  Config = init_config(Config0, Options),
  case cmdline_config:validate(Config) of
    ok ->
      Cmdline0 = #{config => Config,
                   parsing_options => Options,
                   arg0 => Arg0,
                   options => #{},
                   arguments => #{}},
      Cmdline = add_default_options(Cmdline0, Config),
      try
        {ok, parsing_options(Args, Config, Cmdline)}
      catch
        throw:{error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      error({invalid_config, Reason})
  end.

-spec init_config(config(), parsing_options()) ->
        config().
init_config(Config0, Options) ->
  case maps:get(handle_help, Options, false) of
    true ->
      cmdline_config:add_help(Config0);
    false ->
      Config0
  end.

-spec usage(cmdline()) -> unicode:chardata().
usage(#{config := Config, arg0 := Arg0}) ->
  usage(Config, Arg0).

-spec usage(config(), Arg0 :: string()) -> unicode:chardata().
usage(Config, Arg0) ->
  cmdline_usage:format(Config, Arg0).

-spec parsing_options([string()], config(),
                    cmdline()) -> cmdline().
parsing_options(["--" | Args], Config, Cmdline) ->
  maybe_parse_arguments(Args, Config, Cmdline);
parsing_options([[$- | [$- | Name]] | Args], Config, Cmdline) ->
  parse_option(Name, Args, Config, Cmdline);
parsing_options(["-" | _Args], _Config, _Cmdline) ->
  throw({error, truncated_short_option});
parsing_options([[$- | Name] | Args], Config, Cmdline) ->
  parse_option(Name, Args, Config, Cmdline);
parsing_options(Args, Config, Cmdline) ->
  maybe_parse_arguments(Args, Config, Cmdline).

-spec parse_option(string(), [string()], config(),
                   cmdline()) -> cmdline().
parse_option(Name, Args, Config, Cmdline) ->
  case cmdline_config:find_option(Name, Config) of
    {ok, {flag, Short, Long, _}} ->
      Cmdline2 = add_flag(Short, Long, Cmdline),
      parsing_options(Args, Config, Cmdline2);
    {ok, {option, Short, Long, _, _, _}} ->
      case Args of
        [] ->
          throw({error, {missing_option_value, Name}});
        [Value | Args2] ->
          Cmdline2 = add_option(Short, Long, Value, Cmdline),
          parsing_options(Args2, Config, Cmdline2)
      end;
    error ->
      throw({error, {unknown_option, Name}})
  end.

-spec maybe_parse_arguments([string()], config(), cmdline()) ->
        cmdline().
maybe_parse_arguments(Args, Config, Cmdline) ->
  case handle_help(Cmdline) andalso is_option_set("help", Cmdline) of
    true ->
      help(Cmdline);
    false ->
      case short_circuit(Cmdline) of
        true ->
          Cmdline;
        false ->
          parse_arguments(Args, Config, Cmdline)
      end
  end.

-spec parse_arguments([string()], config(), cmdline()) ->
        cmdline().
parse_arguments(Args, Config, Cmdline) ->
  ArgumentConfigs = cmdline_config:arguments(Config),
  NbArgumentConfigs = length(ArgumentConfigs),
  length(Args) < NbArgumentConfigs andalso
    throw({error, missing_arguments}),
  {Args1, Args2} = lists:split(NbArgumentConfigs, Args),
  Arguments = lists:foldl(fun ({Value, {argument, Name, _}}, Acc) ->
                              Acc#{Name => Value}
                          end, #{}, lists:zip(Args1, ArgumentConfigs)),
  parse_trailing_arguments(Args2, Config,
                           Cmdline#{arguments => Arguments}).

-spec parse_trailing_arguments([string()], config(),
                               cmdline()) -> cmdline().
parse_trailing_arguments(Args, Config, Cmdline) ->
  case cmdline_config:find_trailing_arguments(Config) of
    {ok, {trailing_arguments, _, _}} ->
      Cmdline#{trailing_arguments => Args};
    error ->
      parse_commands(Args, Config, Cmdline)
  end.

-spec parse_commands([string()], config(), cmdline()) ->
        cmdline().
parse_commands(Args, Config, Cmdline) ->
  case cmdline_config:commands(Config) of
    [] ->
      Args /= [] andalso throw({error, unhandled_arguments}),
      Cmdline;
    Commands ->
      case Args of
        [Name | Args2] ->
          case lists:keyfind(Name, 2, Commands) of
            false ->
              throw({error, {unknown_command, Name}});
            _ ->
              Cmdline2 = Cmdline#{command => Name,
                                  command_arguments => Args2},
              case handle_help(Cmdline2) andalso Name == "help" of
                true ->
                  help(Cmdline2);
                false ->
                  Cmdline2
              end
          end;
        _ ->
          throw({error, missing_command})
      end
  end.

-spec short_circuit(cmdline()) -> boolean().
short_circuit(Cmdline = #{parsing_options := ParsingOptions}) ->
  case maps:find(short_circuit_options, ParsingOptions) of
    {ok, Options} ->
      short_circuit(Options, Cmdline);
    error ->
      false
  end.

-spec short_circuit([string()], cmdline()) -> boolean().
short_circuit([], _) ->
  false;
short_circuit([Option | Options], Cmdline) ->
  case is_option_set(Option, Cmdline) of
    true ->
      true;
    false ->
      short_circuit(Options, Cmdline)
  end.

-spec is_option_set(string(), cmdline()) -> boolean().
is_option_set(Name, #{options := Options}) ->
  maps:is_key(Name, Options).

-spec option(string(), cmdline()) -> string().
option(Name, Cmdline) ->
  option(Name, Cmdline, undefined).

-spec option(string(), cmdline(), optional_string()) -> string().
option(Name, #{options := Options}, Default) ->
  maps:get(Name, Options, Default).

-spec argument(string(), cmdline()) -> string().
argument(Name, #{arguments := Arguments}) ->
  maps:get(Name, Arguments).

-spec trailing_arguments(cmdline()) -> string().
trailing_arguments(Cmdline) ->
  maps:get(trailing_arguments, Cmdline, []).

-spec command(cmdline()) -> optional_string().
command(Cmdline) ->
  maps:get(command, Cmdline, undefined).

-spec command_arguments(cmdline()) -> optional_string().
command_arguments(Cmdline) ->
  maps:get(command_arguments, Cmdline, []).

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

-spec maybe_add_option(optional_string(), string(), cmdline()) -> cmdline().
maybe_add_option(undefined, _, Cmdline) ->
  Cmdline;
maybe_add_option(Name, Value, Cmdline = #{options := Options}) ->
  Cmdline#{options => Options#{Name => Value}}.

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
  "unhandled argument(s)";
format_error({unknown_command, Name}) ->
  io_lib:format("unknown command \"~ts\"", [Name]).

-spec handle_help(cmdline()) -> boolean().
handle_help(#{parsing_options := Options}) ->
  maps:get(handle_help, Options, false).

-spec help(cmdline()) -> no_return().
help(Cmdline) ->
  io:put_chars(standard_error, usage(Cmdline)),
  erlang:halt(0).
