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

-export([process/2, process/3, process_command/2, process_command/3,
         parse/2, parse/3, usage/1, usage/2,
         program_name/1,
         is_option_set/2, option/2, option/3, argument/2,
         trailing_arguments/1, command/1, command_arguments/1,
         format_error/1, help/1]).

-export_type([config/0, cmdline/0, options/0, arguments/0,
              parsing_options/0, error/0,
              optional_string/0, optional_binary/0]).

-type config() :: cmdline_config:config().

-type cmdline() :: #{config := cmdline_config:config(),
                     parsing_options := parsing_options(),
                     program_name := binary(),
                     options := options(),
                     arguments := arguments(),
                     trailing_arguments => [binary()],
                     command => binary(),
                     command_arguments => [binary()]}.

-type options() :: #{binary() := binary() | boolean()}.
-type arguments() :: #{binary() := binary()}.

-type parsing_options() :: #{program_name => unicode:chardata(),
                             short_circuit_options => [unicode:chardata()]}.

-type error() :: truncated_short_option
               | {unknown_option, unicode:chardata()}
               | {missing_option_value, unicode:chardata()}
               | missing_arguments
               | unhandled_arguments
               | missing_command
               | {unknown_command, unicode:chardata()}.

-type optional_string() :: unicode:chardata() | undefined.
-type optional_binary() :: binary() | undefined.

-spec process([string()], config()) -> cmdline().
process(Args, Config) ->
  process(Args, Config, #{}).

-spec process([string()], config(), parsing_options()) -> cmdline().
process(Args, Config, Options) ->
  case parse(Args, Config, Options) of
    {ok, Cmdline = #{command := <<"help">>}} ->
      help(Cmdline);
    {ok, Cmdline = #{options := #{<<"help">> := true}}} ->
      help(Cmdline);
    {ok, Cmdline} ->
      Cmdline;
    {error, Reason} ->
      ReasonString = cmdline:format_error(Reason),
      io:format(standard_error, "error: ~ts~n", [ReasonString]),
      erlang:halt(1)
  end.

-spec process_command(cmdline(), config()) -> cmdline().
process_command(Cmdline, Config) ->
  process_command(Cmdline, Config, #{}).

-spec process_command(cmdline(), config(), parsing_options()) -> cmdline().
process_command(#{program_name := ParentProgramName,
                  command := Command,
                  command_arguments := CommandArguments},
                Config, Options) ->
  ProgramName = <<ParentProgramName/binary, $\s, Command/binary>>,
  cmdline:process(CommandArguments, Config,
                  Options#{program_name => ProgramName}).

-spec parse([string()], config()) -> {ok, cmdline()} | {error, error()}.
parse(Args, Config) ->
  parse(Args, Config, #{}).

-spec parse([string()], config(), parsing_options()) ->
        {ok, cmdline()} | {error, error()}.
parse(Args0, Config0, Options) ->
  Args = lists:map(fun cmdline_text:text_to_binary/1, Args0),
  Config = init_config(Config0, Options),
  ProgramName0 = maps:get(program_name, Options, escript:script_name()),
  ProgramName = cmdline_text:text_to_binary(ProgramName0),
  case cmdline_config:validate(Config) of
    {ok, Config2} ->
      Cmdline0 = #{config => Config2,
                   parsing_options => Options,
                   program_name => ProgramName,
                   options => #{},
                   arguments => #{}},
      Cmdline = add_default_options(Cmdline0, Config2),
      try
        {ok, parse_options(Args, Config2, Cmdline)}
      catch
        throw:{error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      error({invalid_config, Reason})
  end.

-spec init_config(config(), parsing_options()) ->
        config().
init_config(Config0, _Options) ->
  cmdline_config:add_help(Config0).

-spec usage(cmdline()) -> unicode:chardata().
usage(#{config := Config, program_name := ProgramName}) ->
  usage(Config, ProgramName).

-spec usage(config(), ProgramName :: unicode:chardata()) -> unicode:chardata().
usage(Config, ProgramName) ->
  cmdline_usage:format(Config, ProgramName).

-spec parse_options([binary()], config(), cmdline()) -> cmdline().
parse_options([<<"--">> | Args], Config, Cmdline) ->
  maybe_parse_arguments(Args, Config, Cmdline);
parse_options([<<"--", Name/binary>> | Args], Config, Cmdline) ->
  parse_option(Name, Args, Config, Cmdline);
parse_options([<<"-">> | _Args], _Config, _Cmdline) ->
  throw({error, truncated_short_option});
parse_options([<<"-", Name/binary>> | Args], Config, Cmdline) ->
  parse_option(Name, Args, Config, Cmdline);
parse_options(Args, Config, Cmdline) ->
  maybe_parse_arguments(Args, Config, Cmdline).

-spec parse_option(binary(), [binary()], config(), cmdline()) -> cmdline().
parse_option(Name, Args, Config, Cmdline) ->
  case cmdline_config:find_option(Name, Config) of
    {ok, {flag, Short, Long, _}} ->
      Cmdline2 = add_flag(Short, Long, Cmdline),
      parse_options(Args, Config, Cmdline2);
    {ok, {option, Short, Long, _, _, _}} ->
      case Args of
        [] ->
          throw({error, {missing_option_value, Name}});
        [Value | Args2] ->
          Cmdline2 = add_option(Short, Long, Value, Cmdline),
          parse_options(Args2, Config, Cmdline2)
      end;
    error ->
      throw({error, {unknown_option, Name}})
  end.

-spec maybe_parse_arguments([binary()], config(), cmdline()) -> cmdline().
maybe_parse_arguments(Args, Config, Cmdline) ->
  case short_circuit(Cmdline) of
    true ->
      Cmdline;
    false ->
      parse_arguments(Args, Config, Cmdline)
  end.

-spec parse_arguments([binary()], config(), cmdline()) -> cmdline().
parse_arguments(Args, Config, Cmdline) ->
  ArgumentConfigs = cmdline_config:arguments(Config),
  NbArgumentConfigs = length(ArgumentConfigs),
  length(Args) < NbArgumentConfigs andalso
    throw({error, missing_arguments}),
  {Args1, Args2} = lists:split(NbArgumentConfigs, Args),
  Arguments = lists:foldl(fun ({Value, {argument, Name, _}}, Acc) ->
                              Acc#{Name => Value}
                          end, #{}, lists:zip(Args1, ArgumentConfigs)),
  parse_trailing_arguments(Args2, Config, Cmdline#{arguments => Arguments}).

-spec parse_trailing_arguments([binary()], config(), cmdline()) -> cmdline().
parse_trailing_arguments(Args, Config, Cmdline) ->
  case cmdline_config:find_trailing_arguments(Config) of
    {ok, {trailing_arguments, _, _}} ->
      Cmdline#{trailing_arguments => Args};
    error ->
      parse_commands(Args, Config, Cmdline)
  end.

-spec parse_commands([binary()], config(), cmdline()) -> cmdline().
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
              Cmdline#{command => Name, command_arguments => Args2}
          end;
        _ ->
          throw({error, missing_command})
      end
  end.

-spec short_circuit(cmdline()) -> boolean().
short_circuit(Cmdline = #{parsing_options := ParsingOptions}) ->
  Options = maps:get(short_circuit_options, ParsingOptions, []),
  short_circuit([<<"help">> | Options], Cmdline).

-spec short_circuit([unicode:chardata()], cmdline()) -> boolean().
short_circuit([], _) ->
  false;
short_circuit([Option | Options], Cmdline) ->
  case is_option_set(Option, Cmdline) of
    true ->
      true;
    false ->
      short_circuit(Options, Cmdline)
  end.

-spec program_name(cmdline()) -> binary().
program_name(#{program_name := Name}) ->
  Name.

-spec is_option_set(unicode:chardata(), cmdline()) -> boolean().
is_option_set(Name0, #{options := Options}) ->
  Name = cmdline_text:text_to_binary(Name0),
  maps:is_key(Name, Options).

-spec option(unicode:chardata(), cmdline()) -> binary().
option(Name0, Cmdline) ->
  Name = cmdline_text:text_to_binary(Name0),
  option(Name, Cmdline, undefined).

-spec option(unicode:chardata(), cmdline(), optional_string()) -> binary().
option(Name0, #{options := Options}, Default) ->
  Name = cmdline_text:text_to_binary(Name0),
  maps:get(Name, Options, Default).

-spec argument(unicode:chardata(), cmdline()) -> binary().
argument(Name0, #{arguments := Arguments}) ->
  Name = cmdline_text:text_to_binary(Name0),
  maps:get(Name, Arguments).

-spec trailing_arguments(cmdline()) -> [binary()].
trailing_arguments(Cmdline) ->
  maps:get(trailing_arguments, Cmdline, []).

-spec command(cmdline()) -> optional_binary().
command(Cmdline) ->
  maps:get(command, Cmdline, undefined).

-spec command_arguments(cmdline()) -> [binary()].
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

-spec add_flag(Short :: optional_string(), Long :: optional_string(),
               cmdline()) ->
        cmdline().
add_flag(Short, Long, Cmdline) ->
  Cmdline2 = maybe_add_flag(Short, Cmdline),
  Cmdline3 = maybe_add_flag(Long, Cmdline2),
  Cmdline3.

-spec maybe_add_flag(Name :: optional_string(), cmdline()) -> cmdline().
maybe_add_flag(undefined, Cmdline) ->
  Cmdline;
maybe_add_flag(Name0, Cmdline = #{options := Options}) ->
  Name = cmdline_text:text_to_binary(Name0),
  Cmdline#{options => Options#{Name => true}}.

-spec add_option(Short :: optional_string(), Long :: optional_string(),
                 Value :: unicode:chardata(), cmdline()) ->
        cmdline().
add_option(Short, Long, Value, Cmdline) ->
  Cmdline2 = maybe_add_option(Short, Value, Cmdline),
  Cmdline3 = maybe_add_option(Long, Value, Cmdline2),
  Cmdline3.

-spec maybe_add_option(Name :: optional_string(), Value :: unicode:chardata(),
                       cmdline()) ->
        cmdline().
maybe_add_option(undefined, _, Cmdline) ->
  Cmdline;
maybe_add_option(Name0, Value0, Cmdline = #{options := Options}) ->
  Name = cmdline_text:text_to_binary(Name0),
  Value = cmdline_text:text_to_binary(Value0),
  Cmdline#{options => Options#{Name => Value}}.

-spec format_error(error()) -> unicode:chardata().
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
format_error(missing_command) ->
  "missing command";
format_error({unknown_command, Name}) ->
  io_lib:format("unknown command \"~ts\"", [Name]).

-spec help(cmdline()) -> no_return().
help(Cmdline) ->
  io:put_chars(standard_error, usage(Cmdline)),
  erlang:halt(0).
