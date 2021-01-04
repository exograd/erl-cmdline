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

-module(cmdline_usage).

-export([format/2]).

-spec format(cmdline:config(), Arg0 :: string()) -> unicode:chardata().
format(Config, Arg0) ->
  %% Program name and arguments
  Arguments = cmdline_config:arguments(Config),
  Commands = cmdline_config:sort_commands(cmdline_config:commands(Config)),
  CommandLine = case Commands of
                  [] -> [];
                  _ -> " <command> [...]"
                end,
  FirstLine = ["Usage: ", Arg0, " OPTIONS",
               format_argument_line(Arguments),
               maybe_format_trailing_argument_line(Config),
               CommandLine,
               $\n],
  %% Compute the width of the column containing option labels, argument names
  %% and command names.
  ColumnWidth = column_width(Config),
  %% Options
  Options = cmdline_config:sort_options(cmdline_config:options(Config)),
  OptionSection = ["\nOPTIONS\n\n", format_options(Options, ColumnWidth)],
  %% Arguments
  HasArguments = Arguments /= [],
  HasTrailingArguments =
    cmdline_config:find_trailing_arguments(Config) /= error,
  ArgumentSection = case HasArguments or HasTrailingArguments  of
                      true ->
                        ["\nARGUMENTS\n\n",
                         format_arguments(Arguments, ColumnWidth),
                         maybe_format_trailing_arguments(Config, ColumnWidth)];
                      false ->
                        []
                    end,
  %% Commands
  CommandSection = case Commands of
                     [] -> "";
                     _ -> ["\nCOMMANDS\n\n",
                           format_commands(Commands, ColumnWidth)]
                   end,
  [FirstLine, OptionSection, ArgumentSection, CommandSection].

-spec format_argument_line([cmdline_config:argument()]) -> unicode:chardata().
format_argument_line(Arguments) ->
  lists:map(fun ({argument, Name, _}) ->
                [" <", Name, ">"]
            end, Arguments).

-spec maybe_format_trailing_argument_line(cmdline:config()) ->
        unicode:chardata().
maybe_format_trailing_argument_line(Config) ->
  case cmdline_config:find_trailing_arguments(Config) of
    {ok, {trailing_arguments, Name, _}} ->
      [" [<", Name, "...>]"];
    error ->
      []
  end.

-spec format_options([cmdline_config:flag() | cmdline_config:option()],
                     non_neg_integer()) ->
        unicode:chardata().
format_options(Options, ColumnWidth) ->
  lists:map(fun (Option) -> format_option(Option, ColumnWidth) end, Options).

-spec format_option(cmdline_config:flag() | cmdline_config:option(),
                    non_neg_integer()) ->
        unicode:chardata().
format_option(Entry = {flag, _, _, Description}, ColumnWidth) ->
  Label = format_option_label(Entry),
  [string:pad(Label, ColumnWidth), "  ", Description, $\n];
format_option(Entry = {option, _, _, _, Default, Description},
              ColumnWidth) ->
  Label = format_option_label(Entry),
  DefaultPart = case Default of
                  undefined -> "";
                  _ -> [" (default: ", Default, ")"]
                end,
  [string:pad(Label, ColumnWidth), "  ", Description, DefaultPart, $\n].

-spec format_option_label(cmdline_config:flag() | cmdline_config:option()) ->
        unicode:chardata().
format_option_label({flag, Short, Long, _}) ->
  format_option_names(Short, Long);
format_option_label({option, Short, Long, Value, _, _}) ->
  [format_option_names(Short, Long), " <", Value, ">"].

-spec format_option_names(cmdline:optional_string(),
                          cmdline:optional_string()) ->
        unicode:chardata().
format_option_names(Short, undefined) ->
  ["-", Short];
format_option_names(undefined, Long) ->
  ["--", Long];
format_option_names(Short, Long) ->
  ["-", Short, ", --", Long].

-spec format_arguments([cmdline_config:argument()], non_neg_integer()) ->
        unicode:chardata().
format_arguments(Arguments, ColumnWidth) ->
  lists:map(fun ({argument, Name, Description}) ->
                [string:pad(Name, ColumnWidth), "  ", Description, $\n]
            end, Arguments).

-spec maybe_format_trailing_arguments(cmdline:config(), non_neg_integer()) ->
        unicode:chardata().
maybe_format_trailing_arguments(Config, ColumnWidth) ->
  case cmdline_config:find_trailing_arguments(Config) of
    {ok, {trailing_arguments, Name, Description}} ->
      [string:pad(Name, ColumnWidth), "  ", Description, $\n];
    error ->
      []
  end.

-spec format_commands([cmdline_config:command()], non_neg_integer()) ->
        unicode:chardata().
format_commands(Commands, ColumnWidth) ->
  lists:map(fun ({command, Name, Description}) ->
                [string:pad(Name, ColumnWidth), "  ", Description, $\n]
            end, Commands).

-spec column_width(cmdline:config()) -> non_neg_integer().
column_width(Config) ->
  column_width(Config, 0).

-spec column_width(cmdline:config(), non_neg_integer()) -> non_neg_integer().
column_width([], Width) ->
  Width;
column_width([Entry = {flag, _, _, _} | Config], Width) ->
  LabelWidth = string:length(format_option_label(Entry)),
  column_width(Config, max(LabelWidth, Width));
column_width([Entry = {option, _, _, _, _, _} | Config], Width) ->
  LabelWidth = string:length(format_option_label(Entry)),
  column_width(Config, max(LabelWidth, Width));
column_width([{argument, Name, _} | Config], Width) ->
  column_width(Config, max(string:length(Name), Width));
column_width([{trailing_arguments, Name, _} | Config], Width) ->
  column_width(Config, max(string:length(Name), Width));
column_width([{command, Name, _} | Config], Width) ->
  column_width(Config, max(string:length(Name), Width)).
