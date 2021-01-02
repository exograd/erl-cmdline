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

-export([find_option/2, find_argument/2, arguments/1,
         maybe_add_help_flag/1]).

-export_type([config/0, entry/0,
              optional_string/0]).

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
         Description :: string()}.

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

-spec find_argument(pos_integer(), config()) -> {ok, entry()} | error.
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

-spec arguments(config(), [entry()]) -> config().
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
