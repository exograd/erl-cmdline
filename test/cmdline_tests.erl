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

-module(cmdline_tests).

-include_lib("eunit/include/eunit.hrl").

-export([option_config/0, argument_config/0, full_config/0]).

option_config() ->
  [{flag, "a", undefined, ""},
   {flag, undefined, "b_opt", ""},
   {flag, "c", "c_opt", ""},
   {option, "x", undefined, "value", "x_default", ""},
   {option, undefined, "y_opt", "value", undefined, ""},
   {option, "z", "z_opt", "value", undefined, ""}].

argument_config() ->
  [{argument, "arg1", ""},
   {argument, "arg2", ""}].

full_config() ->
  option_config() ++ argument_config().

parse_invalid_options_test_() ->
  Config = option_config(),
  Parse = fun (Args) -> cmdline:parse("test", Args, Config) end,
  [?_assertEqual({error, truncated_short_option},
                 Parse(["-"])),
   ?_assertEqual({error, truncated_short_option},
                 Parse(["-a", "-", "-c"])),
   ?_assertEqual({error, {unknown_option, "u"}},
                 Parse(["-u"])),
   ?_assertEqual({error, {unknown_option, "u"}},
                 Parse(["-a", "-u"])),
   ?_assertEqual({error, {unknown_option, "unknown"}},
                 Parse(["--unknown"])),
   ?_assertEqual({error, {unknown_option, "unknown"}},
                 Parse(["-a", "--unknown", "--b_opt"])),
   ?_assertEqual({error, {missing_option_value, "x"}},
                 Parse(["-x"])),
   ?_assertEqual({error, {missing_option_value, "y_opt"}},
                 Parse(["--y_opt"])),
   ?_assertEqual({error, {missing_option_value, "z"}},
                 Parse(["-z"])),
   ?_assertEqual({error, {missing_option_value, "z_opt"}},
                 Parse(["--z_opt"]))].

parse_invalid_arguments_test_() ->
  Config = argument_config(),
  Parse = fun (Args) -> cmdline:parse("test", Args, Config) end,
  [?_assertEqual({error, missing_arguments},
                 Parse([])),
   ?_assertEqual({error, missing_arguments},
                 Parse(["foo"])),
   ?_assertEqual({error, unhandled_arguments},
                 Parse(["foo", "bar", "extra1"])),
   ?_assertEqual({error, unhandled_arguments},
                 Parse(["foo", "bar", "extra1", "extra2", "extra3"]))].

parse_test_() ->
  Args = ["-a", "--c_opt", "--y_opt", "1", "-z", "2", "foo", "bar"],
  {ok, C} = cmdline:parse("test", Args, full_config()),
  [?_assert(cmdline:has_option("a", C)),
   ?_assertNot(cmdline:has_option("b", C)),
   ?_assertNot(cmdline:has_option("b_opt", C)),
   ?_assert(cmdline:has_option("c", C)),
   ?_assert(cmdline:has_option("c_opt", C)),
   ?_assertEqual("x_default", cmdline:get_option("x", C)),
   ?_assertEqual("1", cmdline:get_option("y_opt", C)),
   ?_assertEqual("2", cmdline:get_option("z", C)),
   ?_assertEqual("2", cmdline:get_option("z_opt", C)),
   ?_assertEqual("foo", cmdline:get_argument("arg1", C)),
   ?_assertEqual("bar", cmdline:get_argument("arg2", C))].

parse_separator_test_() ->
  Args = ["--", "-a", "--unknown"],
  {ok, C} = cmdline:parse("test", Args, full_config()),
  [?_assertNot(cmdline:has_option("a", C)),
   ?_assertEqual("-a", cmdline:get_argument("arg1", C)),
   ?_assertEqual("--unknown", cmdline:get_argument("arg2", C))].

parse_separator_without_arguments_test_() ->
  Args = ["-a", "-x", "1", "--"],
  {ok, C} = cmdline:parse("test", Args, option_config()),
  [?_assert(cmdline:has_option("a", C)),
   ?_assert(cmdline:has_option("x", C)),
   ?_assertEqual("1", cmdline:get_option("x", C))].
