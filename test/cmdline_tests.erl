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

-export([option_config/0, argument_config/0, command_config/0,
         full_config/0]).

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

command_config() ->
  [{command, "hello", ""},
   {command, "help", ""}].

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
   ?_assertEqual("x_default", cmdline:option("x", C)),
   ?_assertEqual("1", cmdline:option("y_opt", C)),
   ?_assertEqual("2", cmdline:option("z", C)),
   ?_assertEqual("2", cmdline:option("z_opt", C)),
   ?_assertEqual("foo", cmdline:argument("arg1", C)),
   ?_assertEqual("bar", cmdline:argument("arg2", C))].

parse_separator_test_() ->
  Args = ["--", "-a", "--unknown"],
  {ok, C} = cmdline:parse("test", Args, full_config()),
  [?_assertNot(cmdline:has_option("a", C)),
   ?_assertEqual("-a", cmdline:argument("arg1", C)),
   ?_assertEqual("--unknown", cmdline:argument("arg2", C))].

parse_separator_without_arguments_test_() ->
  Args = ["-a", "-x", "1", "--"],
  {ok, C} = cmdline:parse("test", Args, option_config()),
  [?_assert(cmdline:has_option("a", C)),
   ?_assert(cmdline:has_option("x", C)),
   ?_assertEqual("1", cmdline:option("x", C))].

parse_only_trailing_arguments_test_() ->
  Config = [{trailing_arguments, "args", ""} | option_config()],
  Parse = fun (Args) ->
              {ok, C} = cmdline:parse("test", Args, Config),
              cmdline:trailing_arguments(C)
          end,
  [?_assertEqual([], Parse([])),
   ?_assertEqual([], Parse(["-a", "-x", "1"])),
   ?_assertEqual(["foo"], Parse(["-a", "-x", "1", "foo"])),
   ?_assertEqual(["foo", "bar"], Parse(["-a", "-x", "1", "foo", "bar"])),
   ?_assertEqual(["foo", "bar"], Parse(["foo", "bar"]))].

parse_trailing_arguments_test_() ->
  Config = [{trailing_arguments, "args", ""} | full_config()],
  Parse = fun (Args) ->
              {ok, C} = cmdline:parse("test", Args, Config),
              cmdline:trailing_arguments(C)
          end,
  [?_assertEqual([], Parse(["a1", "a2"])),
   ?_assertEqual(["foo"], Parse(["a1", "a2", "foo"])),
   ?_assertEqual(["foo", "bar"], Parse(["a1", "a2", "foo", "bar"]))].

parse_unknown_command_test_() ->
  Config = option_config() ++ command_config(),
  Parse = fun (Args) -> cmdline:parse("test", Args, Config) end,
  [?_assertEqual({error, {unknown_command, "foo"}},
                 Parse(["foo"])),
   ?_assertEqual({error, {unknown_command, "foo"}},
                 Parse(["-a", "foo"]))].

parse_only_commands_test_() ->
  Config = option_config() ++ command_config(),
  Parse = fun (Args) ->
              {ok, C} = cmdline:parse("test", Args, Config),
              {cmdline:command(C), cmdline:command_arguments(C)}
          end,
  [?_assertEqual({"help", []},
                 Parse(["help"])),
   ?_assertEqual({"hello", ["bob"]},
                 Parse(["hello", "bob"])),
   ?_assertEqual({"hello", ["bob", "alice"]},
                 Parse(["hello", "bob", "alice"])),
   ?_assertEqual({"help", []},
                 Parse(["-a", "-x", "1", "help"])),
   ?_assertEqual({"hello", ["bob", "alice"]},
                 Parse(["-a", "-x", "1", "hello", "bob", "alice"]))].

parse_commands_test_() ->
  Config = full_config() ++ command_config(),
  Parse = fun (Args) ->
              {ok, C} = cmdline:parse("test", Args, Config),
              {cmdline:command(C), cmdline:command_arguments(C)}
          end,
  [?_assertEqual({"help", []},
                 Parse(["a1", "a2", "help"])),
   ?_assertEqual({"hello", ["bob"]},
                 Parse(["a1", "a2", "hello", "bob"])),
   ?_assertEqual({"hello", ["bob", "alice"]},
                 Parse(["a1", "a2", "hello", "bob", "alice"])),
   ?_assertEqual({"help", []},
                 Parse(["-a", "-x", "1", "a1", "a2", "help"])),
   ?_assertEqual({"hello", ["bob", "alice"]},
                 Parse(["-a", "-x", "1", "a1", "a2",
                        "hello", "bob", "alice"]))].
