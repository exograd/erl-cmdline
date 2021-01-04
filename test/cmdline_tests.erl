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
  [{flag, "a", undefined,
    "a short flag"},
   {flag, undefined, "b_opt",
    "a long flag"},
   {flag, "c", "c_opt",
    "a flag with both short and long names"},
   {option, "x", undefined, "value", "x_default",
    "an option with a default value"},
   {option, undefined, "y_opt", "value", undefined,
    "a long option"},
   {option, "z", "z_opt", "value", undefined,
    "an option with both short and long names"}].

argument_config() ->
  [{argument, "arg1", "the first argument"},
   {argument, "arg2", "the second argument"}].

command_config() ->
  [{command, "hello", "say hello"},
   {command, "bye", "say bye"}].

full_config() ->
  option_config() ++ argument_config().

parse_invalid_options_test_() ->
  Config = option_config(),
  Options = #{program_name => "test"},
  Parse = fun (Args) -> cmdline:parse(Args, Config, Options) end,
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
  Options = #{program_name => "test"},
  Parse = fun (Args) -> cmdline:parse(Args, Config, Options) end,
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
  Options = #{program_name => "test"},
  {ok, C} = cmdline:parse(Args, full_config(), Options),
  [?_assert(cmdline:is_option_set("a", C)),
   ?_assertNot(cmdline:is_option_set("b", C)),
   ?_assertNot(cmdline:is_option_set("b_opt", C)),
   ?_assert(cmdline:is_option_set("c", C)),
   ?_assert(cmdline:is_option_set("c_opt", C)),
   ?_assertEqual("x_default", cmdline:option("x", C)),
   ?_assertEqual("1", cmdline:option("y_opt", C)),
   ?_assertEqual("2", cmdline:option("z", C)),
   ?_assertEqual("2", cmdline:option("z_opt", C)),
   ?_assertEqual("foo", cmdline:argument("arg1", C)),
   ?_assertEqual("bar", cmdline:argument("arg2", C))].

parse_separator_test_() ->
  Args = ["--", "-a", "--unknown"],
  Options = #{program_name => "test"},
  {ok, C} = cmdline:parse(Args, full_config(), Options),
  [?_assertNot(cmdline:is_option_set("a", C)),
   ?_assertEqual("-a", cmdline:argument("arg1", C)),
   ?_assertEqual("--unknown", cmdline:argument("arg2", C))].

parse_separator_without_arguments_test_() ->
  Args = ["-a", "-x", "1", "--"],
  Options = #{program_name => "test"},
  {ok, C} = cmdline:parse(Args, option_config(), Options),
  [?_assert(cmdline:is_option_set("a", C)),
   ?_assert(cmdline:is_option_set("x", C)),
   ?_assertEqual("1", cmdline:option("x", C))].

parse_only_trailing_arguments_test_() ->
  Config = [{trailing_arguments, "args", ""} | option_config()],
  Options = #{program_name => "test"},
  Parse = fun (Args) ->
              {ok, C} = cmdline:parse(Args, Config, Options),
              cmdline:trailing_arguments(C)
          end,
  [?_assertEqual([], Parse([])),
   ?_assertEqual([], Parse(["-a", "-x", "1"])),
   ?_assertEqual(["foo"], Parse(["-a", "-x", "1", "foo"])),
   ?_assertEqual(["foo", "bar"], Parse(["-a", "-x", "1", "foo", "bar"])),
   ?_assertEqual(["foo", "bar"], Parse(["foo", "bar"]))].

parse_trailing_arguments_test_() ->
  Config = [{trailing_arguments, "args", ""} | full_config()],
  Options = #{program_name => "test"},
  Parse = fun (Args) ->
              {ok, C} = cmdline:parse(Args, Config, Options),
              cmdline:trailing_arguments(C)
          end,
  [?_assertEqual([], Parse(["a1", "a2"])),
   ?_assertEqual(["foo"], Parse(["a1", "a2", "foo"])),
   ?_assertEqual(["foo", "bar"], Parse(["a1", "a2", "foo", "bar"]))].

parse_unknown_command_test_() ->
  Config = option_config() ++ command_config(),
  Options = #{program_name => "test"},
  Parse = fun (Args) -> cmdline:parse(Args, Config, Options) end,
  [?_assertEqual({error, {unknown_command, "foo"}},
                 Parse(["foo"])),
   ?_assertEqual({error, {unknown_command, "foo"}},
                 Parse(["-a", "foo"]))].

parse_only_commands_test_() ->
  Config = option_config() ++ command_config(),
  Options = #{program_name => "test"},
  Parse = fun (Args) ->
              {ok, C} = cmdline:parse(Args, Config, Options),
              {cmdline:command(C), cmdline:command_arguments(C)}
          end,
  [?_assertEqual({"bye", []},
                 Parse(["bye"])),
   ?_assertEqual({"hello", ["bob"]},
                 Parse(["hello", "bob"])),
   ?_assertEqual({"hello", ["bob", "alice"]},
                 Parse(["hello", "bob", "alice"])),
   ?_assertEqual({"bye", []},
                 Parse(["-a", "-x", "1", "bye"])),
   ?_assertEqual({"hello", ["bob", "alice"]},
                 Parse(["-a", "-x", "1", "hello", "bob", "alice"]))].

parse_commands_test_() ->
  Config = full_config() ++ command_config(),
  Options = #{program_name => "test"},
  Parse = fun (Args) ->
              {ok, C} = cmdline:parse(Args, Config, Options),
              {cmdline:command(C), cmdline:command_arguments(C)}
          end,
  [?_assertEqual({"bye", []},
                 Parse(["a1", "a2", "bye"])),
   ?_assertEqual({"hello", ["bob"]},
                 Parse(["a1", "a2", "hello", "bob"])),
   ?_assertEqual({"hello", ["bob", "alice"]},
                 Parse(["a1", "a2", "hello", "bob", "alice"])),
   ?_assertEqual({"bye", []},
                 Parse(["-a", "-x", "1", "a1", "a2", "bye"])),
   ?_assertEqual({"hello", ["bob", "alice"]},
                 Parse(["-a", "-x", "1", "a1", "a2",
                        "hello", "bob", "alice"]))].

parse_short_circuit_options_test_() ->
  Config = [{flag, "x", "x_opt", ""},
            {option, "y", "y_opt", "value", undefined, ""},
            {argument, "arg", ""}],
  Parse = fun (Args, ShortCircuitOpts) ->
              Options = #{program_name => "test",
                          short_circuit_options => ShortCircuitOpts},
              cmdline:parse(Args, Config, Options)
          end,
  [?_assertMatch({error, missing_arguments},
                 Parse([], [])),
   ?_assertMatch({error, missing_arguments},
                 Parse(["-x"], [])),
   ?_assertMatch({ok, #{options := #{"x" := true}}},
                 Parse(["-x"], ["x"])),
   ?_assertMatch({error, missing_arguments},
                 Parse(["-y", "1"], ["x"])),
   ?_assertMatch({ok, #{options := #{"x" := true}}},
                 Parse(["-x", "foo"], ["x"])),
   ?_assertMatch({ok, #{options := #{"y" := "1"}}},
                 Parse(["-y", "1"], ["x", "y"])),
   ?_assertMatch({ok, #{options := #{"x" := true}}},
                 Parse(["-x", "-y", "1"], ["x", "y"])),
   ?_assertMatch({ok, #{options := #{"y" := "1"}}},
                 Parse(["-y", "1", "-x"], ["x", "y"]))].
