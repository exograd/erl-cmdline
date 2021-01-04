#!/usr/bin/env escript

%%! -pa _build/default/lib/cmdline/ebin

-module(commands).

-export([main/1]).

main(Args) ->
  io:setopts([{encoding, unicode}]),
  ProgramName = escript:script_name(),
  Config = [{command, "foo", "the foo command"},
            {command, "bar", "the bar command"}],
  Cmdline = cmdline:process(ProgramName, Args, Config),
  main(cmdline:command(Cmdline), Cmdline).

main("foo", Cmdline) ->
  Config = [{argument, "a", "just an argument"}],
  Cmdline2 = cmdline:process_command(Cmdline, Config),
  io:format("a: ~tp~n", [cmdline:argument("a", Cmdline2)]);

main("bar", Cmdline) ->
  Config = [{argument, "a", "just an argument"},
            {argument, "b", "another argument"}],
  Cmdline2 = cmdline:process_command(Cmdline, Config),
  io:format("a: ~tp~n", [cmdline:argument("a", Cmdline2)]),
  io:format("b: ~tp~n", [cmdline:argument("b", Cmdline2)]).
