#!/usr/bin/env escript

%%! -pa _build/default/lib/cmdline/ebin

-module(options_and_arguments).

-export([main/1]).

main(Args) ->
  io:setopts([{encoding, unicode}]),
  ProgramName = escript:script_name(),
  Cmdline = cmdline:process(ProgramName, Args, cmdline_config()),
  OutputPath = cmdline:option("o", Cmdline),
  Format = cmdline:argument("format", Cmdline),
  InputPaths = cmdline:trailing_arguments(Cmdline),
  case cmdline:is_option_set("verbose", Cmdline) of
    true ->
      io:format("reading form ~0tp~n", [InputPaths]),
      io:format("writing to ~ts using format ~ts~n", [OutputPath, Format]);
    false ->
      ok
  end.

-spec cmdline_config() -> cmdline:config().
cmdline_config() ->
  [{flag, "v", "verbose", "print debug information"},
   {option, "o", undefined, "path", "-", "the output file"},
   {argument, "format", "the output format"},
   {trailing_arguments, "path", "input files"}].
