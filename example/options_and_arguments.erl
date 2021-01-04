#!/usr/bin/env escript

%%! -pa _build/default/lib/cmdline/ebin

-module(options_and_arguments).

-export([main/1]).

main(Args) ->
  io:setopts([{encoding, unicode}]),
  Arg0 = escript:script_name(),
  Options = #{handle_help => true},
  case cmdline:parse(Arg0, Args, cmdline_config(), Options) of
    {ok, Cmdline} ->
      OutputPath = cmdline:option("o", Cmdline),
      Format = cmdline:argument("format", Cmdline),
      InputPaths = cmdline:trailing_arguments(Cmdline),
      case cmdline:is_option_set("verbose", Cmdline) of
        true ->
          io:format("reading form ~0tp~n", [InputPaths]),
          io:format("writing to ~ts using format ~ts~n", [OutputPath, Format]);
        false ->
          ok
      end;
    {error, Reason} ->
      io:format(standard_error, "error: ~ts~n",
                [cmdline:format_error(Reason)]),
      erlang:halt(1)
  end.

-spec cmdline_config() -> cmdline_config:config().
cmdline_config() ->
  [{flag, "v", "verbose", "print debug information"},
   {option, "o", undefined, "path", "-", "the output file"},
   {argument, "format", "the output format"},
   {trailing_arguments, "path", "input files"}].
