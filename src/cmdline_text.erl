-module(cmdline_text).

-export([text_to_binary/1, equal/2, member/2]).

-spec text_to_binary(cmdline:optional_string()) -> binary() | undefined.
text_to_binary(undefined) ->
  undefined;
text_to_binary(Text) ->
  case unicode:characters_to_binary(Text) of
    Bin when is_binary(Bin) ->
      Bin;
    Error ->
      error({invalid_unicode_data, Error, Text})
  end.

-spec equal(unicode:chardata(), unicode:chardata()) -> boolean().
equal(T1, T2) ->
  text_to_binary(T1) =:= text_to_binary(T2).

-spec member(unicode:chardata(), [unicode:chardata()]) -> boolean().
member(T, Ts) ->
  lists:any(fun (T2) -> equal(T2, T) end, Ts).
