% erl-cmdline changelog

# Next Version

# 1.1.0
## Features
- Add `cmdline:program_name/1`.
- Add `cmdline:process/3` and `cmdline:process/4` to automatically handle the
  help option, the help command and any error.
- Add `cmdline:process_command/2` and `cmdline:process_command/3` to simplify
  parsing of command options and arguments. This greatly simplifies programs
  with nested commands.
## Bugs
- Handle `missing_command` errors in the error formatting function.
## Misc
- Remove the `handle_help` option and always add the help option and command.
- Introduce `cmdline:config/0` as alias for `cmdline_config:config/0`.
- Add a `program_name` option for the program name and use
  `escript:script_name/0` by default.

# 1.0.0
First public version.
