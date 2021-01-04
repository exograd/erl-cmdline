% erl-cmdline changelog

# Next Version
## Features
- Add `cmdline:program_name/1`.
- Add `cmdline:process/3` and `cmdline:process/4` to automatically handle the
  help option, the help command and any error.
- Add `cmdline:process_command/2` and `cmdline:process_command/3` to simplify
  parsing of command options and arguments. This greatly simplifies programs
  with nested commands.
## Misc
- Remove the `handle_help` option and always add the help option and command.
- Introduce `cmdline:config/0` as alias for `cmdline_config:config/0`.

# 1.0.0
First public version.
