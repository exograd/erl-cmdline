% erl-pgc

# Introduction
The erl-cmdline project provides functions for command line processing.

# Usage
## Parsing
Command line arguments are parsed with either `cmdline:parse/3` or
`cmdline:parse/4`. Parsing requires the program name, the list of program
arguments and a configuration.

Example:
```erlang
Config = [{flag, "v", "verbose", "print debug information"},
          {option, "o", undefined, "path", "-", "the output file"},
          {argument, "format", "the output format"},
          {trailing_arguments, "path", "input files"}],
cmdline:parse(ProgrameName, Args, Config).
```

On success, parsing functions return `{ok, Cmdline}` where `Cmdline` is a
value that can be used to retrieve information about options, arguments and
commands.

On failure, parsing functions return `{error, Reason}` where `Reason`
describes the error. The `cmdline:format_error/1` function can be used to
obtain a human-readable error message from an error reason.

## Command line data
The following functions can be used to obtain information from a command line
value returned from a parsing function:

- `cmdline:program_name/1`: return the program name originally passed to the
  parsing function.
- `cmdline:is_option_set/2`: indicate whether an option has been defined or
  not. Note that options which have a default value are always defined even if
  they were not explicitely set in command line arguments.
- `cmdline:option/2`, `cmdline:option/3`: return the value associated with an
  option or `true` if the option is a flag. If the option was not set, the
  first function returns `undefined` while the second function returns a
  specific value.
- `cmdline:argument/2`: return the value of an argument.
- `cmdline:trailing_arguments/1`: return the list of values corresponding to
  trailing arguments.
- `cmdline:command/1`: return the name of the command used.
- `cmdline:command_arguments/2`: return the list of values following the
  command name in the list of command line arguments.

## Usage string
The `cmdline:usage/1` function can be used to obtain a human-readable usage
string from a command line value returned by a parsing option.

Note that using the `handle_help` parsing option will let parsing functions
take care of usage display.

# Configuration
A configuration is a list of entries defining elements of the command line.

## Options
Options are defined with the following tuple:

    {option, ShortName, LongName, Value, Default, Description}

Options without values are also called flags and are defined with the
following tuple:

    {flag, ShortName, LongName, Description}

- `ShortName`: a string containing a single character identifying the option,
  or `undefined`.
- `LongName`: a string identifying the option, or `undefined`.
- `Value`: a string indicating the type of value to be passed to the option;
  it is only used for display in usage strings.
- `Default`: a string containing the default value for the option, or
  `undefined` if the option has no default value.
- `Description` a string containing the description of the option; it is only
  used for display in usage strings.

Options must have at least one short name or one long name.

The behaviour of the application is undefined if a configuration contains
multiple options with the same names.

Options are set by using either the short name preceded by `-` or the long
name preceded by `--`. If an option is set multiple times, the only value
retained is the last one.

## Arguments
Arguments are defined with the following tuple:

    {argument, Name, Description}

- `Name`: a string identifying the argument.
- `Description` a string containing the description of the argument; it is
  only used for display in usage strings.

Arguments are mandatory.

## Trailing arguments
It is possible to capture multiple optional arguments after all options and
mandatory arguments using trailing arguments.

Trailing arguments are defined with the following tuple:

    {trailing_arguments, Name, Description}

- `Name`: a string identifying the arguments.
- `Description` a string containing the description of the arguments; it is
  only used for display in usage strings.

## Commands
Commands are defined with the following tuple:

    {command, Name, Description}

- `Name`: a string identifying the command.
- `Description` a string containing the description of the command; it is only
  used for display in usage strings.

A command is called by passing its name after all options and arguments.

Note that commands cannot be used if trailing arguments are defined.

## Parsing options
The following options can be passed to parsing functions:

- `handle_help`: a boolean indicating whether to automatically handle help
  options or not (default: false). When used, a `-h`/`--help` option is
  automatically added. If there is at least one command defined, a `help`
  command is also added. When the option is set, or when the command is used,
  the parsing function will print the usage string to the error output and
  exit with a status code equal to zero.
- `short_circuit_options`: a list of option names which, when set, stop
  processing of command line arguments after options and before arguments and
  commands. This can be used for option which interrupt the normal flow of the
  program, e.g. `--version` which usually prints a version string and exits.
