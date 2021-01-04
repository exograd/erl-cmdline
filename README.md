# Project
This repository contains an Erlang library providing functions to parse
command line arguments.

While similar to [getopt](https://github.com/jcomellas/getopt), erl-cmdline
improves argument handling and add support for commands.

# Example
Escripts demonstrating multiple use cases can be found [in the `example`
directory](./example). Note that the `cmdline` application must be built
before running it:

```sh
    make build
    escript example/options_and_arguments.erl -h
    escript example/commands.erl -h
```

# Documentation
A handbook is available [in the `doc`
directory](https://github.com/exograd/erl-cmdline/blob/master/doc/handbook.md).

# Contact
If you find a bug or have any question, feel free to open a GitHub issue or to
contact me [by email](mailto:khaelin@gmail.com).

Please note that I do not currently review or accept any contribution.
