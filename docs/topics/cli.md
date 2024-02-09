---
order: 4
---
# CLI

Ema apps have a basic CLI argument structure that you can invoke in two possible ways:

1. `run` subcommand (or, no subcommand specified): Run the [[live-server]]. 
    - A random port is used unless you specify one explicitly (`--port`)
    - Pass `--no-ws` if you want to disable the "live" aspect of the server.
2. `gen` subcommand: Generate the static site, instead of starting up the [[live-server]]

The subcommand is passed as the `Ema.CLI.Action` type to your `siteInput` function in [[site]]. 

You can also use `runEmaWith` if you are manually handling the CLI arguments yourself and delegating to ema using `Ema.CLI` wherever appropriate.
