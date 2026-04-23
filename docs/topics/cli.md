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

## Composing custom flags under `run`

`Ema.CLI.runArgsParser :: Parser RunArgs` is exported so downstream applications can splice Ema's `--host` / `--port` / `--no-ws` parser into their own `run` subcommand alongside custom flags. Prefer this over rebuilding `hostParser`/`portParser`/`noWebSocketParser` by hand — if Ema later grows a new run-mode field, the composed parser picks it up automatically.

```haskell
import Ema.CLI qualified
import Options.Applicative

data RunCmd = RunCmd
  { runEmaArgs :: Ema.CLI.RunArgs
  , runMyFlag  :: Maybe Int
  }

runCmdParser :: Parser RunCmd
runCmdParser = RunCmd
  <$> Ema.CLI.runArgsParser
  <*> optional (option auto (long "my-flag" <> metavar "N"))

-- When it's time to hand control to Ema, project RunCmd back into Ema.CLI.Cli:
toEmaCli :: RunCmd -> Ema.CLI.Cli
toEmaCli rc = Ema.CLI.Cli
  { Ema.CLI.action  = Ema.CLI.Run (runEmaArgs rc)
  , Ema.CLI.verbose = False
  }
```
