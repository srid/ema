---
order: 4
---
# CLI

Every Ema site built with `Ema.runSite` exposes a small CLI out of the box. It picks one of two subcommands and a handful of flags, hands the result to your [[site]]'s `siteInput`, and does the right thing.

## Subcommands

- **`run`** — start the [[live-server]]. Also the default when no subcommand is given.
- **`gen DEST`** — do a one-shot static build into `DEST` and exit.

## Flags

Under `run`:

- `--host HOST` — interface to bind to. Defaults to `127.0.0.1`.
- `--port PORT` — TCP port. If omitted, a random free port is chosen and printed at startup.
- `--no-ws` — disable the websocket that powers hot-reload. The server still serves HTML, it just won't push updates.

Top-level:

- `-v`, `--verbose` — drop the log level from `Info` to `Debug`.

## How the CLI reaches your code

`Ema.runSite` parses `argv` into an `Ema.CLI.Cli` and routes the contained `Ema.CLI.Action` to your site: `Generate dest` triggers a static build, `Run RunArgs{..}` starts the live server. Your `EmaSite` instance receives the `Action` via `siteInput`, so you can condition behavior on whether Ema is generating or live-serving — e.g. skipping expensive watchers during a one-shot build.

## Handing Ema a pre-built `Cli`

If you want to control CLI parsing yourself — adding custom subcommands, re-ordering flags, or embedding Ema inside a larger tool — use `runSiteWith` instead of `runSite` and pass a `SiteConfig` carrying your own `Ema.CLI.Cli` value:

```haskell
import Ema qualified
import Ema.CLI qualified

main :: IO ()
main = do
  cli <- myCustomParser                       -- your own optparse-applicative
  let cfg = Ema.SiteConfig cli def
  void $ Ema.runSiteWith @MyRoute cfg mySiteArg
```

This is the escape hatch when the stock `Ema.CLI.cliParser` doesn't fit your UX.

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
