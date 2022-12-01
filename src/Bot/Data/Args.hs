module Bot.Data.Args where

data Args = Args
  { mttrWebhookUrl :: String,
    feedUri :: String,
    summaryApiUrl :: Maybe String,
    summaryApiKey :: Maybe String,
    templateFilePath :: FilePath,
    cachePath :: FilePath,
    insecure :: Bool,
    dryRun :: Bool
  }
