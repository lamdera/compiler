{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Keys where

{- `lamdera keys [list | add [FILE] | remove <NAME>]`: the SSH public keys registered with
your Lamdera account, which `git push` (and so `lamdera deploy`) authenticates with. The
same keys as on the dashboard's Account page. -}

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as FP
import System.IO (hFlush, hIsTerminalDevice, stdin, stdout)
import Text.Read (readMaybe)

import qualified Reporting.Doc as D
import Reporting.Doc ((<+>))

import Lamdera.CLI.Dashboard hiding (Common(..))
import qualified Lamdera.CLI.Dashboard as Dash
import qualified Lamdera.Progress as Progress


data Args
  = List
  | Add (Maybe FilePath)
  | Remove String
  deriving (Show, Eq)


data Flags =
  Flags
    { _json :: Bool
    , _yes :: Bool
    , _name :: Maybe String
    }
    deriving (Show, Eq)


common :: Flags -> Dash.Common
common flags =
  Dash.Common Nothing (_json flags) (_yes flags)


{-| The request; `Add` with the public key line, read from its file. -}
toRequest :: Args -> Maybe T.Text -> Flags -> Request
toRequest args keyLine flags =
  case args of
    List -> request "keyList" []
    Add _ ->
      request "keyAdd"
        [ ("sshKey", FString (maybe "" id keyLine))
        , ("name", maybe FNull (FString . T.pack) (_name flags))
        ]
    Remove name -> request "keyRemove" [ ("name", FString (T.pack name)) ]



-- KEY FILES


{-| The default public keys, in ssh's order of preference. -}
defaultKeys :: FilePath -> [FilePath]
defaultKeys home =
  fmap (\f -> home </> ".ssh" </> f) [ "id_ed25519.pub", "id_ecdsa.pub", "id_rsa.pub" ]


data KeyFile
  = PublicKey T.Text -- the key line
  | PrivateKey
  | NotAKey
  deriving (Show, Eq)


{-| What a key file holds. -}
keyFile :: T.Text -> KeyFile
keyFile contents =
  let
    lines' = filter (not . T.null) (fmap T.strip (T.lines contents))
    isPrivate = any (\l -> "PRIVATE KEY" `T.isInfixOf` l || "PuTTY-User-Key-File" `T.isPrefixOf` l) lines'
  in
  case lines' of
    _ | isPrivate -> PrivateKey
    first : _ | isKeyType (T.takeWhile (/= ' ') first) && length (T.words first) >= 2 -> PublicKey first
    _ -> NotAKey
  where
    isKeyType t =
      any (`T.isPrefixOf` t) [ "ssh-", "ecdsa-sha2-", "sk-ssh-", "sk-ecdsa-" ]


{-| Which key `keys add` without a file means: the one default key there is. -}
data Choice
  = NoKey
  | OneKey FilePath
  | SeveralKeys [FilePath]
  deriving (Show, Eq)


choose :: [FilePath] -> Choice
choose existing =
  case existing of
    [] -> NoKey
    [one] -> OneKey one
    several -> SeveralKeys several


{-| The key line of the file to add, or the problem reported and exit. -}
readKey :: Bool -> Maybe FilePath -> IO (FilePath, T.Text)
readKey jsonOut given = do
  home <- Dir.getHomeDirectory
  path <-
    case given of
      Just p -> pure p
      Nothing -> do
        existing <- Monad.filterM Dir.doesFileExist (defaultKeys home)
        case choose existing of
          OneKey p -> pure p
          NoKey ->
            failWith jsonOut $
              "There is no public key at " <> T.intercalate ", " (fmap (tilde home) (defaultKeys home))
                <> ". Pass the path of yours (`lamdera keys add ~/.ssh/my_key.pub`), or make one with `ssh-keygen -t ed25519` and run `lamdera keys add` again."
          SeveralKeys ps -> do
            tty <- hIsTerminalDevice stdin
            if tty && not jsonOut
              then pickKey home ps
              else
                failWith jsonOut $
                  "There are several public keys: " <> T.intercalate ", " (fmap (tilde home) ps)
                    <> ". Pass the one to add: `lamdera keys add " <> T.concat (fmap (tilde home) (take 1 ps)) <> "`"
  exists <- Dir.doesFileExist path
  Monad.unless exists $ failWith jsonOut ("There is no file at " <> T.pack path <> ".")
  contents <- T.readFile path
  case keyFile contents of
    PublicKey line -> pure (path, line)
    PrivateKey ->
      failWith jsonOut $
        T.pack path <> " is a private key, which never leaves your computer: pass its public half, the .pub file"
          <> (if FP.takeExtension path == ".pub" then "" else " (" <> T.pack (path <> ".pub") <> ")")
          <> "."
    NotAKey ->
      failWith jsonOut (T.pack path <> " does not look like an SSH public key (a line like `ssh-ed25519 AAAA... you@computer`).")


pickKey :: FilePath -> [FilePath] -> IO FilePath
pickKey home ps = do
  Progress.report $ said "There are several public keys:"
  mapM_ (\(i, p) -> T.putStrLn ("    " <> T.pack (show i) <> ". " <> tilde home p)) (zip [1 :: Int ..] ps)
  T.putStr "Which one should I add? Enter its number or a path: "
  hFlush stdout
  answer <- T.strip <$> T.getLine
  case readMaybe (T.unpack answer) of
    Just n | n >= 1 && n <= length ps -> pure (ps !! (n - 1))
    _ | not (T.null answer) -> pure (expand (T.unpack answer))
    _ -> failWith False "Cancelled, nothing was added."
  where
    expand p =
      case p of
        '~' : '/' : rest -> home </> rest
        _ -> p


tilde :: FilePath -> FilePath -> T.Text
tilde home path =
  case T.stripPrefix (T.pack home) (T.pack path) of
    Just rest | not (null home) -> "~" <> rest
    _ -> T.pack path



-- RUNNING


run :: Args -> Flags -> IO ()
run args flags = do
  let c = common flags
  keyLine <-
    case args of
      Add given -> Just . snd <$> readKey (_json flags) given
      _ -> pure Nothing
  ctx <- setup c AppOptional
  case args of
    Remove name -> confirm c ("remove the SSH key " <> T.pack name <> " from your Lamdera account (git push stops accepting it)") Nothing
    _ -> pure ()
  payload <- call ctx (toRequest args keyLine flags)
  output ctx payload (render args payload)


render :: Args -> Aeson.Value -> D.Doc
render args payload =
  case args of
    List ->
      case list payload of
        [] ->
          D.vcat
            [ said "You have no SSH keys registered with Lamdera."
            , "Add this machine's with `lamdera keys add`, so it can `git push` (and `lamdera deploy`) your apps."
            ]
        keys ->
          lines_ $
            table [ "NAME", "KEY", "ADDED", "LAST USED" ]
              (fmap (\k -> [ textAt "name" k, keyText k, timeAt "created" k, timeAt "used" k ]) keys)

    Add _ ->
      D.vcat
        [ said (D.dullgreen ("Added the SSH key" <+> D.fromChars (T.unpack (textAt "name" payload))))
        , "This machine can now `git push` (and `lamdera deploy`) your apps."
        ]

    Remove _ ->
      said (D.dullgreen ("Removed the SSH key" <+> D.fromChars (T.unpack (textAt "removed" payload))))


{-| A key for a listing: its fingerprint, or its type and the ends of the key itself. -}
keyText :: Aeson.Value -> T.Text
keyText k =
  case textAt "fingerprint" k of
    "-" ->
      case T.words (textAt "key" k) of
        tipe : blob : _ | T.length blob > 20 -> tipe <> " " <> T.take 8 blob <> "..." <> T.takeEnd 8 blob
        tipe : blob : _ -> tipe <> " " <> blob
        _ -> textAt "key" k
    fingerprint -> fingerprint
