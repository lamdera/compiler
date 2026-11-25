{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Git where

{- The git side of `lamdera app create` and `app link`: the `lamdera` git remote, and
whether this machine can reach the app's repository on the Lamdera git host.

The connectivity check is `git ls-remote <the app's git remote>`, non-interactively. On the
git host (server/lamdera-receiver in the platform repo) sshd resolves the offered key to a
Lamdera account (the AuthorizedKeysCommand of nixos/groups/ssh-registry-auth.nix), and the
receiver handles any `git-*` command the same way: it authorises the account for the app
(`lx authorise`), creates the app's bare repository if it is new, and runs git-shell. So
`ls-remote` exercises exactly what the first `git push` will, also for a new app with no
deploys yet (an empty repository lists no refs and exits 0). A bare `ssh -T` is no good:
with no command the receiver does nothing and exits 0 for any registered key, without
checking the app.

It never uploads or registers a key: a refused key is reported, with the command to add
one (`lamdera keys add`).

Classification and messages are pure, so they are unit tested (test/Test/CLI.hs) on the
real outputs of ssh, git and the receiver.
-}

import Control.Exception (SomeException, try)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as T
import qualified System.Environment as Env
import System.Exit (ExitCode(..))
import qualified System.Process as Process
import qualified System.Timeout as Timeout



-- THE REMOTE


{-| The app's git remote, as the dashboard gives it (`gitRemote`), or the one every app has
had so far. -}
gitRemoteFor :: T.Text -> Maybe T.Text -> T.Text
gitRemoteFor appName given =
  case fmap T.strip given of
    Just url | not (T.null url) && url /= "-" -> url
    _ -> "git@apps.lamdera.com:" <> appName <> ".git"


{-| The host of a git remote: `git@apps.lamdera.com:app.git` and
`ssh://git@apps.lamdera.com:22/app.git` are both apps.lamdera.com. -}
remoteHost :: T.Text -> T.Text
remoteHost url =
  let
    withoutScheme =
      case T.breakOn "://" url of
        (_, rest) | not (T.null rest) -> T.drop 3 rest
        _ -> url
    withoutUser =
      case T.breakOn "@" withoutScheme of
        (_, rest) | not (T.null rest) && not ("/" `T.isInfixOf` T.takeWhile (/= '@') withoutScheme) -> T.drop 1 rest
        _ -> withoutScheme
  in
  T.takeWhile (\c -> c /= ':' && c /= '/') withoutUser


{-| Whether two remotes are the same repository, however they are spelt (`.git` or not,
scp-like or ssh://). -}
sameRemote :: T.Text -> T.Text -> Bool
sameRemote a b =
  normalise a == normalise b
  where
    normalise url =
      let
        trimmed = T.dropWhileEnd (== '/') (T.strip url)
        noGit = maybe trimmed id (T.stripSuffix ".git" trimmed)
        scpLike =
          case T.stripPrefix "ssh://" noGit of
            Just rest ->
              let (hostPart, path) = T.breakOn "/" rest in
              T.takeWhile (/= ':') hostPart <> ":" <> T.drop 1 path
            Nothing -> noGit
      in
      T.toLower scpLike


data Remote
  = RemoteAdded
  | RemoteExists          -- already the app's
  | RemoteConflict T.Text -- a `lamdera` remote for something else, left alone
  | NotARepo
  | RemoteFailed T.Text
  deriving (Show, Eq)


remoteStatus :: Remote -> T.Text
remoteStatus remote =
  case remote of
    RemoteAdded -> "added"
    RemoteExists -> "exists"
    RemoteConflict _ -> "conflict"
    NotARepo -> "notGitRepo"
    RemoteFailed _ -> "failed"


addCommand :: T.Text -> T.Text
addCommand url =
  "git remote add lamdera " <> url


{-| What happened to the `lamdera` remote, for people. -}
remoteMessage :: T.Text -> Remote -> T.Text
remoteMessage url remote =
  case remote of
    RemoteAdded -> "Added the git remote `lamdera`: " <> url
    RemoteExists -> "The git remote `lamdera` is already " <> url
    RemoteConflict other ->
      "This repository already has a `lamdera` remote, for " <> other <> ", so I left it alone. To deploy this app from here, point it at the app with `git remote set-url lamdera " <> url <> "`"
    NotARepo -> "This directory is not a git repository. In your project, run `git init` then `" <> addCommand url <> "`"
    RemoteFailed problem -> "I could not add the git remote `lamdera` (" <> problem <> "). Add it with `" <> addCommand url <> "`"


{-| Adds the `lamdera` remote for `url` to the repository in the current directory, unless
there is one already. -}
ensureRemote :: T.Text -> IO Remote
ensureRemote url = do
  (inside, _, _) <- git [ "rev-parse", "--is-inside-work-tree" ]
  if inside /= ExitSuccess
    then pure NotARepo
    else do
      (exit, existing, _) <- git [ "remote", "get-url", "lamdera" ]
      let existingUrl = T.strip (T.pack existing)
      if exit == ExitSuccess && not (T.null existingUrl)
        then pure $ if sameRemote existingUrl url then RemoteExists else RemoteConflict existingUrl
        else do
          (added, _, err) <- git [ "remote", "add", "lamdera", T.unpack url ]
          pure $ if added == ExitSuccess then RemoteAdded else RemoteFailed (T.strip (T.pack err))


git :: [String] -> IO (ExitCode, String, String)
git args = do
  r <- try (Process.readProcessWithExitCode "git" args "")
  case r of
    Right v -> pure v
    Left e -> pure (ExitFailure 127, "", show (e :: SomeException))



-- CONNECTIVITY


data Outcome
  = Reachable
  | AuthRefused   -- no key this machine offered is registered with Lamdera
  | NotAuthorised -- a registered key, of an account without access to the app
  | HostKey       -- ssh would not trust the host's key
  | Unreachable   -- DNS, refused, no route, connection closed
  | TimedOut
  | Failed
  deriving (Show, Eq)


outcomeStatus :: Outcome -> T.Text
outcomeStatus outcome =
  case outcome of
    Reachable -> "ok"
    AuthRefused -> "authRefused"
    NotAuthorised -> "notAuthorised"
    HostKey -> "hostKey"
    Unreachable -> "unreachable"
    TimedOut -> "timeout"
    Failed -> "failed"


{-| The `sshKey` field of the --json output. -}
sshKeyStatus :: Outcome -> T.Text
sshKeyStatus outcome =
  case outcome of
    Reachable -> "ok"
    AuthRefused -> "missing"
    NotAuthorised -> "otherAccount"
    _ -> "unknown"


{-| The outcome of `git ls-remote`, from its exit code and what it (and ssh, and the
receiver) wrote on stderr. -}
classify :: ExitCode -> T.Text -> Outcome
classify exit stderr_
  | exit == ExitSuccess = Reachable
  -- the receiver's `lx authorise`, for a key of another account (or an app that is not there)
  | has "key not authorized for this app" = NotAuthorised
  | has "host key verification failed" || has "remote host identification has changed" || has "host key for" && has "has changed" = HostKey
  | has "permission denied" = AuthRefused
  | has "timed out" || has "timeout" = TimedOut
  | any has
      [ "could not resolve hostname", "connection refused", "network is unreachable", "no route to host"
      , "connection closed", "connection reset", "kex_exchange_identification", "host is down"
      ] = Unreachable
  | otherwise = Failed
  where
    lower = T.toLower stderr_
    has s = s `T.isInfixOf` lower


{-| What is worth showing of stderr: without git's generic advice, the receiver's run id,
terminal colours or blank lines. -}
detail :: T.Text -> T.Text
detail stderr_ =
  T.intercalate " " $ take 4 $ filter keep $ fmap (T.strip . stripAnsi) (T.lines stderr_)
  where
    keep l =
      not (T.null l)
        && not (any (`T.isPrefixOf` l)
              [ "fatal: Could not read from remote repository", "Please make sure you have the correct access rights"
              , "and the repository exists", "runId:", "Load key"
              ])


stripAnsi :: T.Text -> T.Text
stripAnsi t =
  case T.breakOn "\ESC[" t of
    (before, rest) | not (T.null rest) ->
      before <> stripAnsi (T.drop 1 (T.dropWhile (\c -> not (Char.isAlpha c)) (T.drop 2 rest)))
    _ -> t


refusedMessage :: T.Text
refusedMessage =
  "Looks like this machine does not have a registered SSH key with Lamdera, run `lamdera keys add [filepath]`"


{-| The result of the check, for people. -}
outcomeMessage :: T.Text -> T.Text -> Outcome -> T.Text -> T.Text
outcomeMessage url appName outcome details =
  let
    host = remoteHost url
    retry = "then try again with `git ls-remote " <> url <> "`"
    because = if T.null details then "" else " (" <> details <> ")"
  in
  case outcome of
    Reachable ->
      "Ready: this machine can push to " <> appName <> ". Deploy with `lamdera deploy`, or `git push lamdera main`."
    AuthRefused ->
      refusedMessage
    NotAuthorised ->
      "This machine's SSH key is registered with Lamdera, but for an account without access to " <> appName
        <> ". Register a key of yours with `lamdera keys add [filepath]` and use it for " <> host <> ", or check the app name in the `lamdera` git remote."
    HostKey ->
      "SSH would not trust the host key of " <> host <> because <> ". If you did not expect it to change, check your network first. To accept the current one, remove the old one with `ssh-keygen -R " <> host <> "`, " <> retry <> "."
    Unreachable ->
      "I could not reach the Lamdera git host " <> host <> " over SSH" <> because <> ". Check your network, or a firewall blocking SSH (port 22), " <> retry <> "."
    TimedOut ->
      "Connecting to the Lamdera git host " <> host <> " timed out" <> because <> ". Check your network, or a firewall blocking SSH (port 22), " <> retry <> "."
    Failed ->
      "I could not check that this machine can push to " <> appName <> because <> ". See what happens with `git ls-remote " <> url <> "`."


{-| The options the check adds to ssh: never prompt (a passphrase, a password, a new host
key), give up on an unresponsive host quickly, and trust the Lamdera host's key on first
contact as a first `git push` would after a "yes". ssh takes the first value it is given for
an option, so options already in the person's own command win; their ssh config still
applies too. -}
checkSshOptions :: String
checkSshOptions =
  "-o BatchMode=yes -o ConnectTimeout=10 -o StrictHostKeyChecking=accept-new"


{-| The GIT_SSH_COMMAND for the check, from the person's GIT_SSH_COMMAND, core.sshCommand
and GIT_SSH (in git's order of precedence): their command with the check's options added,
or Nothing to leave a GIT_SSH program alone (it takes no options). -}
sshCommandFor :: Maybe String -> Maybe String -> Maybe String -> Maybe String
sshCommandFor gitSshCommand coreSshCommand gitSsh =
  case (nonEmpty gitSshCommand, nonEmpty coreSshCommand, nonEmpty gitSsh) of
    (Just command, _, _) -> Just (command <> " " <> checkSshOptions)
    (Nothing, Just command, _) -> Just (command <> " " <> checkSshOptions)
    (Nothing, Nothing, Just _) -> Nothing
    (Nothing, Nothing, Nothing) -> Just ("ssh " <> checkSshOptions)
  where
    nonEmpty m =
      case fmap (List.dropWhileEnd Char.isSpace . dropWhile Char.isSpace) m of
        Just s | not (null s) -> Just s
        _ -> Nothing


{-| Runs the check: the outcome and its detail. -}
check :: T.Text -> IO (Outcome, T.Text)
check url = do
  gitSshCommand <- Env.lookupEnv "GIT_SSH_COMMAND"
  gitSsh <- Env.lookupEnv "GIT_SSH"
  (_, core, _) <- git [ "config", "--get", "core.sshCommand" ]
  environment <- Env.getEnvironment
  let
    sshCommand = sshCommandFor gitSshCommand (Just core) gitSsh
    overrides =
      [ ("GIT_TERMINAL_PROMPT", "0") ]
        ++ maybe [] (\c -> [ ("GIT_SSH_COMMAND", c) ]) sshCommand
    env_ = overrides ++ filter (\(k, _) -> k `notElem` fmap fst overrides) environment
    process = (Process.proc "git" [ "ls-remote", T.unpack url ]) { Process.env = Just env_ }
  -- ConnectTimeout covers connecting; this covers a connection that then stalls. A timeout
  -- kills git (readCreateProcessWithExitCode cleans up its process).
  r <- try (Timeout.timeout 45000000 (Process.readCreateProcessWithExitCode process ""))
  pure $
    case r of
      Right (Just (exit, _, err)) ->
        let e = T.pack err in (classify exit e, detail e)
      Right Nothing ->
        (TimedOut, "")
      Left e ->
        (Failed, T.pack (show (e :: SomeException)))
