#!/usr/bin/env bash
#
# Regenerates nix/package-replacements/ from the git submodules in
# extra/package-replacements/.
#
# The compiler embeds, at build time, the files that each package-replacement
# submodule changes relative to its upstream (git diff origin/master...) plus
# the submodule's commit hash (see extra/Lamdera/PackageReplacementsTH.hs).
# Nix builds cannot run git (no .git directories in the flake source, no
# network in the sandbox), so those answers are snapshotted here and staged
# into the build by flake.nix, which also verifies that the recorded commits
# still match extra/package-replacements/lock.txt.
#
# Run this after bumping any submodule in extra/package-replacements/
# (i.e. whenever you run extra/package-replacements/lock.sh commit).
set -euo pipefail

cd "$(dirname "$0")/.."

out=nix/package-replacements
rm -rf "$out"
mkdir -p "$out"

for dir in extra/package-replacements/*/*/; do
    dir=${dir%/}
    rel=${dir#extra/package-replacements/}
    mkdir -p "$out/$(dirname "$rel")"
    git -C "$dir" rev-parse HEAD > "$out/$rel.commit"
    # origin/master...HEAD (not the bare "origin/master..." the compiler runs
    # in dev builds): nix fetches submodules at their recorded commit, so the
    # snapshot must describe the committed state, not the local working tree.
    git -C "$dir" diff "origin/master...HEAD" --name-only > "$out/$rel.files"
    echo "$rel: $(cat "$out/$rel.commit")"
done

echo "Wrote $out/"
