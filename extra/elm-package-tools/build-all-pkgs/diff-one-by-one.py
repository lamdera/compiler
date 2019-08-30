"""

Build the latest version of all pkgs on package.elm-lang.org

Time to run: ~1h-3h

Method:
1. make an empty dir called `elm` for the pkg to be tested, inside `./pkg-diff/`
2. `ELM_HOME=elm-home elm init` in said dir
3. `ELM_HOME=elm-home elm install author/project` in said dir
4. make an empty dir called `elmx` for the pkg to be tested, inside `./pkg-diff/`
5. `ELM_HOME=elm-home elmx init` in said dir
6. `ELM_HOME=elm-home elmx install author/project` in said dir
7. diff the results; if there's a diff, print it out.
- perform steps 1-7 for different pkgs in parallel using a worker pool
- NOTE: this only tests the latest version of each pkg

Issues encountered:
- github rate limits requests, so this will hit the rate limit when downloading the elm source zipballs
- network speed/quality also impacts parallelism; reduce the worker pool size if you're on a slower connection, it'll automatically retry if it times out, but it'll be (very much) slower than it could be to run the whole script if the pool is much too large. If you see more than a few retries, the worker pool is too large.

"""

import itertools
import difflib
import random
import subprocess
import os
from multiprocessing import Pool
import urllib.request
import json
import sys
from time import sleep
import re

known_unsupported_packages = ["Skinney/elm-deque", "1602/json-schema", "json-tools/json-schema"]

def fnwr(authorpkg):
    elmres = fn(authorpkg, p="pkg-diff/" + "elm" + "/" + authorpkg, binary="elm")
    elmxres = fn(authorpkg, p="pkg-diff/" + "elmx" + "/" + authorpkg, binary="elmx")

    def stripBeforeNeedle(s):
        p1 = s.find("Building dependencies") - 1
        s1 = s[p1:]
        s2 = re.sub(r'Building dependencies .+\n', '', s1)  # drop all `Building dependencies (8/13)` lines
        return s2

    a = stripBeforeNeedle(elmres[1])
    b = stripBeforeNeedle(elmxres[1])
    if a != b:
        print("#" * 30, "DIFF", "###", authorpkg, "#" * 10)
        # print("elm")
        # sys.stdout.write(elmres[1])
        # print("elmx")
        # sys.stdout.write(elmxres[1])
        # print("diff")
        print("\n".join(difflib.unified_diff(a.splitlines(), b.splitlines())))
        print("#" * 30)
    return None


def fn(authorpkg, p, binary):
    print(">fn", authorpkg, p, binary)
    os.makedirs(p, exist_ok=True)
    cmd = "cd " + p + "; yes | " + binary + " init; yes | " + binary + " install " + authorpkg
    # print("cmd", cmd)
    sub = subprocess.run(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, env=dict(os.environ, ELM_HOME="elm-home"))
    try:
        res = (authorpkg, sub.stdout.decode('utf-8'))
        if "HTTP PROBLEM" in res[1]:
            if "404" in res[1]:
                print("404 NotFound", authorpkg)

            if "Operation timed out" in res[1] or "ConnectionTimeout" in res[1] or "ConnectionFailure" in res[1] or "NoResponseDataReceived" in res[1]:
                print("retrying", res[0])
                sleep(random.randint(0, 5))
                return fn(authorpkg, p, binary)

            elif "Too Many" in res[1]:  # rate limit
                spos = res[1].find('Retry-After","') + len('Retry-After","')
                res1s = res[1][spos:]
                epos = res1s.find('"')
                sleeptime = int(res1s[:epos])
                print("rate-limit", res[0], "sleeping for", sleeptime)
                sleep(sleeptime)
                return fn(authorpkg, p, binary)

        print("<fn", authorpkg)
        return (authorpkg, sub.stdout.decode('utf-8'))
    except UnicodeDecodeError:
        print("UnicodeDecodeError in", authorpkg)
        return None


def fns(authorpkgs):
    already_installed = []
    success = []
    old_deps = []
    http_problem = []
    corrupt_json = []
    corrupt_zip = []
    corrupt_cache = []
    failure = []

    with Pool(8) as p:
        results = p.map(fnwr, authorpkgs)
        # print(results)
        for res in results[0:0]:
            if not res:
                continue
            elif "HTTP PROBLEM" in res[1]:
                print("http problem", res[0])
                sys.stdout.write(res[1])
                http_problem += [res]
            elif "CORRUPT ZIP" in res[1]:
                corrupt_zip += [res]
            elif "CORRUPT JSON" in res[1]:
                corrupt_json += [res]
            elif "CORRUPT CACHE" in res[1]:
                corrupt_cache += [res]
            elif "OLD DEPENDENCIES" in res[1]:
                old_deps += [res]
            elif "already installed" in res[1]:
                already_installed += [res]
            elif "Dependencies ready" in res[1]:
                success += [res]
            else:
                failure += [res]
        # done
        return \
            dict(
                already_installed=sorted(already_installed),
                success=sorted(success),
                old_deps=sorted(old_deps),
                http_problem=sorted(http_problem),
                corrupt_json=sorted(corrupt_json),
                corrupt_zip=sorted(corrupt_zip),
                corrupt_cache=sorted(corrupt_cache),
                failure=sorted(failure),
            )


def get_pkgs():
    with urllib.request.urlopen("https://package.elm-lang.org/all-packages/") as url:
        data = json.loads(url.read().decode())
        # print(data.keys())
        return sorted(list(data.keys()))
        # return sorted(random.sample(list(data.keys()), k=100))


def run():
    bl = []
    #bl += ["elm/http", "elm/virtual-dom"]
    bl += ["elm/http", "elm/virtual-dom", "owanturist/elm-union-find", "1602/json-schema", "EdutainmentLIVE/elm-bootstrap", "Skinney/elm-deque", "Libbum/elm-partition", "bluedogtraining/bdt-elm", "MartinSStewart/elm-codec-bytes", "PaackEng/elm-svg-string", "commonmind/elm-csexpr", "eriktim/elm-protocol-buffers", "ceddlyburge/elm-bootstrap-starter-master-view", "elm-community/graph", "elm-toulouse/cbor", "folkertdev/elm-brotli", "holmusk/timed-cache", "folkertdev/elm-flate", "malinoff/elm-jwt", "json-tools/json-schema", "ivadzy/bbase64", "justgook/elm-webdriver", "maca/crdt-replicated-tree", "ozmat/elm-forms", "miyamoen/bibliopola", "miyamoen/elm-command-pallet", "miyamoen/tree-with-zipper", "niho/json-schema-form", "rundis/elm-bootstrap", "timjs/elm-collage", "turboMaCk/lazy-tree-with-zipper", "vViktorPL/elm-jira-connector", "valentinomicko/test-forms", "tomjkidd/elm-multiway-tree-zipper", "zwilias/elm-html-string"]   # broken commits as of 2019-08-04
    bl += ["EdutainmentLIVE/elm-bootstrap", "alexandrepiveteau/elm-ordt", "Kinto/elm-kinto", "Orasund/elm-cellautomata", "PaackEng/elm-svg-string", "dasch/levenshtein", "dasch/parser", "elm-explorations/benchmark", "ceddlyburge/elm-bootstrap-starter-master-view", "driebit/elm-ginger", "joneshf/elm-tagged", "mdgriffith/style-elements", "m-mullins/elm-console", "swiftengineer/elm-data", "zwilias/elm-html-string", "wittjosiah/elm-ordered-dict"]   # broken commits as of 2019-08-26
    #bl += ["1602/json-schema", "Kinto/elm-kinto", "json-tools/json-schema", "m-mullins/elm-console", "niho/json-schema-form", "swiftengineer/elm-data"]
    #bl += ["Skinney/elm-deque"]
    #bl += ["owanturist/elm-union-find"]
    bl = get_pkgs()

    def annot(things, annot):
        res = []
        for t in things:
            res += [t[0] + " => " + annot]
        return res

    # run compiler a bunch of times
    bl = list(set(bl) - set(known_unsupported_packages))
    bl = fns(sorted(bl))


run()
