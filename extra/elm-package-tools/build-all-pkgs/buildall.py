"""

Build the latest version of all pkgs on package.elm-lang.org

Time to run: ~30min-1h30min

Method:
1. make an empty dir for each pkg to be tested, inside `./pkgs/`
2. `ELM_HOME=elm-home elm init` in said dir
3. `ELM_HOME=elm-home elm install author/project` in said dir
- perform steps 1-3 in parallel using a worker pool
- NOTE: this only tests the latest version of each pkg

Issues encountered:
- github rate limits requests, so this will hit the rate limit when downloading the elm source zipballs
- network speed/quality also impacts parallelism; reduce the worker pool size if you're on a slower connection, it'll automatically retry if it times out, but it'll be (very much) slower than it could be to run the whole script if the pool is much too large. If you see more than a few retries, the worker pool is too large.

"""
import itertools
import random
import subprocess
import os
from multiprocessing import Pool
import urllib.request
import json
import sys
from time import sleep
import random

def fn(authorpkg):
    print(">fn", authorpkg)
    p = "pkgs/" + authorpkg
    os.makedirs(p, exist_ok=True)
    sub = subprocess.run("cd " + p + "; yes | elmx init; yes | elmx install " + authorpkg, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, env=dict(os.environ, ELM_HOME="elm-home"))
    try:
        res = (authorpkg, sub.stdout.decode('utf-8'))
        if "HTTP PROBLEM" in res[1]:
            if "404" in res[1]:
                print("404 NotFound", authorpkg)

            if "Operation timed out" in res[1] or "ConnectionTimeout" in res[1] or "ConnectionFailure" in res[1] or "NoResponseDataReceived" in res[1]:
                print("retrying", res[0])
                sleep(random.randint(0,5))
                return fn(authorpkg)

            elif "Too Many" in res[1]: # rate limit
                spos = res[1].find('Retry-After","') + len('Retry-After","')
                res1s = res[1][spos:]
                epos = res1s.find('"')
                sleeptime = int(res1s[:epos])
                print("rate-limit", res[0], "sleeping for", sleeptime)
                sleep(sleeptime)
                return fn(authorpkg)

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
        results = p.map(fn, authorpkgs)
        #print(results)
        for res in results:
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

def getPkgs():
    with urllib.request.urlopen("https://package.elm-lang.org/all-packages/") as url:
        data = json.loads(url.read().decode())
        #print(data.keys())
        return sorted(list(data.keys()))
        #return sorted(random.sample(list(data.keys()), k=100))

def run():
    #bl = fns(["elm-community/graph"])#, "drathier/elm-graph", "AaronCZim/to-elm-format-string"])
    #bl = fns(["NoRedInk/elm-doodad", "arsduo/elm-ui-drag-drop", "bloom/elm-return", "elm-community/elm-lazy-list", "elm-community/elm-linear-algebra", "elm-community/elm-list-extra", "elm-community/elm-webgl", "eskimoblood/elm-wallpaper", "flowlang-cc/elm-audio-graph", "hanshoglund/elm-interval", "jonathanfishbein1/elm-comment", "krisajenkins/elm-dialog", "maca/crdt-replicated-graph", "pukkamustard/elm-identicon", "reserve-protocol/elm-i3166-data", "tesk9/elm-html-a11y", "drathier/elm-test-graph", "kkpoon/elm-echarts"])
    #bl = fns(["AaronCZim/to-elm-format-string", "adeschamps/mdl-context", "alexandrepiveteau/elm-ordt", "alvivi/elm-keyword-list", "alvivi/elm-nested-list", "ambuc/juggling-graph", "andre-dietrich/elm-svgbob", "antivanov/eunit", "apuchenkin/elm-nested-router", "arowM/elm-raw-html", "arsduo/elm-ui-drag-drop", "avh4/elm-meshes", "basti1302/elm-human-readable-filesize", "bloom/elm-return", "CallumJHays/elm-kalman-filter", "Chadtech/elm-loop", "chrisbuttery/elm-scroll-progress", "crazymykl/ex-em-elm", "dalen/elm-charts", "damienklinnert/elm-spinner", "damukles/elm-dialog", "danielnarey/elm-semantic-effects", "danyx23/elm-uuid", "declension/elm-obj-loader", "dosarf/elm-guarded-input", "drathier/elm-graph", "drathier/elm-test-graph", "drathier/elm-test-tables", "DrBearhands/elm-json-editor", "driebit/elm-ginger", "driebit/elm-html-unsafe-headers", "dzuk-mutant/internettime", "eeue56/elm-pretty-print-json", "eliaslfox/queue", "elm-community/elm-lazy-list", "elm-community/elm-linear-algebra", "elm-community/elm-list-extra", "elm-community/elm-webgl", "elm-community/graph", "elm-community/shrink", "elm-lang/html", "elm/svg", "elm/time", "enetsee/elm-color-interpolate", "eskimoblood/elm-wallpaper", "evancz/elm-markdown", "evancz/url-parser", "flowlang-cc/elm-audio-graph", "folkertdev/elm-treemap", "getto-systems/getto-elm-command", "Gizra/elm-keyboard-event", "guid75/ziplist", "h0lyalg0rithm/elm-select", "hanshoglund/elm-interval", "hendore/elm-port-message", "hendore/elm-temperature", "humio/elm-dashboard", "imbybio/outmessage-nested", "jackfranklin/elm-parse-link-header", "jahewson/elm-graphql-module", "Janiczek/elm-encoding", "javcasas/elm-integer", "jonathanfishbein1/elm-comment", "JustinLove/elm-twitch-api", "jxxcarlson/hex", "kennib/elm-maps", "kintail/elm-publish-test", "Kinto/elm-kinto", "kkpoon/elm-echarts", "klaftertief/elm-heatmap", "krisajenkins/elm-dialog", "ktonon/elm-serverless-auth-jwt", "ktonon/elm-word", "KtorZ/elm-notification", "lorenzo/elm-string-addons", "maca/crdt-replicated-graph", "MartinSStewart/elm-codec-bytes", "matthewrankin/elm-mdl", "mgold/elm-turtle-graphics", "MichaelCombs28/elm-mdl", "munksgaard/elm-data-uri", "naddeoa/stream", "nkotzias/elm-jsonp", "NoRedInk/elm-api-components", "NoRedInk/elm-doodad", "NoRedInk/http-upgrade-shim", "not1602/elm-feather", "nphollon/collision", "nphollon/geo3d", "ohanhi/elm-web-data", "opensolid/geometry", "Orasund/pixelengine", "PaackEng/elm-alert-beta", "pablen/toasty", "pablohirafuji/elm-syntax-highlight", "paul-freeman/elm-ipfs", "pilatch/elm-chess", "Pilatch/elm-simple-port-program", "pinx/elm-mdl", "prikhi/remote-status", "pukkamustard/elm-identicon", "reserve-protocol/elm-i3166-data", "robwhitaker/elm-uuid-stream", "RomanErnst/erl", "ryannhg/date-format", "scottcorgan/keyboard-combo", "simonh1000/elm-charts", "Skinney/elm-dict-exploration", "stil4m/elm-syntax", "tesk9/elm-html-a11y", "the-sett/lazy-list", "thought2/elm-wikimedia-commons", "truqu/elm-oauth2", "vipentti/elm-dispatch", "webbhuset/elm-actor-model-elm-ui", "xarvh/elm-dropdown-menu", "xerono/pinnablecache", "Zinggi/elm-game-resources", "Zinggi/elm-hash-icon"])
    bl = fns(getPkgs())

    def annot(things, annot):
        res = []
        for t in things:
            res += [t[0] + " => " + annot]
        return res

    #print(bl)
    print("="*30 + "\n    [already_installed: " + str(len(bl["already_installed"])) + "]\n")
    print("\n".join(annot(bl["already_installed"], "already_installed")))
    print("-"*30 + "\n    [success: " + str(len(bl["success"])) + "]\n")
    print("\n".join(annot(bl["success"], "success")))
    print("-"*30 + "\n    [old_deps: " + str(len(bl["old_deps"])) + "]\n")
    print("\n".join(annot(bl["old_deps"], "old_deps")))
    print("-"*30 + "\n    [http_problem: " + str(len(bl["http_problem"])) + "]\n")
    print("\n".join(annot(bl["http_problem"], "http_problem")))
    print("-"*30 + "\n    [corrupt_json: " + str(len(bl["corrupt_json"])) + "]\n")
    print("\n".join(annot(bl["corrupt_json"], "corrupt_json")))
    print("-"*30 + "\n    [corrupt_zip: " + str(len(bl["corrupt_zip"])) + "]\n")
    print("\n".join(annot(bl["corrupt_zip"], "corrupt_zip")))
    print("-"*30 + "\n    [corrupt_cache: " + str(len(bl["corrupt_cache"])) + "]\n")
    print("\n".join(annot(bl["corrupt_cache"], "corrupt_cache")))
    print("="*30)
    print("\n".join([(x[0] + " => failure => " + x[1]) for x in bl["failure"]]))
    print()
    print("#"*30)
    print("# totals:")
    print("already_installed:" + str(len(bl["already_installed"])))
    print("success:" + str(len(bl["success"])))
    print("old_deps:" + str(len(bl["old_deps"])))
    print("http_problem:" + str(len(bl["http_problem"])))
    print("corrupt_json:" + str(len(bl["corrupt_json"])))
    print("corrupt_zip:" + str(len(bl["corrupt_zip"])))
    print("corrupt_cache:" + str(len(bl["corrupt_cache"])))
    print("failure:" + str(len(bl["failure"])))


run()
