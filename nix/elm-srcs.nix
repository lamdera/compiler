# The exact Elm package set needed to compile Elm code at BUILD time.
#
# Two Template Haskell splices compile Elm during the Haskell build, so this must
# cover the union of their dependencies:
#   - reactor/elm.json      (Develop.StaticFiles, the reactor frontend)
#   - vendor/elm-repl-worker/elm.json  (Lamdera.Repl, the repl worker)
#
# Originally reactor-only:
# (Develop.StaticFiles compiles the reactor frontend via Template Haskell,
# see terminal/src/Develop/StaticFiles/Build.hs). This must cover every
# direct and indirect dependency in reactor/elm.json.
#
# Hashes were produced with:
#   nix-prefetch-url https://github.com/<name>/archive/<version>.tar.gz
# and for the lamdera packages (their GitHub repos are private):
#   nix-prefetch-url https://static.lamdera.com/r/lamdera/<pkg>/pack-<version>.zip
# which is the same host the compiler itself downloads them from
# (extra/Lamdera/Extensions.hs). NOTE: unlike the GitHub tags, those pack
# zips are re-uploadable in principle; if the hash ever mismatches,
# re-run nix-prefetch-url and update it here.
{
  "elm/browser" = {
    version = "1.0.2";
    sha256 = "0nagb9ajacxbbg985r4k9h0jadqpp0gp84nm94kcgbr5sf8i9x13";
  };
  "elm/bytes" = {
    version = "1.0.8";
    sha256 = "02ywbf52akvxclpxwj9n04jydajcbsbcbsnjs53yjc5lwck3abwj";
  };
  "elm/core" = {
    version = "1.0.5";
    sha256 = "19w0iisdd66ywjayyga4kv2p1v9rxzqjaxhckp8ni6n8i0fb2dvf";
  };
  "elm/file" = {
    version = "1.0.5";
    sha256 = "1rljcb41dl97myidyjih2yliyzddkr2m7n74x7gg46rcw4jl0ny8";
  };
  "elm/html" = {
    version = "1.0.0";
    sha256 = "1n3gpzmpqqdsldys4ipgyl1zacn0kbpc3g4v3hdpiyfjlgh8bf3k";
  };
  "elm/http" = {
    version = "2.0.0";
    sha256 = "008bs76mnp48b4dw8qwjj4fyvzbxvlrl4xpa2qh1gg2kfwyw56v1";
  };
  "elm/json" = {
    version = "1.1.3";
    sha256 = "0kjwrz195z84kwywaxhhlnpl3p251qlbm5iz6byd6jky2crmyqyh";
  };
  "elm/parser" = {
    version = "1.1.0";
    sha256 = "0a3cxrvbm7mwg9ykynhp7vjid58zsw03r63qxipxp3z09qks7512";
  };
  # TWO versions, deliberately. Both build-time Elm projects are `application`
  # elm.json files, so their dependency versions are EXACT, and they disagree:
  # reactor/elm.json pins 1.0.1 and vendor/elm-repl-worker/elm.json pins 1.0.2.
  # Seeding only one makes the other fail in Elm's "details" phase, and
  # Lamdera.Repl discards the underlying error, so all you see is the phase name.
  "elm/project-metadata-utils" = [
    {
      version = "1.0.1";
      sha256 = "13vj9341lx4brpislv37d60gbf60fw6qzrg4dwaprx7xm5wqnlmx";
    }
    {
      version = "1.0.2";
      sha256 = "0r5dvbipcc0jhwlpaxv9iw1vrmsk1yxnycm0k4lvw1rbgikis7rv";
    }
  ];
  "elm/svg" = {
    version = "1.0.1";
    sha256 = "1cwcj73p61q45wqwgqvrvz3aypjyy3fw732xyxdyj6s256hwkn0k";
  };
  "elm/time" = {
    version = "1.0.0";
    sha256 = "0vch7i86vn0x8b850w1p69vplll1bnbkp8s383z7pinyg94cm2z1";
  };
  "elm/url" = {
    version = "1.0.0";
    sha256 = "0av8x5syid40sgpl5vd7pry2rq0q4pga28b4yykn9gd9v12rs3l4";
  };
  "elm/virtual-dom" = {
    version = "1.0.2";
    sha256 = "0q1v5gi4g336bzz1lgwpn5b1639lrn63d8y6k6pimcyismp2i1yg";
  };
  "elm-explorations/markdown" = {
    version = "1.0.0";
    sha256 = "0k3110ixa4wwf3vkkdplagwah9ypr965qxr1y147rnsc1xsxmr6y";
  };
  "lamdera/core" = {
    version = "1.0.0";
    sha256 = "1ny759bgnf6kb1vq94k6isi7sch8lv0gdq625n90xpykfhnlyc50";
    lamderaZip = true;
  };
  "lamdera/codecs" = {
    version = "1.0.0";
    sha256 = "03x8q6i250j2f9fidyfvvsdsl32k15xik294fwjq7yxc12mav3qc";
    lamderaZip = true;
  };

  # --- additionally required by vendor/elm-repl-worker ---
  # Lamdera.Repl compiles vendor/elm-repl-worker/src/Repl/Worker.elm with a
  # Template Haskell splice, so its dependencies must be in the offline package
  # set too. Missing them fails late and unhelpfully, as
  # "Error in Lamdera.Repl.lamderaRepl during the compiler details phase".
  "agu-z/elm-zip" = { version = "3.0.1"; sha256 = "1gd4bza80nx5w2bkk2nyfskclg0xvykmvg8akcb1gswbndq4y341"; };
  "cmditch/elm-bigint" = { version = "2.0.1"; sha256 = "00wfcvl3kg8l85cyljvgal51qksnqfqkc4g4diljggz6dcl155w2"; };
  "dasch/levenshtein" = { version = "1.0.3"; sha256 = "0hahn9cyjllz9n7ajgkkb55h6dllcvnzybn8d8kxaq4is2iwbkis"; };
  "miniBill/elm-unicode" = { version = "1.1.1"; sha256 = "1z0ydq2ylh1fx8mk11z4wf8cidb414sb66dmjvzyd7y6nbn75bsv"; };
  "rtfeldman/elm-hex" = { version = "1.0.0"; sha256 = "1y0aa16asvwdqmgbskh5iba6psp43lkcjjw9mgzj3gsrg33lp00d"; };
  "the-sett/elm-pretty-printer" = { version = "3.1.0"; sha256 = "0jv0g78dd8j9rf1bspcllslcld75snhmg8hgwcr1c041jkhg0a2w"; };
  "elm-community/basics-extra" = { version = "4.1.0"; sha256 = "0safacqk5dkcb0rdafdq1yhsch7dki26jjplbsgvfrm1j8klvjhn"; };
  "elm-community/list-extra" = { version = "8.7.0"; sha256 = "02grd0p5hc2gvdy4n723d1s28pm1grn95jrzic6jcgb26qh16vcc"; };
  "elm-community/maybe-extra" = { version = "5.3.0"; sha256 = "185jy9jxx3bqf0xl3rmdxfqqmxzcr084llf32glr6hgl5agshywk"; };
  "elm/regex" = { version = "1.0.0"; sha256 = "0lijsp50w7n1n57mjg6clpn9phly8vvs07h0qh2rqcs0f1jqvsa2"; };
  "folkertdev/elm-flate" = { version = "2.0.5"; sha256 = "1an9ps405l1rv8xyzkn1chbs1wx0ymlws5lhxad9rl8rr4am9wgm"; };
  "justinmimbs/date" = { version = "4.1.0"; sha256 = "11ybasla0nrczpgw1anrmcgqmvl1r0jkrcb9k2v2psng35iwi5ij"; };
  "justinmimbs/time-extra" = { version = "1.2.0"; sha256 = "1innw81fv3cfdx40rggzr4b6lvv6r28prar2b9i5a7isy00yagkw"; };
}
