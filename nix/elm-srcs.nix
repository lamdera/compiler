# The exact Elm package set needed to compile reactor/src at build time
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
  "elm/project-metadata-utils" = {
    version = "1.0.1";
    sha256 = "13vj9341lx4brpislv37d60gbf60fw6qzrg4dwaprx7xm5wqnlmx";
  };
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
    sha256 = "0h0a6vnnrg8g5gx28qwvzc064y079cnq62r5rq23c7jqbw9cxy78";
    lamderaZip = true;
  };
  "lamdera/codecs" = {
    version = "1.0.0";
    sha256 = "0k1dnp4pk9i2q76nx2xvpyzhv92g30zjbiw7g2qbjy54nvk7irvp";
    lamderaZip = true;
  };
}
