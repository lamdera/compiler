#!/bin/sh
# Inject the prototype into the compiled output just before the IIFE closes.
# Field names: recover this build's ShortFieldNames from the Wire3 record-constructor
# lambdas (`{o: active0, ...}`), standing in for `Mode.Prod fields ! name`.
set -e
cd "$(dirname "$0")"
python3 - <<'PY'
import re
src = open('build/opt.js').read()
fields = {}
for short, name in re.findall(r'([A-Za-z_$][\w$]*): ([a-z]\w*)0\b(?=[,}])', src):
    if fields.get(name, short) != short:
        raise SystemExit('inconsistent short name for ' + name)
    fields[name] = short
gen = open('gen-prod.js').read() + '\n' + open('c1-proto.js').read() + '\n' + open('c3-proto.js').read()
gen = re.sub(r'\{\{(\w+)\}\}', lambda m: fields[m.group(1)], gen)
inj = open('w3rt.js').read() + '\n' + gen
i = src.rindex('}(this));')
open('build/opt-w3p.js', 'w').write(src[:i] + '\n' + inj + '\n' + src[i:])
print('fields:', ' '.join('%s=%s' % kv for kv in sorted(fields.items())))
PY
