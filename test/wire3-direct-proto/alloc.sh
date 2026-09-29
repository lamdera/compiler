#!/bin/sh
# prints: allocated MB, peak heap MB above start, retained MB of the result
node --expose-gc --trace-gc "$@" | python3 -c '
import sys, re
lines = sys.stdin.read().split("\n")
on = False; start = end = None; prev = None; alloc = 0.0; peak = 0.0
for l in lines:
    if l.startswith("@@START"): start = float(l.split()[1]); prev = start; on = True; continue
    if l.startswith("@@END"): end = float(l.split()[1]); alloc += end - prev; peak = max(peak, end); on = False; continue
    if l.startswith("@@RETAINED"): ret = float(l.split()[1]); continue
    if on:
        mm = re.search(r"(\d+\.\d+) \((\d+\.\d+)\) -> (\d+\.\d+)", l)
        if mm:
            before, after = float(mm.group(1)), float(mm.group(3))
            alloc += before - prev; peak = max(peak, before); prev = after
print("allocatedMB=%.0f peakAboveStartMB=%.0f resultMB=%.0f" % (alloc, peak - start, ret))
'
