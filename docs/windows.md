
### Testing with LDEBUG=1 in powershell

No way to do it once-off, so have to set the env:

```
$env:LDEBUG = '1';
```

And to remove:
```
Remove-Item Env:\LDEBUG
```
