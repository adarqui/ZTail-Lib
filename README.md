ZTail-Lib
===

An attempt to turn ZTail into a library. Check examples/ for some simple examples on how to use ZTail as a library. This is the best tail util i've encountered thus far.

The original ztail can be found here: http://hub.darcs.net/dylex/ztail


Build
--

```
make
make install
/usr/local/bin/ztail
```


Examples
---

```
./.cabal-sandbox/bin/ztail -c red /var/log/*.log
./.cabal-sandbox/bin/ztail-raw /var/log/*.log
```


Original README
---

An even more improved version of xtail/tail -f.

Requirements:
 - ghc
 - hinotify (http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hinotify)

Example:

```
  ztail -I -r300 \
	-c bright,red /var/log/kernel \
	-c yellow /var/log/daemon \
	-a -c blue -t '%b %d %H:%M:%S' -h '\@ ' /var/log/Xorg.0.log
```

TODO:
  configuration file (currently the command line can get unwieldy)

https://dylex.net:9947/src

Suggestions, improvements, patches, complaints welcome.
