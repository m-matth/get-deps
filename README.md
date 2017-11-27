# get-deps [![TravisCI](https://travis-ci.org/matth-/get-deps.svg)](https://travis-ci.org/matth-/get-deps) 

retrieve package dependencies from import in haskell source code


```
$ ./get-deps -i main.hs 
Cabal-ide-backend -- with 1 more candidate(s) ["Cabal"]
Unique
base
cmdargs
exposed-containers -- with 1 more candidate(s) ["containers"]
hackage-db
haskell-src-exts
text
unordered-containers
```


TODO :
  * be smarter with duplicate
  * fix speed issue and huge memory consumption
  * deal with version
  * output caba-like ```build-depends:``` field
  * find a better tool to achieve this :)
