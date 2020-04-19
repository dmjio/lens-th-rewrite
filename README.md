lens-th-rewrite
=======================

A GHC plugin that rewrites TH splices into regular Haskell functions.

### Usage
```haskell
build-depends: lens-th-rewrite
ghc-options: -fplugin=GHC.Plugin.LensThRewrite
```

### Operation

```haskell
import Control.Lens

data Person
  = Person
  { _name :: String
  , _age :: Int
  } deriving (Show, Eq)

$(makeLenses ''Person)
```

becomes

```haskell
import Control.Lens

data Person
  = Person
  { _name :: String
  , _age :: Int
  } deriving (Show, Eq)

age :: Lens' Person Int
age = lens _age $ \record field -> record { _age = field }

name :: Lens' Person String
name = lens _name $ \record field -> record { _name = field }
```

### Why

Cross-compilation of Haskell code requires TH splices to be executed on the target machine, as opposed to the host.
This can be a non-starter for many projects.

### Limitations

Only `makeLenses` is supported. Also, you must define your data type in the same module `makeLenses` is used.

### Preprocessor

It is possible to use the executable bundled with this library as a pre-processor.

Add `{-# options_ghc -F -pgmF=lens-th-rewrite-pp #-}`

### Long term

GHC should split up Template Haskell into pure and impure variants. Allowing pure TH code to be executed on the host, and impure on the target.

