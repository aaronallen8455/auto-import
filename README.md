# auto-import :inbox_tray:

A GHC plugin that automatically adds import statements to the module being compiled.

### Usage

This plugin is intended to be used with GHCi or adjacent utilities such as
`ghcid` and `ghciwatch` as a developement tool, not as a package dependency.

You must first define a config file that specifies which modules are eligible
to be automatically imported. By default this file is named `.autoimport` and
can be placed in the home directory and/or in the root directory of your
Haskell project. If the file is found in both locations, the configs will be
merged with the local file taking precedence. The local file path can be overriden
by passing this plugin option to GHC: `fplugin-opt=AutoImport:--cfg=path/to/file`.

Here is an example `.autoimport` file:

```
Data.Text as T
Data.Text.IO as TIO
Data.Map (Map)
Data.Map as Map
Data.Maybe (fromMaybe, isJust, isNothing)
Data.Functor.Identity (Identity(runIdentity))
UnliftIO
```

There are two ways of specifying modules that should be automatically imported:
with a qualifier or with a list of identifiers.

Modules are identified by a qualifier if there is no list of identifiers
following the module name, along with an optional explicit qualifier using
`as`. If no explicit qualifier is given then the full module name is used as
the qualifier (such as `UnliftIO` in the example). Qualified imports will be
added for these modules whenever the qualifier is used in a module where it
does not correspond to an existing import.

If a list of identifiers is given then that module will be imported unqualified
with an explicit import list (or added to an existing import if the module is
already imported) when one of the given identifiers is used in a context where
it does not have a definition in scope. Note: wildcards for type and class
subterms is not supported, i.e. `Identity(..)` is not allowed in the config.

With the plugin enabled and the example config in scope, compiling the
following module:

```haskell
tryReadFile :: FilePath -> IO (Either UnliftIO.IOException T.Text)
tryReadFile = UnliftIO.catchIO . TIO.readFile

defaultToZero :: Maybe Int -> Int
defaultToZero = fromMaybe 0
```

Results in the module file being automatically updated to:

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified UnliftIO
import Data.Maybe (fromMaybe)

tryReadFile :: FilePath -> IO (Either UnliftIO.IOException T.Text)
tryReadFile = UnliftIO.tryIO . TIO.readFile

defaultToZero :: Maybe Int -> Int
defaultToZero = fromMaybe 0
```

(Note: the above would actually need to be compiled twice because GHC does not
emit the 'missing qualifier' and 'out of scope variable' errors together. The
missing qualifier errors occur first.)

The plugin works by intercepting GHC errors indicating that a qualifier or
identifier is not defined, and if it matches a line from the config, then the
corresponding import statement is inserted. Compilation is aborted when this
occurs.

Here is an example command for starting a REPL for a stack project with the
`auto-import` plugin enabled (you may need to add `auto-import` to your
`extra-deps` first):

```
stack repl my-project --package auto-import --ghci-options='-fplugin AutoSplit'
```

likewise for a cabal project (you may need to run `cabal update` first):

```
cabal repl my-project --build-depends auto-import --repl-options='-fplugin AutoSplit'
```

This plugin aims to support the four most recent major GHC releases.
