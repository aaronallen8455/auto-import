# auto-import :inbox_tray:

A GHC plugin that automatically adds qualified import statements to the module being compiled.

### Usage

This plugin is intended to be used with GHCi or adjacent utilities such as
`ghcid` and `ghciwatch` as a developement tool, not as a package dependency.

You must first define a config file that specifies which modules are eligible
to be automatically imported. This file must be named `.autoimport` and can
be placed in the home directory and your project's directory. If both, the
configs will be combined with the local file taking precedence. Here is an
example `.autoimport` file:

```
Data.Text as T
Data.Text.IO as TIO
Data.Map as Map
UnliftIO
```

Note that modules can be given an optional explicit qualifier with `as`,
otherwise the full module name will be used as the qualifier. With the plugin
enabled and this config in scope, compiling the following module:

```haskell
tryReadFile :: FilePath -> IO (Either UnliftIO.IOException T.Text)
tryReadFile = UnliftIO.catchIO . TIO.readFile
```

Results in the module file being automatically updated to:

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified UnliftIO

tryReadFile :: FilePath -> IO (Either UnliftIO.IOException T.Text)
tryReadFile = UnliftIO.tryIO . TIO.readFile
```

The plugin works by intercepting GHC errors indicating that a qualifier (such
as `T`) is not imported, and if that qualifier matches a line from the config,
then the corresponding import statement is inserted. Compilation is aborted
when this occurs.

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
