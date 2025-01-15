# Working on cornelis

The project can be built and tested with both `stack` and `cabal`.  In order to
successfully run the test suite, both `agda` and `nvim` need to be on `$PATH`.

## Development using Nix

Nix integration is provided in the form of a Nix Flake.  To build the project,
simply run `nix build`.  For interactive development, use the provided shell:

```sh
# Launch a shell with all development dependencies installed
# (cabal, system libraries, etc.):
nix develop

# Run tests from a shell that includes a Neovim binary:
nix develop --command cabal test
```
