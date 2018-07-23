Idris Empty Code Generator
--------------------------

https://eb.host.cs.st-andrews.ac.uk/drafts/compile-idris.pdf

build:

```bash
stack build

stack install (which should put idris-codegen-sdecl in path)

idris  --codegen sdecl  , you should see output simplified decls

```

This is an code non-generator for Idris. That is, it's a project which builds
an idris back end which doesn't actually do anything. You can use this as
a starting point for a real back end, by:

* Forking this repository (with a more descriptive name)
* Filling out `src/Main.hs`, fill the codegenJs function,useful info is at bottom 
* Renaming the binary from `idris-emptycg` to something appropriate for
  your back end

