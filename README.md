# HSON: Haskell's JSON
##### By: Aakash Jajoo (aakashj1@seas) & Yathushan Nadanapathan (yathu@seas)

HSON is a library enabling interaction with [JSON](https://www.json.org/json-en.html) in Haskell. HSON is a data type that was created to mimic JSON objects. In order to validate JSON objects,
[JSON Schema](https://json-schema.org/) is commonly used in industry. HSONSchema is an internal represntation of JSON Schema in HSON. 

### Primary Usages
 * Users can parse their JSON files into an HSON instance using [*FromJSON*](src/FromJSON.hs). 
 * Users can translate their HSON instances to JSON using [*ToJSON*](src/ToJSON.hs).
 * Users can parse their JSON Schema to HSONSchema using [*FromJSONSchema*](src/FromJSONSchema.hs)
 * Users can validate their HSON instances with an HSONSchema instance using [*ValidateHSON*](src/ValidateHSON.hs)

If you want to change the name of this project, look for all occurrences of
`project-cis5520` in the `project-cis5520.cabal` file and in the `hie.yaml` 
file. (And change the name of the cabal file to match your new name!)

## Module Organization

Haskell packages typically divide their source code into three separate places:
  - The bulk of your code should be developed as a reusable library in 
    modules in the `src` directory. We've created [Lib.hs](src/Lib.hs) 
    for you to get started. You can add additional modules here.
  
  - The entry point for your executable is in [Main.hs](app/Main.hs). 
  
  - All of your test cases should be in [the test directory](test/Spec.hs).

## Building, running, and testing

This project compiles with `stack build`. 
You can run the main executable with `stack run`.
You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.

## Importing additional libraries

This project is designed to run with stackage: you can easily use any library
in https://www.stackage.org/lts-19.19 by adding an entry to the
`build-depends` list of the `common-stanza` in the cabal file. If you want to
use a library that is not on stackage, you'll need to update the common-stanza
*and* add information to `stack.yaml` about where to find that library.

