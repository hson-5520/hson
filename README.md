# HSON: Haskell's JSON
##### By: Aakash Jajoo (aakashj1@seas) & Yathushan Nadanapathan (yathu@seas)

HSON (pronounced H-sawn) is a library enabling interaction with [JSON](https://www.json.org/json-en.html) in Haskell. HSON is a data type that was created to mimic JSON objects. In order to validate JSON objects,
[JSON Schema](https://json-schema.org/) is commonly used in industry. HSONSchema is an internal represntation of JSON Schema in HSON. 

### Primary Usages
 - [x] Parse a JSON file into an HSON instance using [*FromJSON*](src/FromJSON.hs). 
  - [x] Translate an HSON instance to JSON using [*ToJSON*](src/ToJSON.hs).
  - [x] Parse a JSON Schema into an HSONSchema using [*FromJSONSchema*](src/FromJSONSchema.hs)
  - [x] Validate an HSON instances with an HSONSchema instance using [*ValidateHSON*](src/ValidateHSON.hs)


## Module Organization

#### [app](/app/)
* [Main.hs](app/Main.hs):
  * The entry point of the hson executable

#### [src](/src/)
- [Main.hs](app/Main.hs):
  * The entry point of the hson executable

#### [test](/test/)
- [Main.hs](app/Main.hs):
    -- The entry point of the hson executable

Haskell packages typically divide their source code into three separate places:
  - The bulk of your code should be developed as a reusable library in 
    modules in the `src` directory. We've created [Lib.hs](src/Lib.hs) 
    for you to get started. You can add additional modules here.
  
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

