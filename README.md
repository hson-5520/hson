# HSON: Haskell's JSON
##### By: Aakash Jajoo (aakashj1@seas) & Yathushan Nadanapathan (yathu@seas)

HSON (pronounced H-sawn) is a library enabling interaction with [JSON](https://www.json.org/json-en.html) in Haskell. HSON is a data type that was created to mimic JSON objects. HSONSchema is a data type to provide constraints on HSON attributes. In order to validate JSON objects, [JSON Schema](https://json-schema.org/) is commonly used in industry. FromJSONSchema allows the conversion of JSON Schema to HSONSchema.

## Primary Usages
  - [x] Parse a JSON file into an HSON instance using [`FromJSON`](src/FromJSON.hs). 
  - [x] Translate an HSON instance to JSON using [`ToJSON`](src/ToJSON.hs).
  - [x] Parse a JSON Schema into an HSONSchema using [`FromJSONSchema`](src/FromJSONSchema.hs)
  - [x] Validate an HSON instances with an HSONSchema instance using [`ValidateHSON`](src/ValidateHSON.hs)

## Module Organization

#### [`app`](/app/)
* [`Main.hs`](app/Main.hs): an executable that allows the user to validate a JSON object with JSON schema

#### [`src`](/src/)
* [`HSON.hs`](src/HSON.hs): declaration of the HSON data type along with its arbitrary instance
* [`ToJSON.hs`](src/ToJSON.hs): allows for an HSON instance to be translated to a .json file
* [`Parser.hs`](src/Parser.hs): a small, applicative-based parsing library developed in U.Penn's CIS-5520
* [`FromJSON.hs`](src/FromJSON.hs): allows for a .json file to be translated to an HSON instance
* [`HSONSchema.hs`](src/HSONSchema.hs): declaration of the HSONSchema data type
* [`FromJSONSchema.hs`](src/FromJSONSchema.hs): allows for JSON Schema in a .json file to be translated to an HSONSchema instance
* [`ValidateHSON.hs`](src/ValidateHSON.hs): validates an HSON instance with a given HSONSchema

#### [`test`](/test/)
* [`Spec.hs`](test/Spec.hs): an executable that runs all the tests (HUnit and QuickCheck)
* [`HSONTest.hs`](test/HSONTest.hs): a QuickCheck round-trip property for `ToJSON` and `FromJSON`
* [`ToJSONTest.hs`](test/ToJSONTest.hs): HUnit tests for `ToJSON.hs` (references [`test/json`](test/json/))
* [`FromJSONTest.hs`](test/FromJSONTest.hs): HUnit tests for `FromJSON.hs` (references [`test/json`](test/json/))
* [`FromJSONSchemaTest.hs`](test/FromJSONSchemaTest.hs): HUnit tests for `FromJSONSchema` (references [`test/json-schema/schema`](test/json-schema/schema/))
* [`ValidateHSONTest.hs`](test/ValidateHSONTest.hs): HUnit tests for `ValidateHSONTest.hs` (references [`test/json-schema`](test/json-schema/))

Haskell packages typically divide their source code into three separate places:
  - The bulk of your code should be developed as a reusable library in 
    modules in the `src` directory. We've created [Lib.hs](src/Lib.hs) 
    for you to get started. You can add additional modules here.
  
  - All of your test cases should be in [the test directory](test/Spec.hs).

## Building, Running, and Testing

This project compiles with `stack build`. 

You can run the main executable with `stack run`.

You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.

## Importing Additional Libraries

This project is designed to run with stackage: you can easily use any library
in https://www.stackage.org/lts-19.19 by adding an entry to the
`build-depends` list of the `common-stanza` in the cabal file. If you want to
use a library that is not on stackage, you'll need to update the common-stanza
*and* add information to `stack.yaml` about where to find that library.

