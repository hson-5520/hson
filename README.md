# HSON: Haskell's JSON
##### By: Aakash Jajoo (aakashj1@seas) & Yathushan Nadanapathan (yathu@seas)

HSON (pronounced H-sawn) is a library enabling interaction with [JSON](https://www.json.org/json-en.html) in Haskell. HSON is a data type that was created to mimic JSON objects. HSONSchema is a data type to provide constraints on HSON attributes. In order to validate JSON objects, [JSON Schema](https://json-schema.org/) is commonly used in industry. FromJSONSchema allows the conversion of JSON Schema to HSONSchema.

# Table of Contents
1. [Primary Usages](#primary-usages)
2. [Module Organization](#module-organization)
3. [Overview](#brief-overview)
4. [Documentation](#documentation)
5. [Building, Running, and Testing](#building-running-and-testing)
6. [Dependencies](#dependencies)

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
* [`Spec.hs`](test/Spec.hs): test directory that runs all the tests (HUnit and QuickCheck)
* [`HSONTest.hs`](test/HSONTest.hs): a QuickCheck round-trip property for `ToJSON` and `FromJSON`
* [`ToJSONTest.hs`](test/ToJSONTest.hs): HUnit tests for `ToJSON.hs` (references [`test/json`](test/json/))
* [`FromJSONTest.hs`](test/FromJSONTest.hs): HUnit tests for `FromJSON.hs` (references [`test/json`](test/json/))
* [`FromJSONSchemaTest.hs`](test/FromJSONSchemaTest.hs): HUnit tests for `FromJSONSchema` (references [`test/json-schema/schema`](test/json-schema/schema/))
* [`ValidateHSONTest.hs`](test/ValidateHSONTest.hs): HUnit tests for `ValidateHSONTest.hs` (references [`test/json-schema`](test/json-schema/))

## Overview

To understand this library, we suggest you visit the files in the following order:

`HSON.hs` contains the HSON data type declaration. HSON is a list of (key, value) tuples. The keys are strings (without quotes) and the values are either integers, numbers, strings, booleans, arrays (of values), null, or objects (type HSON).

`ToJSON.hs` allows for an HSON instance to be translated to a .json file. The main function is `toJSON` generates a .json file reflecting the given HSON instance at the designated filepath. 

`FromJSON.hs` parses a .json file into an HSON instance. The main function is `parseJSON` that returns either an error or HSON instance from the given .json file's filepath. The error provided is extremely primative - either there was some parse error or the parse succeeded.

`HSONSchema.hs` contains the HSONSchema data type declaration. HSONSchema describes the properties that a given value in an HSON instance should obey. Each type of HSON value has its own properties.

`FromJSONSchema.hs` parses JSON Schema, given as a .json file, into an HSONSchema instance. The main function is `jsonSchemaToHSONSchema` that returns either a HSONSchema if it was able to be parsed successfully or a detailed error message explaining why the JSON Schema couldn't be converted.

`ValidateHSON.hs` takes an HSON instance and an HSONSchema instance and attempts to validate the HSON with the HSONSchema. The main function is `validateHSON` that returns either True if the HSON obeyed the HSONSchema or a detailed error message explaining the first key that was invalid. 

## Documentation


## Building, Running, and Testing

This project compiles with `stack build`. 

You can run the main executable with `stack run`.

You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.

## Dependencies
  - [x] regex-compat: regex validator used in [`ValidateHSON.hs`](src/ValidateHSON.hs)
