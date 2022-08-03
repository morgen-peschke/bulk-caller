# bulk-caller

A CLI tool for making bulk REST calls

## Purpose

This is intended for situations where you have a number of payloads that have to be submitted via the same REST call,
you care about the results, and you don't want to do them manually.

## Use

The arguments are rather sparse, as the bulk of the configuration is specified using a HOCON config file.

```
Unexpected argument: help

Usage:
    bulk-caller --data <-|path> --config <path>
    bulk-caller --version

Make bulk HTTP calls, with tracing

Version: 0.0.1-SNAPSHOT

Options and flags:
    --help
        Display this help text.
    --data <-|path>
        Source of the data for each call.

        The expected format is JSONL, with an 'id' field containing a scalar, and a 'data' field containing a JSON
         object with a key for each name in the substitutions.
    --config <path>
        Source of HOCON config file

        The expected format is documented in src/main/resources/reference.conf
    --version
        Print the version and exit
```

The HOCON config is extensive, and [`reference.conf`(src/main/resources/reference.conf) acts as an extensively
commented guide to how to configure `bulk-caller`.