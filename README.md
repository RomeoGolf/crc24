# CRC24

This program is intended to calculate and check the CRC-24 checksum used in
secondary surveillance radar system (ADS-B, aircraft transponders in MODE-S)

## Getting Started

### Prerequisites

To build this project you need to have haskell stack installed.

Install the latest version of
[Stack](https://github.com/commercialhaskell/stack); use `stack upgrade`
to confirm you are on the latest version.

### Installing

Make sure your Stack project builds without errors.
Preferably by using: stack build --test --haddock --no-haddock-hyperlink-source;

After your project is built successfully, import an existing project by:

Or you can run it by `stack exec crc24-exe [ -- <arguments>]`

For example:

```
stack exec crc24-exe -- -v
```

### Running the tests

Use `stack test` to test the project.

Use `stack test --coverage` instead to test the project
and get the textual coverage reports.

## Description

## Using

### Example

## License

This project is licensed under the MIT License - see
the [LICENSE.md](LICENSE.md) file for details
