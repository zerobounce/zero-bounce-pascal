# Zero bounce API - pascal SDK

This library is a wrapper for the ZeroBounce API v2.

For more information about the API, visit https://www.zerobounce.net/docs/.

In order to run this library, the zero-bounce API which requires an API key. Check [this guide](https://www.zerobounce.net/docs/api-dashboard#API_keys_management) to see how to grab yours.


## Local environment

In order to run this project, one must install [Lazarus IDE](https://www.lazarus-ide.org/).

Open Lazarus IDK, File > Open, brose for `zerobounce.lpk` (found at repository root), click "Open package".

In order to run unit tests or example snippets, first the zero-bounce package into lazarus:
- open library in package mode
- Compile
- 'Project inspector' window > Use > Install

After installing, opening either tests  (`tests/unit_tests.lpi`) or example snippets (any `*.lpi` file from `examples/` folder) should work running.

To run zero bounce with any other project:
- open desired project
- Project > Project inspector > Add > New requirement
- pick "ZeroBounce" from the list
- it should now appear in "Required Packages" and work
