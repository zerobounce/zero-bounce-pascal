# Zero bounce API - pascal SDK

This library is a wrapper for the ZeroBounce API v2.

For more information about the API, visit https://www.zerobounce.net/docs/.

In order to run this library, the zero-bounce API which requires an API key. Check [this guide](https://www.zerobounce.net/docs/api-dashboard#API_keys_management) to see how to grab yours.


## Usage

### Examples

You can check the `examples/` folder to see how can this package be implemented in your project. Projects from `examples/fpc/` can be opened with Lazarus IDE and those from `examples/delphi/` can be opened with Delphi IDE.

### Free Pascal - Lazarus

In order to run this project locally, install [Lazarus IDE](https://www.lazarus-ide.org/).

Open Lazarus IDE, File > Open, browse for `zerobounce.lpk` (found at repository root), click "Open package". Locate 'Project inspector' window and then:
- Compile
- Use > Install

You have now installed the ZeroBounce package in your Lazarus IDE. Now you can run the unit tests (`tests/unit_tests.lpi`), run the example projects (any `*.lpi` file from `examples/fpc/` folder) or use the package in your own project.

To run zero bounce with any other project:
- open desired project
- Project > Project inspector > Add > New requirement
- pick "ZeroBounce" from the list
- it should now appear in "Required Packages" and work

### Delphi

In order to run this project locally, install [Delphi IDE](https://www.embarcadero.com/products/delphi/starter/free-download).

