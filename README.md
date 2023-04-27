# Zero bounce API - pascal SDK


## Local environment

In order to run this project, one must install [Lazarus IDE](https://www.lazarus-ide.org/).

## Editing the package

Open Lazarus IDK, File > Open, brose for `zerobounce.lpk` (found at repository root), click "Open package".

You have now opened the SDK in package mode. Now you can:
- compile it
- open individual files from the package (double-clicking opens files in Lazarus IDE)
- extend the package (by adding other files to it)
- manage requirements
- increment version and manage package details

## Unit tests
Open `tests/unit_tests.lpi` in Lazarus IDE as a project.

In order to run the unit tests, compile and install the zero bounce library:
- open library in package mode
- Compile; Use > Install
- (re)open unit tests
- Project > Project inspector > Add > New requirement
- pick "ZeroBounce" from the list
- it should now appear in "Required Packages"

Now you can run it (Run > Run).

## To do:
- [ ] explain how to run project
- [ ] explain how to run tests
- [ ] explain how to build library
- [ ] explain how to install library
- [ ] tell about ZbException being returned by every method, in case of anything
-
