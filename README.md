# Zero bounce API - pascal SDK

This library is a wrapper for the ZeroBounce API v2.

For more information about the API, visit https://www.zerobounce.net/docs/.

In order to run this library, the zero-bounce API which requires an API key. Check [this guide](https://www.zerobounce.net/docs/api-dashboard#API_keys_management) to see how to grab yours.

## Project's units

The methods implemented in this library can raise `ZbException`.

- ZbUtility - contains utility methods
    - ZbSetApiKey - set the API key to be used within the all SDK's methods
    - ZbException - exception containing data about the response context
- ZbGenerics - contains general purpose methods
    - ZbGetCredits (returns integer) - fetch account's credits
    - ZbGetApiUsage (returns `TApiUsage`) - fetch account's (overall or specific) API usage
    - ZbActivityData (returns integer) - fetch the amount of days an email inbox has been active
- ZbValidation - fetch validation information about emails
    - ZbValidateEmail (returns `TZbValidationResult`) - validate one email
    - ZbBatchValidateEmails (retrns `TZBBatchValidation`) - validate a list of emails
- ZbBulk - bulk email validation or AI scoring
    - Bulk Email validation:
        - ZbBulkValidationFileSubmit (returns `TZBFileFeedback`) - submit for bulk validation a file contents, containing emails
        - ZbBulkValidationFileStatusCheck (returns `TZBFileStatus`) - check the status of a submitted file
        - ZbBulkValidationResultFetch (returns `TZBBulkResponse`) - fetch the validation result of a submitted file
        - ZbBulkValidationResultDelete (returns `TZBFileFeedback`) - delete file submitted for bulk validation
    - AI scoring:
        - ZbAiScoringFileSubmit (returns `TZBFileFeedback`) - submit for AI scoring a file contents, containing emails
        - ZbAiScoringFileStatusCheck (returns `TZBFileStatus`) - check the status of a submitted file
        - ZbAiScoringResultFetch (returns `TZBBulkResponse`) - fetch the validation result of a submitted file
        - ZbAiScoringResultDelete (returns `TZBFileFeedback`) - delete file submitted for AI scoring
- ZbStructures - contains structures returned by the methods enumerated above


## Local environment

### Delphi

In order to run this project, one must install [Delphi IDE](https://www.embarcadero.com/products/delphi/starter/free-download).

Open Delphi IDE, File > Open, browse for `./packages/ZeroBounce.dpk` (or `./packages/zb.groupproj`), click "Open package".

In order to run example snippets or your own project using the zero-bounce package, first install the package into Delphi:
- open package (described above)
- locate projects window (or CTRL+ALT+F11)
- right click on the project ("ZeroBounce.bpl") > Compile
- right click on the project > Install

After installing, the example projects (`./examples/delphi/Examples.groupproj`) should run successfully. If that still doesn't happen, follow the steps below.

To run the ZeroBounce SDK with any other Dephi project:
- open desired project
- Project > Options (or SHIFT+CTRL+F11) > Packages > Runtime Packages
- enable "Link with runtime packages"
- [optional] click on the three dots from "Runtime packages", browse to `C:\Users\Public\Documents\Embarcadero\Studio\{INSTALLED VERSION}\Bpl`, select "Any file (*.*)" from bottom-right, select `ZeroBounce.bpl` file
- you can now import project's units

### Free Pascal
In order to run this project, one must install [Lazarus IDE](https://www.lazarus-ide.org/).

Open Lazarus IDE, File > Open, browse for `./packages/zerobounce.lpk` (relative to repository root), click "Open package".

In order to run unit tests, example snippets or your own project with the zero-bounce package, first install it into lazarus:
- open package (described above)
- locate package window (titled with "Package ZeroBounce {version}")
- Compile
- Use > Install

After installing, opening either tests  (`tests/unit_tests.lpi`) or example snippets (any `*.lpi` file from `examples/fpc/` folder) should work running.

To run ZeroBounce SDK with any other FPC project:
- open desired project
- Project > Project inspector > Add > New requirement
- pick "ZeroBounce" from the list
- it should now appear in "Required Packages"
- you can now import project's units
