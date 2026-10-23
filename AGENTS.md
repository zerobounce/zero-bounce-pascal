# Agent notes — zero-bounce-pascal

## Layout

- `src/zbutility.pas`: the HTTP layer, the base URLs and the response mock. `ZBGetRequest` / `ZBPostRequest` are the originals; `ZBSendRequest` takes an explicit method and an optional body, which the form-encoded bulk jobs and the Inbox Placement Tester's PUT and DELETE need. `ZBFormPostRequest` wraps it.
- `src/zbvalidation.pas`: single and batch validation, plus `TZbValidateOptions`.
- `src/zbbulk.pas`: the multipart bulk uploads, their status checks and `getfile`.
- `src/zbpriority3.pas`: real-time AI scoring, the filters, the list evaluator, the S3 / SFTP bulk jobs and the Inbox Placement Tester.
- `src/zbstructures.pas`: the records and their JSON parsers.
- `tests/testpriority3.pas`: the offline suite for the above. Every request is answered by `ZbMockResponse`.

## Build and test

FPC runs in Docker (there is no `fpc` on the host):

```sh
cd ../sdk-docs && docker compose run --rm --build pascal
```

To build and run against the working tree without rebuilding the image:

```sh
docker run --rm -u 0 -v "$PWD":/src -w /src zerobounce-sdk-pascal:test \
  sh -c "fpc -Fu./src -Fu./tests -Mobjfpc -O2 -gl -viwn tests/unit_tests_console.lpr -FE/tmp && /tmp/unit_tests_console --all"
```

A new test unit must be added to the `uses` clause of `tests/unit_tests_console.lpr`, or it never runs.

The `.o` and `.ppu` files at the repo root are build artifacts, not sources.

## Rules

- **Only endpoints documented at https://www.zerobounce.net/docs/.** Do not add a function for an endpoint the public docs do not describe, even if it exists on the server. Check before concluding something is undocumented: the S3 and SFTP bulk endpoints live under https://www.zerobounce.net/docs/api-integrations, not the main API reference.
- **Four base URLs.** `ZbApiBaseURL`, `BULK_URI` (ends in `/v2`), `ZbBulkRootURL` (the S3/SFTP jobs, **no** `/v2`), and `ZbListEvaluatorURL` / `ZbInboxPlacementURL`. The last three are separate values rather than paths built from `BULK_URI`, so overriding the main one alone leaves a test pointing at the live API. `TTestPriority3.SetUp` resets them.
- **`EncodeParam` is query encoding.** It can turn a space into `+`, which is correct in a query string but a literal plus in a path. Use `EncodePathSegment` for anything that goes into a path, such as the list evaluator file id and the Inbox Placement test id.
- **Two bulk shapes are easy to get wrong.** `ZbValidateSFTPFile`'s `FileName` is optional — leaving it empty uses the input bucket from the account's settings, and every other job requires it. `ZbExportSFTPFile` sends `export_path`, not the `export_bucket` that `ZbExportS3File` sends.
- **Optional record fields use a `Has...` flag.** Pascal records have no nullable scalars, so every optional option carries a companion boolean and is only sent when that flag is set.
- **Offline tests only.** Never call the live API from a test. `ZbMockResponse` short-circuits every request function; `ZbLastRequestMethod` and `ZbLastRequestBody` record what would have been sent.
