# Agent notes — zero-bounce-pascal

## Layout

- `src/zbutility.pas`: the HTTP layer, the base URLs and the response mock. `ZBGetRequest` / `ZBPostRequest` are the originals; `ZBSendRequest` takes an explicit method and an optional body, which the form-encoded bulk jobs and the Inbox Placement Tester's PUT and DELETE need. `ZBFormPostRequest` wraps it.
- `src/zbvalidation.pas`: single and batch validation, plus `TZbValidateOptions`.
- `src/zbbulk.pas`: the multipart bulk uploads, their status checks and `getfile`.
- `src/zbpriority3.pas`: real-time AI scoring, the filters, the list evaluator, the S3 / SFTP bulk jobs and the Inbox Placement Tester.
- `src/zbwarmup.pas`: the Warmup API. A separate product on its own host with its own team-scoped key and an `API-Key` header, so it has its own `ZbWarmupInitialize` and never touches `ZbApiKey`.
- `src/zbstructures.pas`: the records and their JSON parsers.
- `tests/testpriority3.pas`, `tests/testbulkjobs.pas`, `tests/testwarmup.pas`: the offline suites for the above. Every request is answered by `ZbMockResponse`.
- `live/live_api.lpr`: the opt-in live suite. Its own program, outside `tests/`, so the offline suite never builds or runs it.

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

The live suite is a separate program and is never built by the command above:

```sh
docker run --rm -u 0 -v "$PWD":/src -w /src \
  -e ZEROBOUNCE_API_KEY -e ZEROBOUNCE_WARMUP_API_KEY \
  -e ZEROBOUNCE_LIVE_SPEND=1 -e ZEROBOUNCE_LIVE_CHANGES=1 \
  zerobounce-sdk-pascal:test \
  sh -c "fpc -Fu./src -Mobjfpc -O2 live/live_api.lpr -FE/tmp && /tmp/live_api"
```

Keys come from the environment only. Never write one into a file, a commit, a log, a fixture or a message.

The `.o` and `.ppu` files at the repo root are build artifacts, not sources.

## Rules

- **Only endpoints documented at https://www.zerobounce.net/docs/.** Do not add a function for an endpoint the public docs do not describe, even if it exists on the server. Check before concluding something is undocumented: the S3 and SFTP bulk endpoints live under https://www.zerobounce.net/docs/api-integrations, not the main API reference.
- **Four base URLs.** `ZbApiBaseURL`, `BULK_URI` (ends in `/v2`), `ZbBulkRootURL` (the S3/SFTP jobs, **no** `/v2`), and `ZbListEvaluatorURL` / `ZbInboxPlacementURL`. The last three are separate values rather than paths built from `BULK_URI`, so overriding the main one alone leaves a test pointing at the live API. `TTestPriority3.SetUp` resets them.
- **`EncodeParam` is query encoding.** It can turn a space into `+`, which is correct in a query string but a literal plus in a path. Use `EncodePathSegment` for anything that goes into a path, such as the list evaluator file id and the Inbox Placement test id.
- **The live API wins over the docs.** Where they disagree, follow the API and say so in a comment at the call site, prefixed `LIVE:`. The ones found so far: `validatebatch` takes `timeout` as a JSON *string*; the Inbox Placement Tester authenticates with `Authorization: Bearer` and lists tests from `GET /tests` (plural); every S3/SFTP job requires both `file_name` and `return_url`, `validateSFTPfile` included, although the docs call its `file_name` optional; `send-evaluated-file` takes the file id as a form field; Warmup's `check-domain-dmarc` takes an *email address*, not a domain; and `test-smtp-configuration` answers a failed SMTP login with HTTP 500.
- **A `"success": false` field on an HTTP 200 is a failure.** `getfile` and `deletefile` report an unknown or deleted file id that way. `ZbSuccessMeansFailure` catches it in `ZbFinishResponse`, and `ZbException.StatusCode` carries the HTTP status.
- **`ZbExportSFTPFile` sends `export_path`**, not the `export_bucket` that `ZbExportS3File` sends.
- **Safety rails in `ZbUtility`.** `ZbCheckedApiKey` rejects a key with surrounding whitespace or control characters, `ZbRequireHttpsUrl` allows https (and loopback, for a local stub) only, `ZbPathSegment` rejects `.` and `..`, redirects are never followed, and `RedactSecrets` keeps the key out of exception messages. Use them rather than adding a second rail beside them.
- **Optional record fields use a `Has...` flag.** Pascal records have no nullable scalars, so every optional option carries a companion boolean and is only sent when that flag is set.
- **Offline tests only.** Never call the live API from a test under `tests/`. `ZbMockResponse` short-circuits every request function; `ZbLastRequestMethod`, `ZbLastRequestBody` and `ZbLastRequestHeader` record what would have been sent.
- **Never from the live suite:** creating an Inbox Placement test (it cannot be deleted while running), setting the Inbox Placement webhook (it cannot be cleared), or ZB managed Warmup onboarding (it provisions a mailbox). Everything the `ZEROBOUNCE_LIVE_CHANGES=1` tier creates is removed again in the same run.

## Releasing

Before tagging a release, run the live suite by hand with real keys and both flags set, and paste its summary line into the release notes. The offline suite cannot catch an API that has changed shape underneath the SDK.
