# Zero bounce API - pascal SDK


## Requirements

- **Free Pascal 3.2** with the FCL units (`fp-units-fcl`, `fp-units-base`). CI and the test image build with FPC 3.2.2; earlier releases are not tested.
- **FPC builds:** the OpenSSL shared libraries (CI installs `libssl-dev`) and trusted CA certificates, since TLS is verified - see [TLS certificates](#tls-certificates-free-pascal-builds) below.
- **Delphi:** a version whose `System.Net.HttpClient` `THTTPClient` has `ConnectionTimeout`, `ResponseTimeout` and `HandleRedirects`. Delphi builds use the operating system's TLS stack. They are not compiled in CI, so no minimum Delphi version has been verified.

## Security

- Keep API keys on a trusted server. Do not embed them in mobile apps or browser JavaScript that untrusted users can inspect.
- Custom API base URLs (when supported) must use `https://`. Do not pass end-user-controlled hosts into those settings.
- Request URLs include `api_key` as a query parameter (ZeroBounce API contract). Do not log full request URLs or enable payload debug logging in production.
- Redirects are never followed: a 3xx answer raises a `ZbException` carrying its status. Every request has a connect and a read timeout, `ZbConnectTimeoutMs` (default 30000) and `ZbReadTimeoutMs` (default 120000), in milliseconds.
- Trailing slashes on any base URL variable (`ZbApiBaseURL`, `BULK_URI`, `ZbBulkRootURL`, `ZbListEvaluatorURL`, `ZbInboxPlacementURL`, `ZbWarmupBaseURL`) are dropped when a request is built, so no request path contains `//`.

This library is a wrapper for the ZeroBounce API v2.

For more information about the API, visit https://www.zerobounce.net/docs/.

This library calls the ZeroBounce API, which requires an API key. Check [this guide](https://www.zerobounce.net/docs/api-dashboard/keys-management) to see how to grab yours.

The API key is per thread: call `ZBInitialize` on every thread that makes requests.

## TLS certificates (Free Pascal builds)

Since 2.2.0, FPC builds verify the server's TLS certificate and host name, so they need trusted CA certificates. **Breaking** for 2.1.3 users: 2.1.3 did not verify at all. The SDK looks, in order, at:

1. `ZbCACertFile` (unit `ZbUtility`), the full path of a PEM CA bundle;
2. the `SSL_CERT_FILE` environment variable;
3. on Linux, macOS and other Unix systems, the usual bundle paths (`/etc/ssl/certs/ca-certificates.crt`, `/etc/pki/tls/certs/ca-bundle.crt`, `/etc/ssl/ca-bundle.pem`, `/etc/ssl/cert.pem`);
4. `cacert.pem` beside the executable;
5. on Windows, the Windows "ROOT" system certificate store.

If none of them yields a certificate, every request raises a `ZbException` that says so; a `ZbCACertFile` or `SSL_CERT_FILE` naming a file that does not exist raises as well. Verification is never switched off. On Windows, with nothing configured, the SDK uses the certificates in the Windows "ROOT" store. If requests still raise (for example on a machine whose store lacks the root that ZeroBounce's certificate chains to), download a PEM bundle such as https://curl.se/ca/cacert.pem and either save it as `cacert.pem` next to your `.exe`, or point the SDK at it before the first request:

```pascal
uses ZbUtility;

begin
    ZbCACertFile := 'C:\MyApp\cacert.pem';
    ZBInitialize('YOUR__API__KEY');
end.
```

Keep the bundle up to date. Delphi builds use the operating system's TLS stack and ignore `ZbCACertFile`.


## Package's units

The methods implemented in this library can raise `ZbException`.

- __ZbUtility__ - contains utility methods
    - `ZBInitialize` - set the API key (and API base URL) to be used within the all SDK's methods
        ```pascal
            // ApiURL can be ZbApiURLDefault, ZbApiURLUSA, ZbApiURLEU
            ZBInitialize('YOUR__API__KEY', ZbApiURLDefault);

            // or an explicit base URL, for a region the enum does not name
            // or for a local stub. https only - plain http only when the
            // host is exactly 127.0.0.1, localhost or [::1] - and a
            // trailing slash is dropped.
            ZBInitialize('YOUR__API__KEY', 'https://api-au.zerobounce.net/v2');
        ```
    - `ZbException` - exception containing data about the response context
- __ZbGeneric__ - contains general purpose methods
    - `ZbGetCredits` (returns integer) - fetch account's credits
    - `ZbGetApiUsage` (returns `TApiUsage`) - fetch account's (overall or specific) API usage
    - `ZbActivityData` (returns integer) - fetch the amount of days an email inbox has been active
    - `ZbFindEmailByDomain` (returns `TZbFindEmailResponse`) - identifies and validates a person’s primary email address via domain
    - `ZbFindEmailByCompanyName` (returns `TZbFindEmailResponse`) - identifies and validates a person’s primary email address via company name
    - `ZbDomainSearchByDomain` (returns `TZbDomainSearchResponse`) - detects possible email address patterns used by a domain
    - `ZbDomainSearchByCompanyName` (returns `TZbDomainSearchResponse`) - detects possible email address patterns used by a company
    - `ZbScoreEmail` / `ZbScoreEmailPost` (return `TZbScoreEmailResult`) - real-time AI scoring over GET or POST
- __ZbValidation__ - fetch validation information about emails
    - `ZbValidateEmail` (returns `TZbValidationResult`) - validate one email
    - `ZbBatchValidateEmails` (retrns `TZBBatchValidation`) - validate a list of emails
- __ZbBulk__ - bulk email validation or AI scoring (bulk base URL: `https://bulkapi.zerobounce.net/v2`; see [v2 send file](https://www.zerobounce.net/docs/email-validation-api-quickstart/v2-send-file), [v2 file status](https://www.zerobounce.net/docs/email-validation-api-quickstart/v2-file-status), [v2 get file](https://www.zerobounce.net/docs/email-validation-api-quickstart/v2-get-file))
    - `TZbBulkParams` - column indexes and flags for file submit; set `AllowPhase2Specified` and `AllowPhase2` to send `allow_phase_2` on **validation** sendfile only (not AI scoring).
    - `TZbGetFileOptions` / `PTZbGetFileOptions` - optional `download_type` (`DownloadType`) and validation-only `activity_data` (`ActivityDataSpecified` + `ActivityData`); pass `@Options` as the second argument to result-fetch functions, or `nil` for defaults.
    - Constants in __ZbUtility__: `ZB_DOWNLOAD_TYPE_PHASE_1`, `ZB_DOWNLOAD_TYPE_PHASE_2`, `ZB_DOWNLOAD_TYPE_COMBINED`.
    - Bulk Email validation:
        - `ZbBulkValidationFileSubmit` (returns `TZBFileFeedback`) - submit for bulk validation a file contents, containing emails
        - `ZbBulkValidationFileStatusCheck` (returns `TZBFileStatus`) - check the status of a submitted file
        - `ZbBulkValidationResultFetch` (returns `TZBBulkResponse`) - fetch the validation result; optional `Options: PTZbGetFileOptions = nil`
        - `ZbBulkValidationResultDelete` (returns `TZBFileFeedback`) - delete file submitted for bulk validation
    - AI scoring:
        - `ZbAiScoringFileSubmit` (returns `TZBFileFeedback`) - submit for AI scoring a file contents, containing emails
        - `ZbAiScoringFileStatusCheck` (returns `TZBFileStatus`) - check the status of a submitted file
        - `ZbAiScoringResultFetch` (returns `TZBBulkResponse`) - fetch the scoring result; optional `Options: PTZbGetFileOptions = nil` (`activity_data` is not sent for scoring getfile)
        - `ZbAiScoringResultDelete` (returns `TZBFileFeedback`) - delete file submitted for AI scoring
    - For getfile (here and in the email finder / domain search result fetches), a JSON body - error data, or a message such as `{"message": "File is still processing"}`, with any content type and on any HTTP status, 200 included - raises a `ZbException` whose `StatusCode` is the HTTP status. Otherwise `HasContent` is true and `Content` holds the file text.
- __ZbPriority3__ - bulk email finder and domain search, filters, the list evaluator, the S3 / SFTP bulk jobs and the Inbox Placement Tester
    - Bulk email finder and domain search, on the bulk root (`https://bulkapi.zerobounce.net`, **no** `/v2`): `ZbSendEmailFinderFile`, `ZbEmailFinderFileStatus`, `ZbEmailFinderResultFetch`, `ZbEmailFinderResultDelete`, and the matching `ZbSendDomainSearchFile` / `ZbDomainSearch...` functions. `ZbSendEmailFinderFile` needs `DomainColumn` **and** one of `FirstNameColumn` / `FullNameColumn`; with neither name column the job has nothing to build a local part from, so it is refused before the upload
    - List evaluator: `ZbSendListEvaluatorFile`, `ZbGetListEvaluatorResult` and `ZbSendEvaluatedFile(FileId)`, all three built from their own base URL, `ZbListEvaluatorURL` (`send-evaluated-file/` sits beside it). `ZbSendEvaluatedFile` sends the file id as a **form field**, not as a path segment
    - Filters: `ZbAddFilter(Rule, Target, Value)`, `ZbListFilters(UsePost)`, `ZbDeleteFilter(Rule, Target, Value)`. `Rule` is `allow` or `block`; `Target` is `email`, `domain`, `mx` or `tld`. Both are checked before the request is sent.
    - S3 / SFTP jobs: `ZbListS3Files`, `ZbValidateS3File`, `ZbScoreS3File`, `ZbExportS3File`, `ZbEmailFinderS3File`, `ZbDomainSearchS3File`, `ZbValidateSFTPFile`, `ZbScoreSFTPFile`, `ZbEmailFinderSFTPFile`, `ZbDomainSearchSFTPFile`, `ZbExportSFTPFile`. `TZbBulkFileJobOptions` requires **both** `FileName` and `ReturnUrl` — the live API rejects a job without either, `validateSFTPfile` included, although the docs call its `file_name` optional. `ZbExportSFTPFile` sends `export_path`, not the `export_bucket` that `ZbExportS3File` sends. `TZbValidateS3FileOptions` and `TZbScoreS3FileOptions` require `FileName` and `ReturnUrl` for the same reason; they keep their own records because they also carry `EmailAddressColumn` and, for validate, the name, gender and IP columns.
    - Inbox Placement Tester (its own host, `ZbInboxPlacementURL`): `ZbInboxPlacementProviders`, `ZbInboxPlacementTestTypes`, `ZbCreateInboxPlacementTest`, `ZbInboxPlacementTest`, `ZbListInboxPlacementTests`, `ZbDeleteInboxPlacementTest`, `ZbInboxPlacementWebhook`, `ZbSetInboxPlacementWebhook`. It authenticates with an `Authorization: Bearer` header, takes camelCase JSON bodies, and lists tests from `GET /tests`. These calls spend the account's inbox-test credits. Each returns the raw JSON body; `ZbDeleteInboxPlacementTest` returns an empty string for the API's empty answer to a delete, and an empty 2xx from any other of these calls raises a `ZbException` with the HTTP status.
        - `ZbCreateInboxPlacementTest(Name, Options)` - `Name` is capped at 64 characters, `TestType` is 1, 2 or 3, and type 1 (the one you send yourself) needs a `SenderEmail`. Pick the mailboxes with `ProviderIds`, `SeedIds` or `Providers` (an array of `{ Id, MaxSeeds }`); an empty array is left out of the body and the account default applies
        - `ZbListInboxPlacementTests(Options)` - `Status` is `running` or `finished`, `PageSize` is 1-100, and `CreatedAfter` / `CreatedBefore` filter by time. The page size goes out as **`page_size`**: spelled `pageSize` the API accepts it and then silently ignores it, so the filter looks like it worked until you count the rows
- __ZbWarmup__ - the [Warmup API](https://www.zerobounce.net/docs/warmup-api), a separate product with its own host and its own team-scoped key
    - `ZbWarmupInitialize('YOUR__WARMUP__API__KEY')` - the key goes out in an `API-Key` header. It is **not** the validation key, and `ZBInitialize` stays for that one.
    - Every function returns the raw JSON body, or an empty string for the documented empty successes (HTTP 204 and an empty 200).
    - Sending accounts: `ZbWarmupListSendingAccounts`, `ZbWarmupGetSendingAccount`, `ZbWarmupAddSendingAccount`, `ZbWarmupUpdateSendingAccount`, `ZbWarmupDeleteSendingAccount`, `ZbWarmupChangeFromAddress`, `ZbWarmupListFromAddresses`
    - Engagement rules: `ZbWarmupGetEngagementRules`, `ZbWarmupSetEngagementRule`, `ZbWarmupGetCustomEngagement`, `ZbWarmupCreateCustomEngagement`, `ZbWarmupUpdateCustomEngagement`, `ZbWarmupDeleteCustomEngagement`. A standard rule is 0 to 4; a custom rule takes `reply_pct` 5-45, `important_pct` 5-40 and `click_pct` 0-70 and at most `important_pct` x 1.75, all checked before sending.
    - Seeds, progress and statistics: `ZbWarmupGetSeeds`, `ZbWarmupDownloadSeeds`, `ZbWarmupGetSeedCount`, `ZbWarmupGetProgressToday`, `ZbWarmupGetSentToday`, `ZbWarmupGetEmailsPerDay`, `ZbWarmupGetStatistics`, `ZbWarmupGetStatisticsPerDay`, `ZbWarmupGetStatisticsDetailed`, `ZbWarmupDownloadStatistics`, `ZbWarmupGetFromAddressStatistics`. The two download functions return CSV text and save it as well when given a path.
    - Deliverability: `ZbWarmupCheckDomainDmarc(FromAddress[, DkimSelector])` takes an **email address**, not a bare domain, which is what the live API requires; `ZbWarmupTestSmtpConfiguration` stores nothing and answers a failed SMTP login with HTTP 500.
- __ZbStructures__ - contains structures returned by the methods enumerated above (`TZbFileStatus` includes `FilePhase2Status` / `file_phase_2_status` when the API returns it)


## Usage

### Delphi

In order to run this package, one must install [Delphi IDE](https://www.embarcadero.com/products/delphi/starter/free-download).

Open Delphi IDE, File > Open, browse for `./packages/ZeroBounce.dpk` (or `./packages/zb.groupproj`), click "Open package".

**Minimal Delphi example (no package):** For a self-contained single-email validation example using Delphi's built-in `System.Net.HttpClient` (Delphi XE8 or later, no OpenSSL DLLs), see **`examples/delphi-api-v2/`**. Build `ValidateEmailExample.dpr` there; no SDK package install required.

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
In order to run this package, one must install [Lazarus IDE](https://www.lazarus-ide.org/).

Open Lazarus IDE, File > Open, browse for `./packages/zerobounce.lpk` (relative to repository root), click "Open package".

In order to run unit tests, example snippets or your own project with the zero-bounce package, first install it into lazarus:
- open package (described above)
- locate package window (titled with "Package ZeroBounce {version}")
- Compile
- Use > Install

After installing, opening either tests  (`./tests/unit_tests.lpi`) or example snippets (any `*.lpi` file from `./examples/fpc/` folder) should work running.

To run ZeroBounce SDK with any other FPC project:
- open desired project
- Project > Project inspector > Add > New requirement
- pick "ZeroBounce" from the list
- it should now appear in "Required Packages"
- you can now import project's units

## Testing

Unit tests (FPCUnit) can be run via Docker or locally.

**Docker:** from the repository root (the image builds the console runner; running it runs the suite):
```bash
docker build -t zerobounce-pascal-sdk:test .
docker run --rm --network none zerobounce-pascal-sdk:test
```

The repository includes a `.dockerignore` to keep the image build context small.

**Local (console):** with FPC and fp-units-fcl installed, the same commands CI runs (`.github/workflows/sdk_ci.yml`): `fpc -Fu./src -Fu./tests -Mobjfpc -O2 -gl -viwn tests/unit_tests_console.lpr -FE.` then `./unit_tests_console --all`.

**Local (GUI):** open `tests/unit_tests.lpr` in Lazarus and run (Run → Run).

The offline suite never reaches the live API. `live/live_api.lpr` is a separate, opt-in live suite; see the comments at its top for the environment variables it reads.

## Publish

1. Bump version in `packages/zerobounce.lpk`, commit, tag (`vX.Y.Z`), push tag.
2. **Actions → Publish → Run workflow** with that tag.

No package registry — GitHub release only. Pin the tag via clone or submodule.

## License

MIT. See [LICENSE](LICENSE).
