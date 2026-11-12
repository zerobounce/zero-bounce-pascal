# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed

- A chunked download cut mid-stream came back as the whole result: fphttpclient stops reading a chunked body when the connection closes and reports nothing, so `ZbBulkValidationResultFetch` returned the 34 bytes that had arrived as the file. Every request now notes when the server closed the connection, and a chunked body that ends that way, before its last (empty) chunk, raises `ZbException` ("cut short") with status 0, on every transport and download (Warmup CSV included).
- `ZbBulkValidationResultFetch`, `ZbAiScoringResultFetch`, `ZbEmailFinderResultFetch` and `ZbDomainSearchResultFetch` returned `HasContent = True` for an empty HTTP 200 body, and for an HTML page whose body did not look like HTML (a proxy's `<p>Access denied by proxy</p>` sent as `text/html`). Both now raise `ZbException` with the status; a `text/html` content type is checked in any letter case.
- An answer cut short was returned as the result on the JSON POST (`ZbBatchValidateEmails`), form POST (`ZbValidateEmail` with `UsePost`, `ZbScoreEmailPost`, the S3/SFTP jobs, filters, `ZbSendEvaluatedFile`) and multipart upload (`ZbBulkValidationFileSubmit`, `ZbAiScoringFileSubmit`, `ZbSendEmailFinderFile`, `ZbSendDomainSearchFile`, `ZbSendListEvaluatorFile`) requests: when the connection closed before the `Content-Length` the server promised, whatever had arrived was parsed, and if it parsed it came back as a success. With FPC these requests now raise `ZbException` (`The response from ZeroBounce was cut short: N of M bytes received`, `StatusCode` 0), as GET requests and the Warmup and Inbox Placement Tester requests have since 3.0.1. The Delphi branch is unchanged.
- An upload whose connection was refused raised `ZbException` with an uninitialised `StatusCode` (any number, for example `-599851432`). It is now 0, like every other error with no HTTP answer.
- A request that got no HTTP answer (connection refused or timed out, a TLS or protocol failure) raised fphttpclient's own exception instead of the `ZbException` the README tells callers to catch: `ESocketError` (for example `Connect to 127.0.0.1:1 failed.`) or `EHTTPClient`, with no `StatusCode`, from every GET request (`ZbGetCredits`, `ZbValidateEmail`, the file status, result and delete calls, the email finder and domain search, the list evaluator and more), the JSON POST (`ZbBatchValidateEmails`), the form POSTs (`ZbValidateEmail` with `UsePost`, `ZbScoreEmailPost`, the S3/SFTP jobs, the filters, `ZbSendEvaluatedFile`) and `ZBSendRequest` / `ZBSendRequestWithHeader` / `ZBSendForeignKeyRequest` (the Inbox Placement Tester). They now raise `ZbException` with `StatusCode` 0 and the original message, with the API key masked as `REDACTED`, as uploads and the Warmup API already did. The Delphi branch gets the same wrapping (its multipart upload too), but it is not compiled in CI.
- **Behaviour change:** the Inbox Placement Tester calls returned an empty string as the result for an empty 2xx body, whatever the call. Only `ZbDeleteInboxPlacementTest` answers with an empty body, so it alone still does; an empty 2xx from `ZbInboxPlacementProviders`, `ZbInboxPlacementTestTypes`, `ZbCreateInboxPlacementTest`, `ZbInboxPlacementTest`, `ZbListInboxPlacementTests`, `ZbInboxPlacementWebhook` or `ZbSetInboxPlacementWebhook` now raises a `ZbException` (`Http Error: Empty response from ZeroBounce (HTTP 200)`) with `StatusCode` set to the HTTP status.
- `ZbWarmupDownloadSeeds` and `ZbWarmupDownloadStatistics` returned (and saved to `DownloadPath`) an answer that is not a CSV file when its content type did not say JSON: a JSON object or array under `text/csv` (`{"message": "..."}`), or a `text/html` page. They now raise `ZbException` with the status for an empty body, a JSON or HTML content type, or a body that is a JSON object or array, and save nothing.
- `ZbWarmupDownloadSeeds` and `ZbWarmupDownloadStatistics` with a `DownloadPath` opened the path with `fmCreate` and wrote into it, so a write that failed part-way (a full disk, a file size limit) raised but destroyed a file that was already there and left the partial one in its place. They now write to a temporary file next to `DownloadPath` (`<path>.<pid>-<n>.part`) and move it into place only when the whole file is written; a failed write removes the temporary file and raises as before, and the earlier file is kept unchanged.
- An HTTP 200 labelled `text/html` whose body parsed as JSON (a proxy's or a captive portal's page, `{"a":1}`) came back as the result of every JSON call (validation, scoring, the email finder and domain search, the bulk status calls, the filters, the S3/SFTP jobs, the Inbox Placement Tester, every Warmup JSON call); only an HTML-looking body raised. A `text/html` content type (in any letter case) now raises `ZbException` with the HTTP status on every request.
- The raw-JSON calls (`ZbAddFilter`, `ZbListFilters`, `ZbDeleteFilter`, `ZbGetListEvaluatorResult`, `ZbListS3Files`, the Inbox Placement Tester calls) and every Warmup JSON call returned an HTTP 200 body that is not JSON as their result: a proxy's `hello` as `text/plain`, `not json` or a CSV labelled `application/json`. They now raise a `ZbException` with `StatusCode` 200 (`Json Error: ...`), as the parsed calls already did. An empty 200 is still the documented empty success of the Warmup calls and of `ZbDeleteInboxPlacementTest`; from the other raw-JSON calls it now raises with its status too.
- A JSON body followed by other text on an HTTP 200 (`{"a":1} <- proxy note`) was read as its leading JSON value, because fpjson's `GetJSON` stops after the first value: every JSON call returned it as the result. Every JSON call now raises a `ZbException` with the status for a body that is anything but one JSON object or array (whitespace around it allowed).

### Added

- `TZbWarmupManagedSending.TimezoneOffsetHours` (`Double`) and `TimezoneOffsetHoursSpecified`, added at the end of the record: hours from UTC with fractional zones such as 5.5 (India) or 5.75 (Nepal). Sent as `timezone_offset` when `TimezoneOffsetHoursSpecified` is set, in place of `TimezoneOffset` if both are set; a whole number is sent as an integer (`"timezone_offset":3`), and a value outside -11.0 to 14.0, NaN or an infinity raises before anything is sent. Like the other `...Specified` flags it must start out `False`: zero the record (`FillChar`, `Default`) before filling it, as the 3.0.x fields already required.
- Offline tests: code written for 3.0.x keeps compiling with `TZbWarmupManagedSending` (an `Integer` and a `PInteger` from `TimezoneOffset`, a record constant without the new fields) and sends the same body; every public call answering 200 with a non-JSON body (`application/json`, `text/plain`, `text/csv`), a JSON value with text after it, or a cut JSON object; every spelling of a secret composed from raw, JSON-escaped (with and without `\/`) and URL-encoded (`%20` and `+`) forms, plus JSON-escaped twice, generated in the test rather than listed by hand.
- Offline tests: a pre-existing file at `DownloadPath` survives a Warmup CSV download whose write fails (a file size limit), with no partial file left beside it; a sweep over every public call answering 200 `text/html` with `{"a":1}`, and every download answering 200 `application/json` with a CSV body; every encoded form of an SMTP password containing a space, `"`, `/`, `+`, `%`, `&`, `=`, `<`, `>`, `'` and `\`, each needed by a test.
- Offline tests: the default timeouts (30 s to connect, 120 s to read) read back from the `TFPHTTPClient` every transport builds (`ZbLastRequestTimeouts`, FPC only), so changing a default fails a test.
- Offline tests for guards that had none: a control character inside the key (not only at its ends), `RedactSecrets` on the raw and url-encoded key, the key kept out of API error messages, Warmup errors keeping the Warmup key and the SMTP username and password out of `Message` and `Payload` (including fphttpclient's own exceptions), `"success": "false"` as a string, `ZBInitialize` leaving the key and base URL unchanged when it raises, and the email finder and domain search getfile calls raising on a JSON content type or a JSON object body.
- The plain-http sweep now runs every public request function in a shared list, overloads and the ZbUtility transports included, and a test reads the interfaces of ZbUtility, ZbGeneric, ZbValidation, ZbBulk, ZbPriority3 and ZbWarmup and fails when a public request function is missing from that list. The same list checks that a refused connection raises `ZbException` everywhere.
- `ZbRegisterSecretRedactor` in __ZbUtility__: adds a function that `RedactSecrets`, and so every `ZbException`, runs over its `Message` and `Payload`. ZbWarmup registers one for the Warmup key and the SMTP credentials of the call in progress.
- Offline tests: the key sweep also answers every call with a 200 JSON body made only of `message` or `detail` quoting both keys (a Warmup CSV download must raise on it, without either key); every documented `add_sending_account` field in one exact request body; fractional `TimezoneOffsetHours` values (5.5, 3.0 and 5.75 accepted; -11.5, 14.5, NaN and the infinities rejected); the CSV downloads rejecting a JSON body or content type, an HTML page by body or content type, and an empty body.

### Security

- The API key, the Warmup key and the SMTP username and password of `ZbWarmupTestSmtpConfiguration` were masked raw (and the keys URL-encoded) only, so an answer that quoted them JSON-escaped, as a server or proxy echoing the request body would (`Pa\/ss\"w%rd`), or the SMTP credentials URL-encoded, left them readable in `ZbException.Message` and `Payload`. Every secret is now masked raw, URL-encoded (a space as `%20` or `+`, and as `EncodeParam` puts it on a query string), JSON-escaped (`/` escaped or not), JSON-escaped twice (an echoed body quoted inside another JSON string, `Pa\\\/ss\\\"w%rd`) and URL-encoded after JSON escaping (`Pa%5C%2Fss%5C%22w%25rd`, as a server putting an echoed JSON body into a URL or form would), through the new `ZbMaskSecret`.
- `ZbException.Payload` held the response body unredacted: a body quoting the API key, such as `{"error": "Invalid API key <key>"}` or `{"success": false, "message": "... <key>"}` on an HTTP 200, or a 4xx quoting the request URL with `api_key=<key>`, handed the key to any caller that read `Payload`, on every call. Only `Message` was masked. Every `ZbException` now has the key, raw and url-encoded, replaced by `REDACTED` in both `Message` and `Payload`, whatever built it (an API error body, an account-level `ZbBatchValidateEmails` error, a transport error). The Warmup functions also mask the Warmup key in its url-encoded form, not only raw.
- A Warmup CSV download that got a JSON answer quoting the Warmup key, such as `{"message": "Invalid API-Key <key>"}` on an HTTP 200, raised a `ZbException` with the key in both `Message` and `Payload`: the error was raised after the request, outside the only place that masked the Warmup key, and `ZbException` itself masked only the validation key. Every `ZbException` now masks the Warmup key (raw and url-encoded) and the SMTP username and password of `ZbWarmupTestSmtpConfiguration` in `Message` and `Payload`, whatever raised it.

### Changed

- **Behaviour change:** bulk and scoring `sendfile` always sent `remove_duplicate` from `TZbBulkParams.RemoveDuplicate`, so a zero-filled record (as in `examples/fpc/ExampleBulkValidation.lpr`) sent `false` and turned off the API's default de-duplication. The new `TZbBulkParams.RemoveDuplicateSpecified` (last in the record) decides whether it is sent; left `False`, nothing is sent and the API's default (true) applies. Code that set `RemoveDuplicate := False` must now also set `RemoveDuplicateSpecified := True`. Record constants written for the old layout still compile, with a "Some fields ... were not initialized" warning.

### Deprecated

- `TZbWarmupManagedSending.TimezoneOffset` (`Integer`, whole hours only): use `TimezoneOffsetHours` with `TimezoneOffsetHoursSpecified`. It keeps its type and behaviour and is ignored when `TimezoneOffsetHoursSpecified` is set.

## [3.0.1] - 2026-11-11

### Fixed

- A download cut short after its HTTP 200 header was returned as the whole file. When the connection closed part-way through the body (for example after 26 of the 1000 bytes its `Content-Length` promised), fphttpclient handed back what had arrived, and `ZbBulkValidationResultFetch`, `ZbAiScoringResultFetch`, `ZbEmailFinderResultFetch` and `ZbDomainSearchResultFetch` returned it with `HasContent = True`; `ZbWarmupDownloadSeeds` and `ZbWarmupDownloadStatistics` returned and saved it. With FPC they now raise `ZbException` (`The response from ZeroBounce was cut short: 26 of 1000 bytes received`) and save nothing. This covers every GET request and the Warmup and Inbox Placement Tester requests. A body that stalls past `ZbReadTimeoutMs` part-way already raised. The Delphi branch is unchanged.

### Added

- An offline test that every transport applies `ZbConnectTimeoutMs`. It is skipped where the unrouted test address (192.0.2.1) is refused at once, as in a container without a network.

## [3.0.0] - 2026-11-12

### Breaking

- `ZbBulkValidationResultFetch`, `ZbAiScoringResultFetch`, `ZbEmailFinderResultFetch` and `ZbDomainSearchResultFetch` now raise a `ZbException` carrying the HTTP status (`StatusCode`) when getfile answers with a JSON body - a JSON content type, or a body that is a JSON object, on any status. In 2.2.0 a body such as `{"message": "File is still processing"}` on an HTTP 200 came back as a result with `HasContent = False` and the text in `Feedback.Message`; the other SDKs raise. A returned `TZBBulkResponse` now always has `HasContent = True`. Upgrading: replace `if not R.HasContent then ... R.Feedback.Message` with an `except on E: ZbException` around the call.
- The deprecated `ZBSetApiKey` now checks the key like `ZBInitialize`: a key with surrounding whitespace or control characters raises `ZbException` (`api_key has surrounding whitespace or control characters`) and the key already set stays. In 2.2.0 it stored any key unchecked.

### Added

- `ZbConnectTimeoutMs` (default 30000) and `ZbReadTimeoutMs` (default 120000) in __ZbUtility__: the connect and read timeouts, in milliseconds, of every request. They were fixed at the same values before.
- `ZbBaseUrl` in __ZbUtility__: a base URL with its trailing slashes dropped.
- Offline tests that every transport stops at a redirect with the status kept, applies the read timeout, and that every public request function refuses a plain `http://` base URL.

### Fixed

- A trailing slash on `ZbApiBaseURL`, `BULK_URI`, `ZbBulkRootURL`, `ZbInboxPlacementURL` or `ZbWarmupBaseURL` built a request path with `//` (for example `https://warmup-api.zerobounce.net//api/v1/warmup/seeds`). Trailing slashes are now dropped from every base URL when a request is built. `ZbListEvaluatorURL` and the `ZBInitialize(ApiKey, BaseUrl)` overload already dropped them.
- FPC builds: a GET answered with a redirect, or with a status outside a fixed list (200, 201, 204, 400, 401, 403, 404, 409, 422, 429, 500, 502, 503), raised fphttpclient's own `EHTTPClient` (`Unexpected response status code: 302`) with no status, instead of the `ZbException` with `StatusCode` that every other request raises. Every status now reaches the SDK's response checks. Redirects were not followed before either.

## [2.2.0] - 2026-11-11

### Upgrading from 2.1.3

Read the **Breaking** entries under Changed before upgrading. In short: call `ZBInitialize` on every thread that makes requests; FPC builds need a CA bundle (or, on Windows, a readable system root store) because TLS is now verified; `BULK_URI` is a variable, not a constant; and `ZbException.Message` no longer carries the URL, status code and payload.

### Added

- `ZBInitialize(ApiKey, BaseUrl: String)`, an overload taking an explicit Main API base URL. The enum names three regions; anything else - another region, or a local stub - meant assigning `ZbApiBaseURL` by hand. The URL goes through the same check as every request URL (see the safety rails below), and a trailing slash is dropped.
- `TZbValidateOptions` and the `ZbValidateEmail` / `ZbBatchValidateEmails` overloads that take it. A `-1` field is not sent; booleans go on the wire as `true`/`false`.
  - `ZbValidateEmail` honours every field: `Timeout` (3-60 s), `ActivityData`, `VerifyPlus`, `DomainInfo`, `GreylistProcessing`, `CallbackUrl` and `UsePost` (form-encoded `POST /validate`). `CallbackUrl` is where the greylist retry posts its result, so it is rejected, before any request is sent, without `GreylistProcessing = 1` and when blank.
  - `ZbBatchValidateEmails` sends only `Timeout` (10-120 s; LIVE: as a JSON **string** - sent as a number the API rejects the whole batch with the account-level "Invalid API Key or your account ran out of credits"), `ActivityData` and `VerifyPlus`. It ignores `DomainInfo`, `GreylistProcessing`, `CallbackUrl` and `UsePost`: none of them is sent or checked, and the batch is always a JSON `POST /validatebatch`.
- `TZbValidationResult` reads `ActiveInDays`, `ActiveFirstSeen`, `DomainWebsiteExists`, `DomainRegistrantCompanyName` and `Message`. `Message` is the API's `Message` key, returned instead of a result while a greylist retry is still running.
- `ZbScoreEmail` (`GET /v2/scoring`) and `ZbScoreEmailPost` (form-encoded `POST /v2/scoring`).
- **ZbPriority3**, a new unit:
  - Bulk email finder (`ZbSendEmailFinderFile`, `ZbEmailFinderFileStatus`, `ZbEmailFinderResultFetch`, `ZbEmailFinderResultDelete`) and bulk domain search (`ZbSendDomainSearchFile`, `ZbDomainSearchFileStatus`, `ZbDomainSearchResultFetch`, `ZbDomainSearchResultDelete`). `has_header_row` is sent whenever it is set, `false` included. `ZbSendEmailFinderFile` requires `DomainColumn` and one of `FirstNameColumn` / `FullNameColumn`, the two name columns the docs make one of required.
  - List evaluator (`ZbSendListEvaluatorFile`, `ZbGetListEvaluatorResult`, `ZbSendEvaluatedFile`). All three are built from `ZbListEvaluatorURL`: the upload is `ZbListEvaluatorURL/`, the result `ZbListEvaluatorURL/{file_id}/`, and `send-evaluated-file/` sits beside it (by default `https://bulkapi.zerobounce.net/v2/send-evaluated-file/`), so overriding that one variable moves the whole flow. The result's file id is encoded as a single path segment and a blank id is rejected before a request is sent. LIVE: `ZbSendEvaluatedFile` takes the file id as a form field rather than the file upload the docs describe.
  - Filters (`ZbAddFilter`, `ZbListFilters`, `ZbDeleteFilter`).
  - S3 bulk (`ZbValidateS3File`, `ZbScoreS3File`, `ZbExportS3File`, `ZbListS3Files`, `ZbEmailFinderS3File`, `ZbDomainSearchS3File`) and the SFTP/FTP jobs (`ZbValidateSFTPFile`, `ZbScoreSFTPFile`, `ZbEmailFinderSFTPFile`, `ZbDomainSearchSFTPFile`, `ZbExportSFTPFile`), the last six sharing `TZbBulkFileJobOptions`. LIVE: every one of these jobs requires both `file_name` and `return_url` - `ZbValidateSFTPFile` included, although the docs call its `file_name` optional - so `FileName` and `ReturnUrl` are checked before the request is sent; without a return URL the API answers HTTP 400 "Provided return URL is not permitted: None". `ZbExportS3File`'s `ExportBucket` and `ZbExportSFTPFile`'s `ExportPath` are optional (empty uses the one from your ZeroBounce settings); `ZbExportSFTPFile` sends `export_path`, not `export_bucket`.
  - The Inbox Placement Tester (`ZbInboxPlacementProviders`, `ZbInboxPlacementTestTypes`, `ZbCreateInboxPlacementTest`, `ZbInboxPlacementTest`, `ZbListInboxPlacementTests`, `ZbDeleteInboxPlacementTest`, `ZbInboxPlacementWebhook`, `ZbSetInboxPlacementWebhook`), with camelCase JSON bodies and unset fields left out. LIVE: it authenticates with `Authorization: Bearer`, not the `api_key` query parameter, and its listing is `GET /tests` (plural); `/test` answers `Method "GET" not allowed`. `TZbCreateInboxPlacementTestOptions` carries `ProviderIds`, `SeedIds` and `Providers` (`TZbInboxPlacementProvider`: an `Id` and an optional `MaxSeeds`); an empty array is left out and the account default applies. `ZbCreateInboxPlacementTest` checks `Name` (at most 64 characters), `TestType` (1-3) and a `SenderEmail` for type 1 before spending an inbox-test credit. `TZbListInboxPlacementTestsOptions` carries `Status`, `CreatedAfter`, `CreatedBefore`, `Page` and `PageSize`; `Status` (`running` / `finished`) and `PageSize` (1-100) are checked before the call. LIVE: the page size is sent as `page_size`; spelled `pageSize` the API silently ignores it.
- **ZbWarmup**: a client for the [Warmup API](https://www.zerobounce.net/docs/warmup-api), a separate product with its own host, its own team-scoped key (`ZbWarmupInitialize`, sent in an `API-Key` header) and 26 endpoints. Every function returns the raw JSON body, or an empty string for the documented empty successes (HTTP 204 and an empty 200); `ZbWarmupDownloadSeeds` and `ZbWarmupDownloadStatistics` return CSV text and save it as well when given a path.
- **ZbUtility**: the base URL variables `ZbBulkRootURL`, `ZbListEvaluatorURL` and `ZbInboxPlacementURL` (alongside `BULK_URI`, now a variable - see Changed) and the `BULK_HOST` constant; `ZBFormPostRequest`, `ZBSendRequest`, `ZBSendRequestWithHeader`, `ZBSendForeignKeyRequest`, `EncodePathSegment`, `ZbJsonEscape`; the response checks `ZbBodyLooksLikeHtml`, `ZbSuccessMeansFailure` and `ZbMessageFromBody`; and `ZbLastRequestMethod` / `ZbLastRequestBody` / `ZbLastRequestHeader` for the offline tests.
- **ZbUtility** safety rails: `ZbCheckedApiKey` rejects a key with surrounding whitespace or control characters; `ZbRequireHttpsUrl` parses the URL's scheme, user information, host and port and allows only `https://`, or plain `http://` when the host is exactly `127.0.0.1`, `localhost` or `[::1]` (any port and path) so an offline test can use a local stub - a URL with user information (`user@host`), or with `?`, `#` or `\` before the path, is refused; `ZbPathSegment` rejects a `.` or `..` id; `ZbCheckedUrl` applies the URL check to every request; redirects are never followed; and `RedactSecrets` keeps the key out of exception messages. A regional base URL paired with the single global bulk host warns once on stderr.
- **ZbUtility**, FPC only: `ZbCACertFile`, the CA bundle used to verify TLS (see the CA-bundle entry under Changed), and `ZbWindowsRootCertificates` / `ZbTrustDerCertificates` / `TZbDerCertificates`, which load the Windows "ROOT" system store into an OpenSSL connection.
- `live/live_api.lpr`, an opt-in live suite. It is its own program, outside `tests/`, so the offline suite never builds or runs it. Keys come from `ZEROBOUNCE_API_KEY` and `ZEROBOUNCE_WARMUP_API_KEY`; `ZEROBOUNCE_LIVE_SPEND=1` and `ZEROBOUNCE_LIVE_CHANGES=1` opt in to calls that spend credits or make reversible account changes.

### Changed

- **Breaking:** `ZbApiKey` and `ZbApiBaseURL` are per thread (`threadvar`). In 2.1.3 they were global, so an app that calls `ZBInitialize` on the main thread and makes requests from worker threads now gets `ZeroBounce is not initialized. Please call ZBInitialize(api_key) first` on those threads. Call `ZBInitialize` on each thread that makes requests.
- **Breaking:** `BULK_URI` is now a typed variable instead of a constant, so tests can point it at a local stub. Code that uses it in a constant expression (`const MyUrl = BULK_URI + '/sendfile';`) no longer compiles; use a variable or the new `BULK_HOST` constant (`'bulkapi.zerobounce.net'`).
- **Breaking:** FPC builds now verify the server's TLS certificate and host name (2.1.3 did not verify at all), which needs trusted CA certificates. They come from `ZbCACertFile`, else the `SSL_CERT_FILE` environment variable, else (not on Windows) the usual Unix bundle paths (`/etc/ssl/certs/ca-certificates.crt`, `/etc/pki/tls/certs/ca-bundle.crt`, `/etc/ssl/ca-bundle.pem`, `/etc/ssl/cert.pem`), else `cacert.pem` beside the executable, else - on Windows only - the Windows "ROOT" system certificate store. When none of these yields a certificate, every request raises a `ZbException` saying how to provide one, and a `ZbCACertFile` or `SSL_CERT_FILE` that names a missing file raises rather than falling back. Verification is never switched off. To supply a bundle yourself, download one (for example https://curl.se/ca/cacert.pem) and either save it as `cacert.pem` beside the executable or set `ZbCACertFile := 'C:\path\to\cacert.pem';` before the first request. Delphi builds use the operating system's TLS stack and are unaffected.
- `ZbException.Message` is now just the error, with the key redacted. In 2.1.3 it was the message followed by `Url:`, the URL (key included), `Status code:` and `Payload:` blocks; read `ZbException.StatusCode` and `ZbException.Payload` instead, which are unchanged. On an HTTP error the message is the API's own (a JSON error body as sent, or a non-JSON body with a repeated `Error:` / `[Error]:` prefix removed), prefixed with `Http Error: `, for GETs, JSON and form POSTs alike. FPC builds raise a `ZbException` carrying the status for every HTTP error; in 2.1.3 a GET with any status other than 200 raised fphttpclient's `EHTTPClient` ("Unexpected response status code") instead.
- `ZBInitialize` raises a `ZbException` when the key has surrounding whitespace or control characters, instead of sending it.
- **Breaking** for code that checked the returned record: a `"success": false` field on an HTTP 200 now raises instead of being returned as a result, with `ZbException.StatusCode` 200. `getfile` and `deletefile` report an unknown or deleted file id that way. This applies to every request path - GET, JSON POST, multipart upload and form POST.
- A redirect (HTTP 3xx) raises `ZbException` ("ZeroBounce returned a redirect, which this client does not follow.") instead of being followed, so the key cannot be sent on to another host. This is new for Delphi, whose `THTTPClient` followed redirects in 2.1.3; FPC builds already raised on one.
- API requests have a 30-second connect timeout and a 120-second read timeout (FPC: `ConnectTimeout` / `IOTimeout`; Delphi: `ConnectionTimeout` / `ResponseTimeout`). 2.1.3 set none.
- Query values, including the API key and email address, are percent-encoded.
- The standalone example `examples/delphi-api-v2/ValidateEmailExample.dpr` uses Delphi's `System.Net.HttpClient` instead of Indy (`IdHTTP`/`IdSSLOpenSSL`), so it needs Delphi XE8 or later but no OpenSSL DLLs, and it no longer builds with Free Pascal. Its README and the root README now say so. The SDK itself is unaffected.

### Fixed

- An invalid or drained API key now raises a `ZbException` carrying the API's message, with `StatusCode` 200. The API answers one with an HTTP 200 and no `success` field: `{"error": "Invalid API key or your account ran out of credits"}` from validate and activity, `{"error": "Invalid API key"}` from getapiusage, and `{"Message": ...}` from the email finder and domain search. In 2.1.3 `ZbValidateEmail` and `ZbGetApiUsage` raised a misleading `Json Error: Invalid date/timestamp ...`, `ZbDomainSearchByDomain` and `ZbDomainSearchByCompanyName` raised `Json Error: Field "other_domain_formats" not found ...`, `ZbFindEmailByDomain` and `ZbFindEmailByCompanyName` returned a result with an empty email, and `ZbActivityData` returned -1. `message`, `Message` and `detail` decide failure only for the email finder and domain search, because successful responses use them too (a repeated `greylist_processing` validate answers `{"Message": "Greylist validation still in progress. Please try again later."}`, which is a result). `ZbGetCredits` still returns -1 for an invalid key, as documented.
- `ZbFindEmailByDomain` and `ZbFindEmailByCompanyName` omit `first_name` when it is empty. Both appended `&first_name=` unconditionally, so the overloads that pass `''` for the names they do not take sent a blank field. The deprecated `ZbFindEmail` had always guarded it.
- The api-usage response now reads `sub_status_leading_period_removed` and `sub_status_alias_address`, two counters the API returns that were silently discarded.
- A validation response without `processed_at` no longer raises. A greylist retry that is still running answers with `Message` and no date; `ProcessedAt` is left at zero instead.
- A 403 HTML "Access denied" page is raised as a short error. An HTML page with status 200 is an error as well, including bulk `getfile`, instead of file content.
- An HTTP error status (400 and above) whose body is empty or not JSON raises a short message, with a repeated `Error:` / `[Error]:` prefix removed. An empty body on HTTP 200 raises `Empty response from ZeroBounce (HTTP 200)`; any other non-JSON body on HTTP 200 still raises a `Json Error` with the JSON parser's message (the body is on `Payload`).
- `ZbBatchValidateEmails` raises when the batch has an account-level error (HTTP 200, error for address `all`, such as an invalid API key or no credits). An error for one address stays on the returned batch.
- `ZbBatchValidateEmails` sends an empty IP address as JSON `null`, and escapes the email, IP and key for JSON.
- Requests are not sent when `ZBInitialize` was given an empty API key.
- The domain search examples (`examples/fpc/ExampleDomainSearch.lpr`, `examples/delphi/ExampleDomainSearch.dpr`) compile. Since 2.0.0 they have called `ZbDomainSearchByDomain('example.com', 'John', 'Doe')`, but the function takes only the domain; they now pass the domain alone and print the format and confidence the API returns. CI now compiles every FPC example.

### Security

- FPC builds verify TLS certificates and host names (see the CA-bundle entry under Changed); 2.1.3 accepted any certificate, so the `api_key` in request URLs could be read by a man in the middle.
- Plain `http://` is accepted only for an exact loopback host, and a URL with user information is refused, so neither `ZBInitialize` nor a hand-set base URL can send the key to another host over plain HTTP (`http://localhost.evil.example`, `http://127.0.0.1@evil.example`).
- Redirects are not followed, so the key is never forwarded to another host.
- Query parameters are percent-encoded, so a value cannot rewrite the request URL.
- Exception messages no longer include the request URL (which carries the key) or the raw HTML access-denied page. The response body remains on `ZbException.Payload`.

## [2.1.2] - 2026-06-10

### Added

- `TZbValidationResult.CatchallDomain`, read from `catchall_domain`.

## [2.1.0] - 2026-04-05

### Added

- Optional bulk get-file parameters: `DownloadType` (`phase_1`, `phase_2`, `combined`) and `ActivityData` (validation get-file only).
- `AllowPhase2` on validation bulk file submit.
- File status field for phase 2.

### Fixed

- Bulk get-file treats a JSON error body, including HTTP 200 with `"success": false`, as an error instead of file content.

## [2.0.11] - 2026-03-03

### Changed

- License the project under MIT.

## [2.0.0] - 2026-02-27

### Added

- `examples/delphi-api-v2`: single-email validation example and offline tests for URL encoding and status interpretation, merged from the former zerobounce-delphi-api-v2 repository.

### Changed

- The example uses shared encoding and case-insensitive status checks.

### Fixed

- The example frees the HTTP client when the response is not valid JSON.

## [1.1.0] - 2023-09-04

### Added

- Full Pascal/Delphi SDK: validation, batch, bulk, AI scoring, find email, domain search, credits, API usage, and activity data.

[Unreleased]: https://github.com/zerobounce/zero-bounce-pascal/compare/v3.0.1...HEAD
[3.0.1]: https://github.com/zerobounce/zero-bounce-pascal/compare/v3.0.0...v3.0.1
[3.0.0]: https://github.com/zerobounce/zero-bounce-pascal/compare/v2.2.0...v3.0.0
[2.2.0]: https://github.com/zerobounce/zero-bounce-pascal/compare/v2.1.3...v2.2.0
[2.1.2]: https://github.com/zerobounce/zero-bounce-pascal/compare/v2.1.0...v2.1.2
[2.1.0]: https://github.com/zerobounce/zero-bounce-pascal/compare/v2.0.11...v2.1.0
[2.0.11]: https://github.com/zerobounce/zero-bounce-pascal/compare/v2.0.0...v2.0.11
[2.0.0]: https://github.com/zerobounce/zero-bounce-pascal/compare/v1.1.0...v2.0.0
[1.1.0]: https://github.com/zerobounce/zero-bounce-pascal/releases/tag/v1.1.0
