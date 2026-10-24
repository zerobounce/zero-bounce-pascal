# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `TZbValidateOptions` accepts `DomainInfo`, `GreylistProcessing` and `CallbackUrl`. A `-1` field is not sent; booleans go on the wire as the strings `true`/`false`. `CallbackUrl` is where the greylist retry posts its result, so it is rejected without `GreylistProcessing = 1`, and when blank, before any request is sent.
- `TZbValidationResult` carries `DomainWebsiteExists`, `DomainRegistrantCompanyName` and `Message`. `Message` is the API's `Message` key, returned instead of a result while a greylist retry is still running.
- The rest of the documented bulk surface: `ZbEmailFinderS3File`, `ZbDomainSearchS3File`, and the SFTP/FTP jobs `ZbValidateSFTPFile`, `ZbScoreSFTPFile`, `ZbEmailFinderSFTPFile`, `ZbDomainSearchSFTPFile` and `ZbExportSFTPFile`, sharing the new `TZbBulkFileJobOptions`. `ZbValidateSFTPFile`'s `FileName` is optional - leaving it empty uses the input bucket from your ZeroBounce settings - and `ZbExportSFTPFile` takes `export_path`, not the `export_bucket` `ZbExportS3File` sends.
- The Inbox Placement Tester: an `Authorization: Bearer` key, camelCase JSON bodies, and unset fields left out.
- **ZbWarmup**: a client for the [Warmup API](https://www.zerobounce.net/docs/warmup-api), a separate product with its own host, its own team-scoped key (`ZbWarmupInitialize`, sent in an `API-Key` header) and 26 endpoints. Every function returns the raw JSON body, or an empty string for the documented empty successes (HTTP 204 and an empty 200); `ZbWarmupDownloadSeeds` and `ZbWarmupDownloadStatistics` return CSV text and save it as well when given a path.
- **ZbUtility**: `ZbListEvaluatorURL` and `ZbInboxPlacementURL` (separate base URLs rather than paths built from `BULK_URI`), `ZBSendRequest`, `ZBSendRequestWithHeader`, `ZBSendForeignKeyRequest`, `EncodePathSegment`, `ZbJsonEscape`, and `ZbLastRequestMethod` / `ZbLastRequestBody` / `ZbLastRequestHeader` for the offline tests.
- **ZbUtility** safety rails: `ZbCheckedApiKey` rejects a key with surrounding whitespace or control characters, `ZbRequireHttpsUrl` allows only `https://` (and loopback, so an offline test can use a local stub), `ZbPathSegment` rejects a `.` or `..` id, redirects are never followed, and `RedactSecrets` keeps the key out of exception messages. A regional base URL paired with the single global bulk host warns once.
- `live/live_api.lpr`, an opt-in live suite. It is its own program, outside `tests/`, so the offline suite never builds or runs it. Keys come from `ZEROBOUNCE_API_KEY` and `ZEROBOUNCE_WARMUP_API_KEY`; `ZEROBOUNCE_LIVE_SPEND=1` and `ZEROBOUNCE_LIVE_CHANGES=1` opt in to calls that spend credits or make reversible account changes.

### Changed

- Verified against the live API. Where it disagrees with the docs, the API wins and the call site says so:
  - `validatebatch` takes `timeout` as a JSON **string**. Sent as a number the API rejects the whole batch with the account-level "Invalid API Key or your account ran out of credits".
  - The Inbox Placement Tester authenticates with `Authorization: Bearer`, not the `api_key` query parameter the rest of the API takes, and its listing is `GET /tests` (plural) - `/test` answers `Method "GET" not allowed`.
  - Every S3/SFTP job requires both `file_name` and `return_url`, `ZbValidateSFTPFile` included, although the docs call its `file_name` optional. Both are checked before the request is sent.
  - `ZbSendEvaluatedFile` takes the file id as a form field rather than a path segment.
- A `"success": false` field on an HTTP 200 now raises instead of being returned as a result. `getfile` and `deletefile` report an unknown or deleted file id that way, and `ZbException.StatusCode` carries the HTTP status.
- `ZbGetListEvaluatorResult` uses `ZbListEvaluatorURL`, encodes the file id as a single path segment, and rejects a blank id before a request is sent.
- `ZbExportS3File`'s `ExportBucket` is optional, matching the documented endpoint: an empty string uses the export bucket from your ZeroBounce settings. A whitespace-only value is still rejected.

### Fixed

- The api-usage response now reads `sub_status_leading_period_removed` and `sub_status_alias_address`, two counters the API returns that were silently discarded.

- A validation response without `processed_at` no longer raises. A greylist retry that is still running answers with `Message` and no date; `ProcessedAt` is left at zero instead.


### Added

- `ZbValidateEmail` and `ZbBatchValidateEmails` take `TZbValidateOptions`: `Timeout`, `ActivityData`, `VerifyPlus`, and `UsePost` (form-encoded `POST /validate`).
- `TZbValidationResult` reads `ActiveInDays` and `ActiveFirstSeen`.
- `ZbScoreEmail` (`GET /v2/scoring`) and `ZbScoreEmailPost` (form-encoded `POST /v2/scoring`).
- Bulk email finder (`ZbSendEmailFinderFile`, `ZbEmailFinderFileStatus`, `ZbEmailFinderResultFetch`, `ZbEmailFinderResultDelete`).
- Bulk domain search (`ZbSendDomainSearchFile`, `ZbDomainSearchFileStatus`, `ZbDomainSearchResultFetch`, `ZbDomainSearchResultDelete`).
- List evaluator (`ZbSendListEvaluatorFile`, `ZbGetListEvaluatorResult`, `ZbSendEvaluatedFile`).
- Filters (`ZbAddFilter`, `ZbListFilters`, `ZbDeleteFilter`).
- S3 bulk (`ZbValidateS3File`, `ZbScoreS3File`, `ZbExportS3File`, `ZbListS3Files`).

### Changed

- API requests time out after 120 seconds.
- Query values, including the API key and file id, are percent-encoded.
- `ZbApiKey` and `ZbApiBaseURL` are per thread. Call `ZBInitialize` on each thread that makes requests.

### Fixed

- A 403 HTML "Access denied" page is raised as a short error. An HTML page with status 200 is an error as well, including bulk `getfile`, instead of file content.
- Empty and non-JSON bodies are a short error. A repeated `Error:` / `[Error]:` prefix is removed.
- `ZbBatchValidateEmails` raises when the batch has an account-level error (HTTP 200, error for address `all`, such as an invalid API key or no credits). An error for one address stays on the returned batch.
- Bulk email finder and domain search send `has_header_row` when it is set to false.
- `ZbBatchValidateEmails` sends an empty IP address as JSON `null`.
- Requests are not sent when `ZBInitialize` was given an empty API key.

### Security

- Query parameters are percent-encoded, so a value cannot rewrite the request URL.
- Exception messages no longer include the raw HTML access-denied page. The response body remains on `ZbException.Payload`.

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

[Unreleased]: https://github.com/zerobounce/zero-bounce-pascal/compare/v2.1.3...HEAD
[2.1.2]: https://github.com/zerobounce/zero-bounce-pascal/compare/v2.1.0...v2.1.2
[2.1.0]: https://github.com/zerobounce/zero-bounce-pascal/compare/v2.0.11...v2.1.0
[2.0.11]: https://github.com/zerobounce/zero-bounce-pascal/compare/v2.0.0...v2.0.11
[2.0.0]: https://github.com/zerobounce/zero-bounce-pascal/compare/v1.1.0...v2.0.0
[1.1.0]: https://github.com/zerobounce/zero-bounce-pascal/releases/tag/v1.1.0
