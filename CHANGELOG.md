# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
