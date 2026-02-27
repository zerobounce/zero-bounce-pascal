# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added

- **examples/delphi-api-v2/** — Merged content from the former **zerobounce-delphi-api-v2** repository (now decommissioned):
  - **ZeroBounceValidate.pas** — Shared unit with `UrlEncode` and `IsValidStatus` for reuse and testing (Delphi/Indy).
  - **ValidateEmailExample.dpr** — Minimal console example for single-email validation using Indy (IdHTTP, IdSSLOpenSSL) and `System.JSON`.
  - **TestValidateEmail.dpr** — Console test runner (no Indy/API key) for `UrlEncode` and `IsValidStatus`.
  - **run-tests.bat** / **run-tests.sh** — Scripts to run the test executable after building.
  - Unit tests for URL encoding (empty, alphanumeric, `@`, email, spaces, dot, hyphen).
  - Unit tests for status interpretation (valid/invalid/unknown case-insensitive, catch-all, spamtrap, sub_status).

---

## [2.0.0] - 2025-02-26 (zerobounce-delphi-api-v2, merged)

### Added (from zerobounce-delphi-api-v2)

- **ZeroBounceValidate.pas** — Shared unit with `UrlEncode` and `IsValidStatus` for reuse and testing.
- **TestValidateEmail.dpr** — Console test runner (no Indy/API key) for `UrlEncode` and `IsValidStatus`.
- **run-tests.bat** / **run-tests.sh** — Scripts to run the test executable after building.
- Unit tests for URL encoding (empty, alphanumeric, `@`, email, spaces, dot, hyphen).
- Unit tests for status interpretation (valid/invalid/unknown case-insensitive, catch-all, spamtrap, sub_status).

### Changed (from zerobounce-delphi-api-v2)

- **ValidateEmailExample.dpr** now uses `ZeroBounceValidate` for encoding and status logic.
- Status check is **case-insensitive** so API responses like `"invalid"` or `"Unknown"` are handled correctly.

### Fixed (from zerobounce-delphi-api-v2)

- **Resource leak** when the API response was not valid JSON: `HTTP` and `SSLHandler` are now always freed via an outer `try/finally`.

---

## [1.x] — zerobounce-delphi-api-v2 (initial)

- Single-email validation console example using Indy and Zero Bounce API v2.

---

## [1.1.0] — zero-bounce-pascal

- Full Pascal/Delphi SDK: validation, batch, bulk, AI scoring, find email, domain search, credits, API usage, activity data.
- See repository history and tags for earlier changes.
