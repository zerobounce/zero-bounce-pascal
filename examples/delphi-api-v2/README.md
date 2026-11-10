# Delphi API v2 — minimal console example

This folder contains the **legacy minimal example** formerly in the standalone `zerobounce-delphi-api-v2` repository, merged into zero-bounce-pascal for a single place to maintain all Pascal/Delphi SDK code.

**v2.0.0** — Console example for validating a single email with the [Zero Bounce API v2](https://www.zerobounce.net/docs/), using Delphi's built-in `System.Net.HttpClient` and `System.JSON` — no forms, no VCL UI. Since v2.2.0 it no longer uses Indy, so no OpenSSL DLLs are needed: HTTPS goes through the operating system's TLS stack.

## When to use this

- You want a **minimal, self-contained** single-email validation example (no dependency on the full ZeroBounce package).
- For the **full SDK** (all endpoints, batch, bulk file, scoring, find email, domain search), use the main library: see the [root README](../../README.md) and `examples/delphi/` or `examples/fpc/`.

## Runnable example

Open and build **`ValidateEmailExample.dpr`** in this folder. Requirements:

- Delphi XE8 or later (`System.Net.HttpClient` first shipped in XE8). Free Pascal is not supported by this example: it has no `System.Net.HttpClient`. For Free Pascal, use the SDK and `examples/fpc/`.
- No Indy and no OpenSSL DLLs

### Run

```text
ValidateEmailExample.exe <email_to_validate> [api_key]
```

If `api_key` is omitted, the program uses the `ZEROBOUNCE_API_KEY` environment variable.

**Examples:**

```bash
# API key as second argument
ValidateEmailExample.exe user@example.com your_api_key_here

# API key from environment
set ZEROBOUNCE_API_KEY=your_api_key_here
ValidateEmailExample.exe user@example.com
```

Output: `Valid: <status>` or `Invalid: <status>` (and optional `sub_status`).

## Unit tests

Build and run **`TestValidateEmail.dpr`** (no network access or API key required). It tests:

- **UrlEncode** — empty, alphanumeric, `@`, full email, spaces, dots, hyphens
- **IsValidStatus** — `valid` / `invalid` / `unknown` (case-insensitive), catch-all, spamtrap, status with sub_status

```text
TestValidateEmail.exe
```

Result: `N passed, 0 failed` with exit code 0 on success. Use **run-tests.bat** (Windows) or **run-tests.sh** (Unix) after building.

## Project layout

| File | Purpose |
|------|--------|
| `ValidateEmailExample.dpr` | Main console app (API validation) |
| `ZeroBounceValidate.pas` | Shared helpers: `UrlEncode`, `IsValidStatus` |
| `TestValidateEmail.dpr` | Console test runner for the helpers |
| `run-tests.bat` / `run-tests.sh` | Scripts to run the test executable after building |
