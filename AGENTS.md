# AGENTS.md

Guidance for AI coding agents working in this repository.

## Project

`zero-bounce-pascal` is the ZeroBounce Pascal/Delphi SDK. Callers use `ZBInitialize(apiKey)` and the `Zb*` functions in `src/`. HTTP uses Free Pascal `fphttpclient` or Delphi `THTTPClient` in `src/zbutility.pas`. There is no package registry: releases are GitHub tags.

## Layout

- `src/zbvalidation.pas`: single and batch validation.
- `src/zbbulk.pas`: bulk file send, status, get, and delete, including AI scoring.
- `src/zbgeneric.pas`: credits, usage, activity, scoring, email finder, and domain search.
- `src/zbpriority3.pas`: bulk email finder, domain search, list evaluator, filters, and S3.
- `src/zbstructures.pas`: response records and JSON parsing.
- `src/zbutility.pas`: HTTP, URL encoding, and `ZbException`.
- `packages/zerobounce.lpk`: package version as attributes `Major`, `Minor`, and `Build`. There is no single version string.
- `tests/unit_tests_console.lpr`: FPCUnit runner. `tests/testoffline.pas` is the offline suite. It speaks HTTP to a stub bound to `127.0.0.1`.
- `examples/delphi-api-v2/`: a separate Indy example. It is not the SDK HTTP stack.

## Commands

Free Pascal was not on the machine used to add the offline suite. CI installs it. The default test command is:

```bash
fpc -Fu./src -Fu./tests -Mobjfpc -O2 -gl -viwn \
  tests/unit_tests_console.lpr -FE.
./unit_tests_console --all
```

That compile is the typecheck. There is no separate lint step. CI (`.github/workflows/sdk_ci.yml`) runs the same build and `./unit_tests_console --all`. Do not commit `unit_tests_console` or `debugging`.

## Conventions

- **The API key is per thread.** `ZbApiKey` and `ZbApiBaseURL` are `threadvar`. Call `ZBInitialize` on every thread before requests. Do not share one thread across two keys.
- **Column indexes are 1-based** for optional name, gender, and IP columns. `0` means the field is left out. `ZbValidateS3File` sends `EmailAddressColumn` when `EmailAddressColumnSpecified` is true, including `0`.
- **Tri-state ints are `-1` omit, `0` false, `1` true** on `TZbValidateOptions`. Booleans that have a `Specified` flag are omitted until that flag is true. Explicit false is sent as `false`.
- **Booleans are the strings `true` and `false`.**
- **`AllowPhase2` is validation bulk send only.** `ActivityData` on get-file is validation only.
- **Batch validation uses `ZbApiBaseURL`**, which defaults to `https://api.zerobounce.net/v2`, not the bulk host. An empty IP address is JSON `null`.
- **Failures raise `ZbException`.** `Message` is short: no raw HTML, no megabyte body, and no `Error: Error:` prefix. `Payload` still holds the response body. An account-level batch error (`EmailAddress` `all`) raises. A per-address error stays on `TZBBatchValidation.Errors`.
- **Offline tests retarget `ZbApiBaseURL`, `BULK_URI`, and `ZbBulkRootURL` at `http://127.0.0.1`.** Do not call the live API from the default test run.
- Match the surrounding style. Do not reformat unrelated code.

## Changelog

`CHANGELOG.md` follows [Keep a Changelog 1.1.0](https://keepachangelog.com/en/1.1.0/), and the project adheres to [Semantic Versioning 2.0.0](https://semver.org/spec/v2.0.0.html).

- Add every user-visible change to `## [Unreleased]` in the same commit as the change.
- Group entries under these headings, in this order, omitting empty ones: `Added`, `Changed`, `Deprecated`, `Removed`, `Fixed`, `Security`.
- Write for SDK users. Leave out CI-only changes and fixes to code that was never released.
- At 2.y.z, a breaking change bumps the major version, a new function or option bumps the minor version, and fixes alone bump the patch version.

## Releasing

Publishing and pushing tags need a maintainer's explicit approval. Do not bump the version, tag, or publish unless they asked.

1. Move `[Unreleased]` entries under `## [x.y.z] - YYYY-MM-DD`, leave an empty `[Unreleased]` section, and update the compare links.
2. Set the same version on `<Version Major="x" Minor="y" Build="z"/>` in `packages/zerobounce.lpk`. Those three attributes are the version. There is no runtime version constant.
3. Build and run `./unit_tests_console --all` with the command above.
4. Tag `vx.y.z` on that commit and run the Publish workflow. It creates the GitHub release. There is no package registry.
