# Contributing to Zero Bounce Pascal SDK

Thank you for your interest in contributing. This document explains how to get set up and submit changes.

## Code of Conduct

By participating in this project, you agree to uphold our [Code of Conduct](CODE_OF_CONDUCT.md).

## Getting Started

See the [README](README.md) for prerequisites, setup, and how to run tests.

## How to Contribute

### Reporting Bugs

Open an [issue](https://github.com/zerobounce/zero-bounce-pascal/issues) and include:

* Environment details (compiler, OS)
* Steps to reproduce
* Expected vs actual behavior
* Relevant code or error messages

### Suggesting Changes

* Check existing issues and pull requests first.
* Open an issue to discuss larger changes or API design before coding.

### Submitting Changes

1. **Fork** the repository and create a branch from `main`:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes** and add or update tests where relevant.

3. **Run the test suite** (see README) before submitting.

4. **Commit** with a clear message (e.g. `Add X`, `Fix Y`).

5. **Push** your branch and open a **Pull Request** against `main`.

6. In the PR description, briefly explain what changed and why. Link any related issues.

Maintainers will review and may request changes. Once approved, your PR can be merged.

## Regenerating the Unicode tables

`src/zbcasefold.inc` (full case folding) and `src/zbnormalize.inc` (canonical decompositions and combining classes, for NFD) are generated, not written by hand: secret redaction in `ZbUtility` uses them. Regenerate them only when the Unicode version changes on purpose (the committed tables are Unicode 16.0.0), both together. Use a Python whose `unicodedata.unidata_version` is `16.0.0`:

```bash
python3 tools/gen_casefold.py > src/zbcasefold.inc
python3 tools/gen_normalize.py > src/zbnormalize.inc
```

Each script exits non-zero under any other Unicode version, so a different Python cannot silently change a table. To move to a newer Unicode, change `EXPECTED_UNICODE_VERSION` in both scripts, regenerate, review the diff and run the full suite. Never edit an `.inc` by hand.

## Questions

* [Zero Bounce API docs](https://www.zerobounce.net/docs/)
* [Project homepage](https://zerobounce.net)
* Contact: **integrations@zerobounce.net**

Thanks for contributing.
