# ZeroBounce Pascal SDK – unit tests (FPC + FPCUnit console runner)
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y --no-install-recommends \
    fpc \
    fp-units-fcl \
    fp-units-base \
    libssl-dev \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY . .

# Build console test runner (no LCL/GUI). Unit path: src + tests; fpcunit from fp-units-fcl
RUN fpc -Fu./src -Fu./tests -Mobjfpc -O2 -gl -viwn \
    tests/unit_tests_console.lpr -FE.

# Run all registered tests (--all); exit code 1 on failure
CMD ["./unit_tests_console", "--all"]
