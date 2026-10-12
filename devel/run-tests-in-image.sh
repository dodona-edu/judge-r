#!/usr/bin/env bash
# Runs devel/run-tests.sh as root inside the judge image, like CI. Arguments are passed on.
set -euo pipefail

repo="$(cd "$(dirname "$0")/.." && pwd)"

docker run --rm --platform linux/amd64 --user root --env JUDGE_R_IMAGE=1 \
  ${UPDATE_EXPECTED:+--env UPDATE_EXPECTED} \
  --volume "$repo:/judge-r" --workdir /judge-r \
  "${IMAGE:-ghcr.io/dodona-edu/dodona-r:latest}" devel/run-tests.sh "$@"
