#!/usr/bin/env bash

set -euo pipefail

echo "==> Checking environment..."
# apt-get update && apt-get install -y libcurl4-openssl-dev

echo "==> Restoring R package dependencies via renv..."
Rscript -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv')"
Rscript -e "renv::restore()"

echo "==> Executing main data cleaning pipeline..."
Rscript src/main.R

echo "==> Pipeline completed successfully!"
