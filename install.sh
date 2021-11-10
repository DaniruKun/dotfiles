#!/usr/bin/env bash
set -euo pipefail

echo "Copying zsh files into home dir"

cp ./{.zprofile,.zshrc,.zshenv} $HOME/
