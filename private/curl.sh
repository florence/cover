#!/usr/bin/env bash

file=$1

curl -v --include --form json_file=@"$file" "https://coveralls.io/api/v1/jobs"
