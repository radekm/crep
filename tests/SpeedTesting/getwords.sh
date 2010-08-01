#!/bin/sh

tr -cs A-Za-z '\n' |
  sort |
  uniq -c |
  sort -k1,1nr -k2 |
  awk 'length($2) > 3 { print $2 }' |
  sed ${1}q
