#!/bin/sh

./getwords.sh $1 |
  awk '{ print $0, "1 /" $0 "/" length($0) "/" }'
