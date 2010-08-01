#!/bin/sh

./getwords.sh $1 |
  awk '{ print "s/" $0 "/" length($0) "/g" }'
