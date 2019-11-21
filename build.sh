#!/bin/bash

set -eu
cd `dirname $0`

dotnet tool restore
dotnet paket restore

if [ "X$OS" = "XWindows_NT" ] ; then
  packages/build/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
else
  mono packages/build/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
fi