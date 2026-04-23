#!/bin/bash

bindir=/p/d-hydro/dimrset/2026/2026.01/lnx64/bin

PATH=$bindir:$PATH
LD_LIBRARY_PATH=$bindir/../lib:$LD_LIBRARY_PATH

$bindir/dimr dimr_config.xml

