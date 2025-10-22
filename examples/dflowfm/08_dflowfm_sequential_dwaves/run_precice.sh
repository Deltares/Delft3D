#!/bin/bash

RUN_DATE=$(date "+%Y%m%d_%H%M%S")
REPO_ROOT=$(cd ../../..; pwd)
PATH=${REPO_ROOT}/install_all/bin/:$PATH


cd dflowfm
dflowfm f34.mdu > ../dflowfm_${RUN_DATE}_out.log 2>&1 &
cd ..
echo "[PROCESS] $(ps -a | grep dflowfm | head -1)"
cd dwaves
wave f34.mdw 1 > ../wave_${RUN_DATE}_out.log 2>&1 &
cd ..
echo "[PROCESS] $(ps -a | grep wave | head -1)"

while [ "$(ps -a | grep wave | head -1)" != "" -a "$(ps -a | grep dflowfm | head -1)" != "" ]
do
    echo -n "."
    sleep 1
done
echo -e "\nDone"
