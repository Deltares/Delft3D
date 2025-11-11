#!/bin/bash

RUN_DATE=$(date "+%Y%m%d_%H%M%S")
#REPO_ROOT=$(cd ../../..; pwd)
#PATH=${REPO_ROOT}/install_all/bin/:$PATH

pid_of() { ps aux | grep "$1" | grep -v grep | head -1 | tr -s ' ' | cut -f 2 -d ' '; }

if [ "$1" == "-k" ]
then
    while [ "$(pid_of dflowfm)" != "" ]
    do
        kill -9 $(pid_of dflowfm)
    done
    while [ "$(pid_of wave)" != "" ]
    do
        kill -9 $(pid_of wave)
    done
    shift
fi

if [ "$1" == "-c" ]
then
    rm -rf precice-run
    rm -rf dwaves/precice-profiling
    rm -rf dflowfm/precice-profiling
    shift
fi

cd dflowfm
dflowfm f34.mdu > ../dflowfm_${RUN_DATE}_out.log 2>&1 &
ps aux | grep dflowfm | head -1
cd ..
echo "[PROCESS] $(pid_of dflowfm)"
cd dwaves
wave f34.mdw 1 > ../wave_${RUN_DATE}_out.log 2>&1 &
ps aux | grep wave | head -1
cd ..
echo "[PROCESS] $(pid_of wave)"

while [ "$(pid_of wave)" != "" -a "$(pid_of dflowfm)" != "" ]
do
    echo -n "."
    sleep 1
done
sleep 5

while [ "$(pid_of dflowfm)" != "" ]
do
    kill -9 $(pid_of dflowfm)
done
while [ "$(pid_of wave)" != "" ]
do
    kill -9 $(pid_of wave)
done

echo -e "\nDone"
