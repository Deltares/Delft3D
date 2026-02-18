#!/bin/bash

# Call all run scripts with a path to a Dimrset-bin folder
dimrset_bin=$PWD/../build_all/lnx64/bin
root=$PWD

echo "==================================="
echo "=== SEQUENTIAL COMPUTATIONS"
echo "==================================="
find -maxdepth 6 -type d | sort -n | while read -r i
do
    cd $root/$i
    if [ -e run.sh ]; then
        echo -e "\n\n================================="
        echo "$PWD/run.sh $dimrset_bin"
        ./run.sh $dimrset_bin
    fi
done

echo ...finished
