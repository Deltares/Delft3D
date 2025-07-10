mkdir ./example && cp -r examples/dflowfm/01_dflowfm_sequential/* ./example
rm ./example/run.* ./example/run_docker.sh
cp -f ci/teamcity/Delft3D/linux/docker/readme.txt .
cp -f ci/teamcity/Delft3D/linux/docker/delft3dfm_latest_readme.txt .