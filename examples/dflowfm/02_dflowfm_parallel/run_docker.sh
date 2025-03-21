mount_cmd="type=bind,source=$(pwd),target=/mnt/example"
image="containers.deltares.nl/delft3d/legacy/delft3dfm"
work_dir="/mnt/example"
example_script="./run_example.sh"

docker run --rm --mount $mount_cmd --workdir $work_dir $image $example_script
