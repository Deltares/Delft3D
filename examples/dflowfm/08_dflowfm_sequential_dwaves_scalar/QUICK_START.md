# Quick Start

## With 100 Scalars

```bash
cd /workspaces/Delft3D/examples/dflowfm/08_dflowfm_sequential_dwaves_scalar

python3 add_dummy_scalars.py 100

source ~/.precice_env

cp ../08_dflowfm_sequential_dwaves/run_precice.sh .

./run_precice.sh
```

## With Different Number

```bash
python3 add_dummy_scalars.py 500

source ~/.precice_env

./run_precice.sh
```

## Disable Dummy Scalars

```bash
python3 add_dummy_scalars.py 0

source ~/.precice_env

./run_precice.sh
```
