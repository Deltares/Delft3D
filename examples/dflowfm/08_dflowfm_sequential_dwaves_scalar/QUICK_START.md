# Quick Start

## With 10 Scalars (Default)

```bash
cd /workspaces/Delft3D/examples/dflowfm/08_dflowfm_sequential_dwaves_scalar

# Generate config with 10 scalars (matches hardcoded value)
python3 add_dummy_scalars.py 10

source ~/.precice_env

./run_precice.sh
```

## With Different Number

**Important**: Must update hardcoded values in source code first!

```bash
# 1. Edit FM and Wave source files to change num_dummy_scalars
# 2. Rebuild the code
# 3. Then generate matching config:

python3 add_dummy_scalars.py 50

source ~/.precice_env

./run_precice.sh
```

## Disable Dummy Scalars

```bash
python3 add_dummy_scalars.py 0

source ~/.precice_env

./run_precice.sh
```
