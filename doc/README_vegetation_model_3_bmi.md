# Vegetation model 3 drag through Python BMI

Use `BaptistDragCallback` to calculate bed and stem drag in Python from the
link depth that D-Flow FM has just computed. D-Flow FM calls the registered
callback before applying vegetation drag at each computational flow step.
DIMR can continue advancing the simulation at its configured coupling interval.

## D-Flow FM setup

Use a D-Flow FM shared library that exports `dfm_set_vegetation_drag_callback`.
Enable the callback in the MDU for a 2D model (`Kmx = 0` in `[geometry]`).

```ini
[veg]
Vegetationmodelnr = 3
BaptistDragCallback = true
```

| Model 3 configuration | Source of drag coefficients |
| --- | --- |
| `BaptistDragCallback = true` | The registered BMI callback supplies coefficients each flow step. |
| `BaptistDragCallback = false` or omitted | The caller supplies coefficients through the existing BMI setters on its own schedule. |

Use `Vegetationmodelnr = 2` to calculate drag in D-Flow FM from plant properties
and the current link depth. Supply the plant properties through the model
inputs or BMI. Configure the MDU with the scalar `Cdveg` or set
`Cdvegspatial = 1` and supply `Cdvegsp` through BMI.

Provide the vegetation density field `rnveg` through the model inputs or BMI.
D-Flow FM uses the mean density of the cells at each link and the MDU setting
`Densvegminbap` to select links where model 3 applies. Supply the diameter,
height and drag coefficient fields required by your Python formula. If the
case also uses SWAN vegetation, configure and populate its plant fields as
usual.

## Python BMI setup

Initialize the simulation, prepare the plant properties, and register the
callback before the first call to `update`. When using DIMR, use the same
loaded D-Flow FM library instance for registration and BMI field access as the
one DIMR is advancing. A run with `BaptistDragCallback = true` stops at the first
computational step if the callback has not been registered.

For MPI runs, register the callback on each D-Flow FM rank and supply arrays
with that rank's local link layout.

The callback takes no arguments and returns a C `int`. Read `hu` in metres via
BMI inside the callback. Write `cfuveg` for bed drag and `alfaveg` for stem drag
as contiguous one-dimensional `float64` arrays in the same link order and shape
as `hu`. Both coefficients are dimensionless. D-Flow FM divides them by local
depth when applying model 3, so supply the coefficients before depth division.
Provide values for the full link arrays and clear entries for links where your
formulation supplies no drag contribution.

The example uses an initialized `BMIWrapper` as `flow`. Supply a
`calculate_drag(hu)` function that uses your plant properties and returns the
two coefficient arrays.

```python
import ctypes

import numpy as np


def register_baptist_drag(flow, calculate_drag):
    callback_type = ctypes.CFUNCTYPE(ctypes.c_int)
    errors = []

    @callback_type
    def update_drag():
        try:
            hu = flow.get_var("hu")
            bed, stems = calculate_drag(hu)
            bed = np.ascontiguousarray(bed, dtype=np.float64)
            stems = np.ascontiguousarray(stems, dtype=np.float64)
            for values in (bed, stems):
                if values.shape != hu.shape or np.any(~np.isfinite(values) | (values < 0)):
                    raise ValueError("Expected finite, nonnegative drag for each flow link.")
            flow.set_var("cfuveg", bed)
            flow.set_var("alfaveg", stems)
        except BaseException as error:
            errors.append(error)
            return 1
        return 0

    register = flow.library.dfm_set_vegetation_drag_callback
    register.argtypes = [callback_type]
    register.restype = None
    register(update_drag)
    return update_drag, errors
```

Keep the returned callback object alive until the simulation is finalized or
the callback is unregistered. Call BMI getters and setters from the callback
and leave time advancement to the outer driver. Treat `hu` as a read-only
view owned by D-Flow FM and read it again on each callback.

A callback return value of zero means success. In a serial run, a nonzero
return stops the step and passes an error to the outer driver. The example
below raises the captured Python exception with its traceback. Here `driver`
is the initialized DIMR or D-Flow FM BMI wrapper that advances the simulation.

```python
drag_callback, callback_errors = register_baptist_drag(flow, calculate_drag)
try:
    status = driver.update(duration_seconds)
finally:
    if callback_errors:
        raise callback_errors[0]
if status:
    raise RuntimeError(f"BMI update failed with status {status}.")
```

Passing a null function pointer unregisters the callback. D-Flow FM also
clears it during BMI finalization.

In an MPI run, a nonzero return or missing callback on any D-Flow FM rank
aborts the MPI job. Log Python exceptions inside the callback before returning
a nonzero status so the traceback is available in the job output.
