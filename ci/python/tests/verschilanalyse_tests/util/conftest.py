import pytest
from ci_tools.verschilanalyse.util.verschillentool import (
    VariableRegistry,
    Tolerance,
    OutputType,
)

@pytest.fixture(scope="session")
def test_variable_registry() -> VariableRegistry:
    """
    Shared variable registry available to all verschilanalyse tests.
    New variables can be added here later.
    """
    variable_registry = VariableRegistry()

    # Core variables present in all models
    variable_registry.register(
        name="sea_surface_height",
        unit="m",
        tolerances={
            OutputType.HIS: Tolerance(max=0.01, bias=0.0001, rms=0.001),
            OutputType.MAP: Tolerance(max=0.05, bias=0.0001, rms=0.001),
        },
    )

    variable_registry.register(
        name="sea_water_speed",
        unit="m/s",
        tolerances={
            OutputType.HIS: Tolerance(max=0.05, bias=0.0005, rms=0.005),
            OutputType.MAP: Tolerance(max=0.10, bias=0.0005, rms=0.005),
        },
    )

    return variable_registry

