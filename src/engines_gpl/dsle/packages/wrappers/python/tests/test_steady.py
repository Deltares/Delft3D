import unittest

import numpy as np

from pyzsf import zsf_calc_steady


class TestSaltLoadSteady(unittest.TestCase):
    def setUp(self):
        self.parameters = {
            "lock_length": 240.0,
            "lock_width": 12.0,
            "lock_bottom": -4.0,
            "num_cycles": 24.0,
            "door_time_to_open": 300.0,
            "leveling_time": 300.0,
            "calibration_coefficient": 1.0,
            "symmetry_coefficient": 1.0,
            "ship_volume_sea_to_lake": 0.0,
            "ship_volume_lake_to_sea": 0.0,
            "head_sea": 0.0,
            "salinity_sea": 25.0,
            "temperature_sea": 15.0,
            "head_lake": 0.0,
            "salinity_lake": 5.0,
            "temperature_lake": 15.0,
            "flushing_discharge_high_tide": 0.0,
            "flushing_discharge_low_tide": 0.0,
            "density_current_factor_sea": 1.0,
            "density_current_factor_lake": 1.0,
        }

        self.reference_results = zsf_calc_steady(**self.parameters)
        self.reference_load = self.reference_results["salt_load_lake"]

    @staticmethod
    def assert_allclose_loose(*args, **kwargs):
        return np.testing.assert_allclose(*args, **kwargs, rtol=0.01, atol=0.01)

    @staticmethod
    def assert_allclose_tight(*args, **kwargs):
        # For when results are supposed to be the same, but might be slightly
        # different due to the convergence criterion and/or numerical
        # inaccuries.
        return np.testing.assert_allclose(*args, **kwargs, rtol=1e-5, atol=1e-5)

    def test_reference_load(self):
        self.assert_allclose_loose(zsf_calc_steady(**self.parameters)["salt_load_lake"], -34.315)

    def test_deeper_lock(self):
        sl_high_head = zsf_calc_steady(**dict(self.parameters, head_lake=4.0, head_sea=4.0,))[
            "salt_load_lake"
        ]

        sl_low_bottom = zsf_calc_steady(**dict(self.parameters, lock_bottom=-8.0,))[
            "salt_load_lake"
        ]

        sl_higher_bottom = zsf_calc_steady(**dict(self.parameters, lock_bottom=-2.0,))[
            "salt_load_lake"
        ]

        # Comparison checks
        self.assertLess(-sl_higher_bottom, -self.reference_load)
        self.assertGreater(-sl_low_bottom, -sl_higher_bottom)
        self.assert_allclose_tight(sl_high_head, sl_low_bottom)

        # Check values against known good values
        self.assert_allclose_loose(sl_high_head, -97.346)

        self.assert_allclose_loose(sl_low_bottom, -97.346)

        self.assert_allclose_loose(sl_higher_bottom, -11.138)

    def test_salinity_lake_sea(self):
        sl_sal_gap_smaller = zsf_calc_steady(
            **dict(self.parameters, salinity_lake=10.0, salinity_sea=20.0,)
        )["salt_load_lake"]

        sl_sal_gap_wider = zsf_calc_steady(
            **dict(self.parameters, salinity_lake=0.0, salinity_sea=30.0,)
        )["salt_load_lake"]

        # Comparison checks
        self.assertGreater(-self.reference_load, -sl_sal_gap_smaller)
        self.assertGreater(-sl_sal_gap_wider, -self.reference_load)

        # Check values against known good values
        self.assert_allclose_loose(sl_sal_gap_smaller, -11.138)

        self.assert_allclose_loose(sl_sal_gap_wider, -64.234)

    def test_lock_dimensions(self):
        sl_lock_longer = zsf_calc_steady(**dict(self.parameters, lock_length=480.0,))[
            "salt_load_lake"
        ]

        sl_lock_wider = zsf_calc_steady(**dict(self.parameters, lock_width=24.0,))["salt_load_lake"]

        # Comparison checks
        # NOTE: A longer lock does not imply more salt intrusion. A deeper or
        # wider one does.
        self.assertGreater(-sl_lock_wider, -self.reference_load)

        # Check values against known good values
        self.assert_allclose_loose(sl_lock_longer, -28.806)

        self.assert_allclose_loose(sl_lock_wider, -68.632)

    def test_num_cycles(self):
        num_cycles_to_sl = [
            (54.0, -9.155),
            (43.0, -16.083),
            (36.0, -21.604),
            (28.0, -29.721),
            (20.0, -37.758),
            (16.0, -37.176),
            (12.0, -31.042),
            (8.0, -21.312),
        ]

        for num_cycles, sl_ref in num_cycles_to_sl:
            result = zsf_calc_steady(**dict(self.parameters, num_cycles=num_cycles,))

            self.assert_allclose_loose(
                result["salt_load_lake"], sl_ref, err_msg=f"num_cycles: {num_cycles}"
            )

    def test_quicker_door_level(self):
        sl_door_quick_open = zsf_calc_steady(**dict(self.parameters, door_time_to_open=0.0,))[
            "salt_load_lake"
        ]

        sl_door_quick_level = zsf_calc_steady(**dict(self.parameters, leveling_time=0.0,))[
            "salt_load_lake"
        ]

        # Comparison checks
        self.assertGreater(-sl_door_quick_open, -self.reference_load)
        self.assert_allclose_tight(sl_door_quick_open, sl_door_quick_level)

        # Check values against known good values
        self.assert_allclose_loose(sl_door_quick_open, -43.679)

        self.assert_allclose_loose(sl_door_quick_level, -43.679)

    def test_calibration_factor(self):
        # TODO: Why do we also change num_cycles 14.4?
        sl_ref = zsf_calc_steady(**dict(self.parameters, num_cycles=14.4,))["salt_load_lake"]

        sl_calibration_fac = zsf_calc_steady(
            **dict(self.parameters, num_cycles=14.4, calibration_coefficient=0.5,)
        )["salt_load_lake"]

        # Comparison checks
        self.assertGreater(-sl_ref, -sl_calibration_fac)

        # Check values against known good values
        self.assert_allclose_loose(sl_calibration_fac, -20.590)

    def test_symmetry_coefficient(self):
        results_sym_0_5 = zsf_calc_steady(**dict(self.parameters, symmetry_coefficient=0.5,))

        results_sym_1_5 = zsf_calc_steady(**dict(self.parameters, symmetry_coefficient=1.5,))

        sl_sym_0_5 = results_sym_0_5["salt_load_lake"]
        sl_sym_1_5 = results_sym_1_5["salt_load_lake"]

        disch_to_lake_sym_0_5 = results_sym_0_5["discharge_to_lake"]
        disch_to_sea_sym_0_5 = results_sym_0_5["discharge_to_sea"]
        disch_to_lake_sym_1_5 = results_sym_1_5["discharge_to_lake"]
        disch_to_sea_sym_1_5 = results_sym_1_5["discharge_to_sea"]

        # Comparison checks
        # Any asymmetry in door open times results in a lower mass load
        self.assertGreater(-self.reference_load, -sl_sym_0_5)
        # Shorter opened lake door -> volume exchanged in lock exchange is
        # lower -> cycle-averaged discharge is lower.
        self.assertLess(disch_to_lake_sym_0_5, self.reference_results["discharge_to_lake"])
        # It does not matter what direction the asymmetry is for the mass load
        # (when not flushing). Discharges flip side.
        self.assert_allclose_tight(
            sl_sym_0_5, sl_sym_1_5,
        )
        self.assert_allclose_tight(
            disch_to_lake_sym_0_5, disch_to_sea_sym_1_5,
        )
        self.assert_allclose_tight(
            disch_to_sea_sym_0_5, disch_to_lake_sym_1_5,
        )

        # Check values against known good values
        self.assert_allclose_loose(sl_sym_0_5, -24.707)

        self.assert_allclose_loose(sl_sym_1_5, -24.707)

    def test_ship_water_deplacement(self):
        results_ship_sea_to_lake = zsf_calc_steady(
            **dict(self.parameters, ship_volume_sea_to_lake=5000.0,)
        )
        sl_ship_sea_to_lake = results_ship_sea_to_lake["salt_load_lake"]

        results_ship_lake_to_sea = zsf_calc_steady(
            **dict(self.parameters, ship_volume_lake_to_sea=5000.0,)
        )
        sl_ship_lake_to_sea = results_ship_lake_to_sea["salt_load_lake"]

        results_ship_both = zsf_calc_steady(
            **dict(self.parameters, ship_volume_sea_to_lake=5000.0, ship_volume_lake_to_sea=5000.0,)
        )
        sl_ship_both = results_ship_both["salt_load_lake"]

        # Comparison checks
        self.assertGreater(-self.reference_load, -sl_ship_sea_to_lake)
        self.assertGreater(-sl_ship_lake_to_sea, -sl_ship_both)
        self.assertGreater(-sl_ship_both, -sl_ship_sea_to_lake)

        # A ship going from sea to lake means a movement of water in the
        # opposite (lake to sea) direction
        self.assertGreater(
            results_ship_sea_to_lake["discharge_from_lake"],
            self.reference_results["discharge_from_lake"],
        )
        self.assertGreater(
            results_ship_lake_to_sea["discharge_from_sea"],
            self.reference_results["discharge_from_sea"],
        )

        # Discharges are flipped when ships move in opposite direction
        self.assert_allclose_tight(
            results_ship_sea_to_lake["discharge_from_lake"],
            results_ship_lake_to_sea["discharge_from_sea"],
        )
        self.assert_allclose_tight(
            results_ship_sea_to_lake["discharge_to_lake"],
            results_ship_lake_to_sea["discharge_to_sea"],
        )
        self.assert_allclose_tight(
            results_ship_sea_to_lake["discharge_from_sea"],
            results_ship_lake_to_sea["discharge_from_lake"],
        )
        self.assert_allclose_tight(
            results_ship_sea_to_lake["discharge_to_sea"],
            results_ship_lake_to_sea["discharge_to_lake"],
        )

        # Check values against known good values
        self.assert_allclose_loose(results_ship_sea_to_lake["salt_load_lake"], -8.846)

        self.assert_allclose_loose(results_ship_lake_to_sea["salt_load_lake"], -50.513)

        self.assert_allclose_loose(results_ship_both["salt_load_lake"], -22.373)

    def test_bubble_screen(self):
        sl_bubble_50 = zsf_calc_steady(
            **dict(
                self.parameters, density_current_factor_sea=0.5, density_current_factor_lake=0.5,
            )
        )["salt_load_lake"]

        sl_bubble_30 = zsf_calc_steady(
            **dict(
                self.parameters, density_current_factor_sea=0.25, density_current_factor_lake=0.25,
            )
        )["salt_load_lake"]

        # Comparison checks
        self.assertGreater(-self.reference_load, -sl_bubble_50)
        self.assertGreater(-sl_bubble_50, -sl_bubble_30)

        # Check values against known good values
        self.assert_allclose_loose(sl_bubble_50, -14.403)

        self.assert_allclose_loose(sl_bubble_30, -6.369)

    def test_flushing_equal_head(self):
        results_flushing_lw = zsf_calc_steady(
            **dict(self.parameters, flushing_discharge_low_tide=1.0,)
        )

        results_flushing_hw = zsf_calc_steady(
            **dict(self.parameters, flushing_discharge_high_tide=1.0,)
        )

        # Comparison checks
        # Equal water level dictates that it is _high_ tide (convention).
        # There is therefore no flushing in the low tide test.
        self.assert_allclose_tight(results_flushing_lw["salt_load_lake"], self.reference_load)
        self.assertGreater(-self.reference_load, -results_flushing_hw["salt_load_lake"])

        # Check values against known good values
        self.assert_allclose_loose(results_flushing_lw["salt_load_lake"], -34.316)

        self.assert_allclose_loose(results_flushing_hw["salt_load_lake"], -25.035)

    def test_low_high_tide(self):
        results_low_tide = zsf_calc_steady(**dict(self.parameters, head_sea=-2.0,))

        results_high_tide = zsf_calc_steady(**dict(self.parameters, head_sea=2.0,))

        results_flushing_low_tide = zsf_calc_steady(
            **dict(self.parameters, head_sea=-2.0, flushing_discharge_low_tide=1.0,)
        )

        results_flushing_high_tide = zsf_calc_steady(
            **dict(self.parameters, head_sea=2.0, flushing_discharge_high_tide=1.0,)
        )

        # Comparison checks
        self.assertGreater(-self.reference_load, -results_low_tide["salt_load_lake"])
        self.assertGreater(-results_high_tide["salt_load_lake"], -self.reference_load)

        self.assertGreater(
            -results_high_tide["salt_load_lake"], -results_flushing_high_tide["salt_load_lake"]
        )
        self.assertGreater(
            -results_low_tide["salt_load_lake"], -results_flushing_low_tide["salt_load_lake"]
        )

        self.assertGreater(
            results_low_tide["discharge_from_lake"], self.reference_results["discharge_from_lake"]
        )
        self.assertGreater(
            results_high_tide["discharge_from_sea"], self.reference_results["discharge_from_sea"]
        )

        # Check values against known good values
        self.assert_allclose_loose(results_low_tide["salt_load_lake"], -2.189)

        self.assert_allclose_loose(results_high_tide["salt_load_lake"], -73.995)

        self.assert_allclose_loose(results_flushing_low_tide["salt_load_lake"], 4.587)

        self.assert_allclose_loose(results_flushing_high_tide["salt_load_lake"], -63.516)

    def test_sill(self):
        sl_sill_sea = zsf_calc_steady(**dict(self.parameters, sill_height_sea=1.0,))[
            "salt_load_lake"
        ]

        sl_sill_lake = zsf_calc_steady(**dict(self.parameters, sill_height_lake=1.0,))[
            "salt_load_lake"
        ]

        # Comparison checks
        self.assertGreater(-self.reference_load, -sl_sill_sea)
        self.assertGreater(-sl_sill_sea, -sl_sill_lake)

        # Check values against known good values
        self.assert_allclose_loose(sl_sill_sea, -32.043)

        self.assert_allclose_loose(sl_sill_lake, -26.126)

    def test_distance_door_bubble_screen(self):
        base_params = dict(
            self.parameters, density_current_factor_sea=0.25, density_current_factor_lake=0.25
        )

        sl_bubble_base = zsf_calc_steady(**dict(base_params,))["salt_load_lake"]

        sl_bubble_distance_sea = zsf_calc_steady(
            **dict(base_params, distance_door_bubble_screen_sea=4.0,)
        )["salt_load_lake"]

        sl_bubble_distance_lake = zsf_calc_steady(
            **dict(base_params, distance_door_bubble_screen_lake=4.0,)
        )["salt_load_lake"]

        # Comparison checks
        self.assertGreater(-self.reference_load, -sl_bubble_base)
        self.assertGreater(-sl_bubble_distance_sea, -sl_bubble_base)
        self.assertGreater(-sl_bubble_distance_lake, -sl_bubble_base)

        # Values are not _exactly_ equal due to differences in computation order,
        # but because the effect is the same they should be very close
        self.assertNotEqual(sl_bubble_distance_lake, sl_bubble_distance_sea)
        self.assert_allclose_loose(sl_bubble_distance_lake, sl_bubble_distance_sea)

        # Check values against known good values
        self.assert_allclose_loose(sl_bubble_distance_sea, -6.467)
        self.assert_allclose_loose(sl_bubble_distance_lake, -6.467)
