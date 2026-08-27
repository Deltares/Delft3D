import tempfile
import unittest
from pathlib import Path

from stage_dflowfm_model import ModelCollector, copy_file, file_inventory, stage_model


class ModelCollectorTest(unittest.TestCase):
    def test_bc_file_is_collected_without_scanning_its_contents(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            mdu = root / "model.mdu"
            forcing = root / "large.bc"
            mdu.write_text("forcingFile = large.bc\n", encoding="utf-8")
            forcing.write_text(
                "[Forcing]\nFILENAME = must_not_be_followed.nc\n",
                encoding="utf-8",
            )

            collector = ModelCollector(mdu)
            files = collector.collect()

            self.assertEqual({mdu.resolve(), forcing.resolve()}, files)
            self.assertFalse(collector.missing)

    def test_collects_file_valued_structure_parameter(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            mdu = root / "model.mdu"
            structures = root / "structures.ini"
            timeseries = root / "gate.tim"
            mdu.write_text("StructureFile = structures.ini\n", encoding="utf-8")
            structures.write_text(
                "[structure]\n"
                "CrestLevel = -5.85\n"
                "GateLowerEdgeLevel = gate.tim\n",
                encoding="utf-8",
            )
            timeseries.write_text("0 0\n", encoding="utf-8")

            collector = ModelCollector(mdu)
            files = collector.collect()

            self.assertEqual({mdu.resolve(), structures.resolve(), timeseries.resolve()}, files)
            self.assertFalse(collector.missing)

    def test_rerun_skips_matching_file_and_repairs_wrong_size(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            source = root / "source"
            destination = root / "local"
            source.mkdir()
            destination.mkdir()
            mdu = source / "model.mdu"
            data = source / "data.nc"
            mdu.write_text("NetFile = data.nc\n", encoding="utf-8")
            data.write_bytes(b"correct data")
            (destination / "model.mdu").write_text(
                "NetFile = data.nc\n", encoding="utf-8"
            )
            (destination / "data.nc").write_bytes(b"bad")
            mdu_timestamp = (destination / "model.mdu").stat().st_mtime_ns

            collector, _ = stage_model(mdu, destination)

            self.assertEqual(mdu_timestamp, (destination / "model.mdu").stat().st_mtime_ns)
            self.assertEqual(b"correct data", (destination / "data.nc").read_bytes())
            self.assertEqual(1, collector.copied_files)
            self.assertEqual(len(b"correct data"), collector.copied_bytes)
            self.assertEqual(1, collector.skipped_files)

    def test_collects_space_separated_relative_file_list(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            run = root / "run"
            geometry = root / "geometry"
            run.mkdir()
            geometry.mkdir()
            mdu = run / "model.mdu"
            mdu.write_text(
                "ThinDamFile = ../geometry/first.pli ../geometry/second.pli "
                "../geometry/third.pli\n",
                encoding="utf-8",
            )
            for name in ("first.pli", "second.pli", "third.pli"):
                (geometry / name).write_text(name, encoding="utf-8")

            collector = ModelCollector(mdu)
            files = collector.collect()

            self.assertEqual(4, len(files))
            self.assertFalse(collector.missing)

    def test_scan_callback_visits_every_collected_file(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            mdu = root / "model.mdu"
            child = root / "child.ext"
            mdu.write_text("ExtForceFile = child.ext\n", encoding="utf-8")
            child.write_text("[Boundary]\n", encoding="utf-8")
            scanned: list[Path] = []

            collector = ModelCollector(
                mdu, on_scan=lambda _found, _queued, path: scanned.append(path)
            )
            files = collector.collect()

            self.assertEqual(files, set(scanned))

    def test_copy_file_reports_all_copied_bytes(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            source = root / "source.dat"
            target = root / "target.dat"
            content = b"test data" * 1024
            source.write_bytes(content)
            reported_bytes: list[int] = []

            copy_file(source, target, reported_bytes.append)

            self.assertEqual(content, target.read_bytes())
            self.assertEqual(len(content), sum(reported_bytes))

    def test_excluded_reference_is_recorded_and_not_staged(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            source = root / "source"
            destination = root / "local"
            source.mkdir()
            (source / "model.mdu").write_text(
                "ExtForceFile = forcing.ext\n", encoding="utf-8"
            )
            (source / "forcing.ext").write_text(
                "FILENAME = meteo/large.nc\n", encoding="utf-8"
            )
            (source / "meteo").mkdir()
            (source / "meteo" / "large.nc").write_bytes(b"large")

            collector, _ = stage_model(
                source / "model.mdu",
                destination,
                exclude_patterns=["*/meteo/*"],
            )

            self.assertEqual(2, len(collector.files))
            self.assertEqual(1, len(collector.excluded))
            self.assertFalse((destination / "meteo" / "large.nc").exists())

    def test_file_inventory_is_largest_first(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            small = root / "small.dat"
            large = root / "large.dat"
            small.write_bytes(b"a" * 10)
            large.write_bytes(b"b" * 2048)

            inventory = file_inventory([small, large])

            self.assertIn("2.00 KiB", inventory[0])
            self.assertIn(str(large), inventory[0])
            self.assertIn("10.00 B", inventory[1])
            self.assertIn(str(small), inventory[1])

    def test_collects_nested_references_from_run_directory(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            run = root / "computations" / "case"
            inputs = root / "inputs"
            run.mkdir(parents=True)
            inputs.mkdir()
            (run / "model.mdu").write_text(
                "[General]\nPathsRelativeToParent = 0\n"
                "[geometry]\nNetFile = grid_net.nc\n"
                "[external forcing]\nExtForceFileNew = ../../inputs/forcing.ext\n",
                encoding="utf-8",
            )
            (run / "grid_net.nc").write_bytes(b"netcdf")
            (inputs / "forcing.ext").write_text(
                "[Boundary]\nlocationFile = ../../inputs/boundary.pli\n"
                "forcingFile = ../../inputs/water level.bc\n",
                encoding="utf-8",
            )
            (inputs / "boundary.pli").write_text("boundary\n", encoding="utf-8")
            (inputs / "water level.bc").write_text("[Forcing]\n", encoding="utf-8")

            collector = ModelCollector(run / "model.mdu")
            files = collector.collect()

            self.assertEqual(5, len(files))
            self.assertFalse(collector.missing)

    def test_staging_preserves_parent_relative_structure(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            source = root / "source"
            run = source / "run"
            forcing = source / "forcing"
            destination = root / "local"
            run.mkdir(parents=True)
            forcing.mkdir()
            (run / "model.mdu").write_text(
                "PathsRelativeToParent = 1\nExtForceFileNew = ../forcing/input.ext\n",
                encoding="utf-8",
            )
            (forcing / "input.ext").write_text("forcingFile = data.bc\n", encoding="utf-8")
            (forcing / "data.bc").write_text("[Forcing]\n", encoding="utf-8")

            collector, source_root = stage_model(run / "model.mdu", destination)

            self.assertEqual(source, source_root)
            self.assertEqual(3, len(collector.files))
            self.assertTrue((destination / "run" / "model.mdu").is_file())
            self.assertTrue((destination / "forcing" / "data.bc").is_file())
            self.assertTrue((destination / "stage_manifest.json").is_file())


if __name__ == "__main__":
    unittest.main()