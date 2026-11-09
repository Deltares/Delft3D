"""Executable regression tests: set DFMOUTPUT_EXE, then run this file with Python."""

import os
from pathlib import Path
import subprocess
import tempfile
import unittest

from netCDF4 import Dataset
import numpy as np


class MapMergeTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        executable = os.environ.get("DFMOUTPUT_EXE")
        if not executable:
            raise unittest.SkipTest("Set DFMOUTPUT_EXE to the dfmoutput executable")
        cls.executable = str(Path(executable).resolve())

    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)

    @staticmethod
    def variable(ds, name, dtype, dimensions, values, **attributes):
        var = ds.createVariable(
            name, dtype, dimensions, fill_value=-999 if dtype != "S1" else None
        )
        var.setncatts(attributes)
        var[:] = values
        return var

    def network(self, ds):
        ds.createDimension("network1d_nNodes", 2)
        ds.createDimension("network1d_nEdges", 1)
        ds.createDimension("network1d_nGeometryNodes", 2)
        topology = ds.createVariable("network1d", "i4")
        topology.setncatts(
            {
                "cf_role": "mesh_topology",
                "topology_dimension": 1,
                "node_dimension": "network1d_nNodes",
                "edge_dimension": "network1d_nEdges",
                "node_coordinates": "network1d_node_x network1d_node_y",
                "edge_node_connectivity": "network1d_edge_nodes",
                "edge_geometry": "network1d_geometry",
                "edge_length": "network1d_edge_length",
                "node_id": "network1d_node_id",
                "node_long_name": "network1d_node_long_name",
                "branch_id": "network1d_branch_id",
                "branch_long_name": "network1d_branch_long_name",
            }
        )
        self.variable(
            ds,
            "network1d_edge_nodes",
            "i4",
            ("network1d_nEdges", "Two"),
            [[1, 2]],
            cf_role="edge_node_connectivity",
            start_index=1,
        )
        self.variable(ds, "network1d_edge_length", "f8", ("network1d_nEdges",), [2.0])
        for suffix, dimension, count in (
            ("node", "network1d_nNodes", 2),
            ("branch", "network1d_nEdges", 1),
        ):
            for label in ("id", "long_name"):
                self.variable(
                    ds,
                    f"network1d_{suffix}_{label}",
                    "S1",
                    (dimension, "strLengthIds"),
                    np.array([list(f"{i:08d}") for i in range(count)], dtype="S1"),
                )
        for axis, values in (("x", [0.0, 2.0]), ("y", [-1.0, -1.0])):
            for suffix, dimension in (
                ("node", "network1d_nNodes"),
                ("geom", "network1d_nGeometryNodes"),
            ):
                self.variable(
                    ds,
                    f"network1d_{suffix}_{axis}",
                    "f8",
                    (dimension,),
                    values,
                    standard_name=f"projection_{axis}_coordinate",
                    units="m",
                )
        geometry = ds.createVariable("network1d_geometry", "i4")
        geometry.setncatts(
            {
                "geometry_type": "line",
                "node_count": "network1d_geom_node_count",
                "node_coordinates": "network1d_geom_x network1d_geom_y",
            }
        )
        self.variable(ds, "network1d_geom_node_count", "i4", ("network1d_nEdges",), [2])

    def mesh(self, ds, dimension, global_ids, owners, partition, cell_positions=None):
        name = f"mesh{dimension}d"
        count = len(global_ids)
        location = "node" if dimension == 1 else "face"
        spatial_dimension = f"{name}_n{'Nodes' if dimension == 1 else 'Faces'}"
        ds.createDimension(spatial_dimension, count)
        topology = ds.createVariable(name, "i4")
        topology.cf_role = "mesh_topology"
        topology.topology_dimension = dimension
        topology.node_dimension = f"{name}_nNodes"
        topology.edge_dimension = f"{name}_nEdges"
        topology.node_coordinates = f"{name}_node_x {name}_node_y"
        topology.edge_coordinates = f"{name}_edge_x {name}_edge_y"
        topology.edge_node_connectivity = f"{name}_edge_nodes"
        if dimension == 1:
            self.network(ds)
            topology.coordinate_space = "network1d"
            topology.node_coordinates = (
                "mesh1d_node_branch mesh1d_node_offset mesh1d_node_x mesh1d_node_y"
            )
            topology.edge_coordinates = (
                "mesh1d_edge_branch mesh1d_edge_offset mesh1d_edge_x mesh1d_edge_y"
            )
            self.variable(
                ds,
                "mesh1d_node_branch",
                "i4",
                (spatial_dimension,),
                np.ones(count),
                start_index=1,
            )
            self.variable(
                ds, "mesh1d_node_offset", "f8", (spatial_dimension,), np.arange(count)
            )
            points = np.column_stack((np.arange(count), np.full(count, -1.0)))
            edges = np.array([(i, i + 1) for i in range(count - 1)])
        else:
            positions = list(range(count)) if cell_positions is None else cell_positions
            corners = sorted(
                {(x + dx, y) for x in positions for dx in (0, 1) for y in (0, 1)}
            )
            points = np.asarray(corners, dtype=float)
            node_index = {point: i for i, point in enumerate(corners)}
            faces = np.array(
                [
                    [
                        node_index[point]
                        for point in ((x, 0), (x + 1, 0), (x + 1, 1), (x, 1))
                    ]
                    for x in positions
                ]
            )
            edge_faces = {}
            for face, nodes in enumerate(faces):
                for a, b in zip(nodes, np.roll(nodes, -1)):
                    edge_faces.setdefault(tuple(sorted((a, b))), []).append(face + 1)
            edges = np.array(list(edge_faces))
            ds.createDimension(f"{name}_nNodes", len(points))
            ds.createDimension(f"{name}_nMax_face_nodes", 4)
            topology.face_dimension = spatial_dimension
            topology.face_coordinates = f"{name}_face_x {name}_face_y"
            topology.face_node_connectivity = f"{name}_face_nodes"
            topology.edge_face_connectivity = f"{name}_edge_faces"
            topology.max_face_nodes_dimension = f"{name}_nMax_face_nodes"
            self.variable(
                ds,
                f"{name}_face_nodes",
                "i4",
                (spatial_dimension, f"{name}_nMax_face_nodes"),
                faces + 1,
                cf_role="face_node_connectivity",
                start_index=1,
            )
            adjacent_faces = [
                faces_for_edge + [-999] * (2 - len(faces_for_edge))
                for faces_for_edge in edge_faces.values()
            ]
            for axis, values in zip(("x", "y"), points[faces].mean(axis=1).T):
                self.variable(
                    ds,
                    f"{name}_face_{axis}",
                    "f8",
                    (spatial_dimension,),
                    values,
                    mesh=name,
                    location="face",
                )
        ds.createDimension(f"{name}_nEdges", len(edges))
        if dimension == 1:
            self.variable(
                ds,
                "mesh1d_edge_branch",
                "i4",
                (f"{name}_nEdges",),
                np.ones(len(edges)),
                start_index=1,
            )
            self.variable(
                ds,
                "mesh1d_edge_offset",
                "f8",
                (f"{name}_nEdges",),
                np.arange(len(edges)) + 0.5,
            )
        if dimension == 2:
            self.variable(
                ds,
                f"{name}_edge_faces",
                "i4",
                (f"{name}_nEdges", "Two"),
                adjacent_faces,
                cf_role="edge_face_connectivity",
                start_index=1,
            )
        self.variable(
            ds,
            f"{name}_edge_nodes",
            "i4",
            (f"{name}_nEdges", "Two"),
            edges + 1,
            cf_role="edge_node_connectivity",
            start_index=1,
        )
        for axis, values in zip(("x", "y"), points.T):
            self.variable(
                ds,
                f"{name}_node_{axis}",
                "f8",
                (f"{name}_nNodes",),
                values,
                mesh=name,
                location="node",
                standard_name=f"projection_{axis}_coordinate",
                units="m",
            )
        for axis, values in zip(("x", "y"), points[edges].mean(axis=1).T):
            self.variable(
                ds,
                f"{name}_edge_{axis}",
                "f8",
                (f"{name}_nEdges",),
                values,
                mesh=name,
                location="edge",
            )
        self.variable(
            ds,
            f"{name}_flowelem_domain",
            "i4",
            (spatial_dimension,),
            owners,
            mesh=name,
            location=location,
        )
        self.variable(
            ds,
            f"{name}_flowelem_globalnr",
            "i4",
            (spatial_dimension,),
            global_ids,
            mesh=name,
            location=location,
            valid_max=10000,
        )
        values = np.asarray(global_ids, dtype=float)
        values = np.where(np.asarray(owners) == partition, values, -123.0)
        self.variable(
            ds,
            f"{name}_s1",
            "f8",
            ("time", spatial_dimension),
            [values, values + 0.5],
            mesh=name,
            location=location,
        )
        if dimension == 1:
            labels = np.array(
                [list(f"{value:08d}") for value in global_ids], dtype="S1"
            )
            self.variable(
                ds,
                f"{name}_node_id",
                "S1",
                (spatial_dimension, "strLengthIds"),
                labels,
                mesh=name,
                location="node",
            )
        return name

    def partition(
        self,
        partition,
        *,
        dimensions=(2,),
        owners=(2, 5, 5),
        ids=None,
        contacts=False,
        owners_1d=None,
        layers=False,
        cell_positions=None,
    ):
        path = self.root / f"model_{partition:04d}_map.nc"
        with Dataset(path, "w", format="NETCDF3_64BIT_OFFSET") as ds:
            ds.Conventions = "CF-1.8 UGRID-1.0"
            ds.createDimension("Two", 2)
            ds.createDimension("strLengthIds", 8)
            ds.createDimension("time", None)
            self.variable(
                ds,
                "time",
                "f8",
                ("time",),
                [0, 60],
                standard_name="time",
                units="seconds since 2000-01-01",
            )
            self.variable(ds, "timestep", "f8", ("time",), [0, 60])
            for dimension in dimensions:
                global_ids = (
                    ids
                    if ids is not None
                    else ([1100, 1200, 1300] if dimension == 1 else [100, 200, 300])
                )
                mesh_owners = (
                    owners_1d if dimension == 1 and owners_1d is not None else owners
                )
                self.mesh(
                    ds, dimension, global_ids, mesh_owners, partition, cell_positions
                )
            if layers:
                ds.createDimension("mesh2d_nLayers", 2)
                ds["mesh2d"].layer_dimension = "mesh2d_nLayers"
                values = np.asarray([100, 200, 300], dtype=float)
                values = np.where(np.asarray(owners) == partition, values, -123.0)
                values = np.stack((values, values + 1), axis=-1)
                self.variable(
                    ds,
                    "mesh2d_temperature",
                    "f8",
                    ("time", "mesh2d_nFaces", "mesh2d_nLayers"),
                    [values, values + 0.5],
                    mesh="mesh2d",
                    location="face",
                )
            if contacts:
                ds.createDimension("contact_1D2D_nContacts", 2)
                self.variable(
                    ds,
                    "contact_1D2D",
                    "i4",
                    ("contact_1D2D_nContacts", "Two"),
                    [[1, 1], [3, 3]],
                    cf_role="mesh_topology_contact",
                    contact="mesh1d: node mesh2d: face",
                    contact_type="contact_1D2D_contact_type",
                    start_index=1,
                )
                self.variable(
                    ds,
                    "contact_1D2D_contact_type",
                    "i4",
                    ("contact_1D2D_nContacts",),
                    [3, 3],
                )
                values = [
                    10 if partition == owners[0] else -123,
                    30 if partition == owners[2] else -123,
                ]
                self.variable(
                    ds,
                    "contact_1D2D_q1",
                    "f8",
                    ("time", "contact_1D2D_nContacts"),
                    [values, np.asarray(values) + 0.5],
                    mesh="contact_1D2D",
                    location="contact",
                )
        return path

    def merge(self, files, error=None):
        output = self.root / "merged.nc"
        result = subprocess.run(
            [
                self.executable,
                "mapmerge",
                "--force",
                "--infile",
                *map(str, files),
                "--outfile",
                str(output),
            ],
            capture_output=True,
            text=True,
            timeout=60,
        )
        log = result.stdout + result.stderr
        self.assertEqual(result.returncode, 0, log)
        if error is not None:
            self.assertIn(error, log)
            return output
        self.assertNotIn("Error:", log)
        self.assertNotIn("forrtl:", log)
        return output

    def check_mesh(self, ds, dimension, expected_ids):
        name = f"mesh{dimension}d"
        np.testing.assert_array_equal(ds[f"{name}_flowelem_globalnr"][:], expected_ids)
        np.testing.assert_array_equal(
            ds[f"{name}_s1"][:], [expected_ids, np.asarray(expected_ids) + 0.5]
        )
        node_count = len(ds.dimensions[f"{name}_nNodes"])
        edge_nodes = ds[f"{name}_edge_nodes"][:]
        self.assertTrue(np.all((edge_nodes >= 1) & (edge_nodes <= node_count)))
        points = np.column_stack((ds[f"{name}_node_x"][:], ds[f"{name}_node_y"][:]))
        self.assertEqual(len(np.unique(points, axis=0)), node_count)
        if dimension == 1:
            np.testing.assert_array_equal(edge_nodes, [[1, 2], [2, 3]])
            labels = [b"".join(row).decode() for row in ds[f"{name}_node_id"][:]]
            self.assertEqual(labels, [f"{value:08d}" for value in expected_ids])
        else:
            self.assertEqual(node_count, 8)
            self.assertEqual(len(ds.dimensions[f"{name}_nEdges"]), 10)
            faces = ds[f"{name}_face_nodes"][:]
            self.assertTrue(np.all((faces >= 1) & (faces <= node_count)))
            centers = points[faces - 1].mean(axis=1)
            np.testing.assert_array_equal(centers[:, 0], ds[f"{name}_face_x"][:])
            adjacent = ds[f"{name}_edge_faces"][:].compressed()
            self.assertTrue(np.all((adjacent >= 1) & (adjacent <= len(expected_ids))))

    def test_sparse_2d_with_missing_first_middle_and_last_files(self):
        files = [
            self.partition(5),
            self.root / "model_0000_map.nc",
            self.partition(2),
            self.root / "model_0007_map.nc",
        ]
        with Dataset(self.merge(files)) as ds:
            self.check_mesh(ds, 2, [100, 200, 300])
            np.testing.assert_array_equal(ds["mesh2d_flowelem_domain"][:], [2, 5, 5])

    def test_single_surviving_partition(self):
        with Dataset(self.merge([self.partition(5, owners=(5, 5, 5))])) as ds:
            self.check_mesh(ds, 2, [100, 200, 300])

    def test_global_ids_without_original_count_attribute(self):
        files = [self.partition(p) for p in (2, 5)]
        for path in files:
            with Dataset(path, "a") as ds:
                ds["mesh2d_flowelem_globalnr"].delncattr("valid_max")
        with Dataset(self.merge(files)) as ds:
            self.check_mesh(ds, 2, [100, 200, 300])

    def test_layered_mesh_with_missing_partitions(self):
        files = [self.partition(p, layers=True) for p in (2, 5)]
        with Dataset(self.merge(files)) as ds:
            self.check_mesh(ds, 2, [100, 200, 300])
            expected = np.array([[100, 101], [200, 201], [300, 301]])
            np.testing.assert_array_equal(
                ds["mesh2d_temperature"][:], [expected, expected + 0.5]
            )

    def test_different_local_cropped_meshes(self):
        files = [
            self.partition(2, owners=(2, 5), ids=[100, 200], cell_positions=[0, 1]),
            self.partition(5),
        ]
        with Dataset(self.merge(files)) as ds:
            self.check_mesh(ds, 2, [100, 200, 300])

    def test_partition_with_only_ghost_cells(self):
        files = [self.partition(p, owners=(5, 5, 5)) for p in (2, 5)]
        with Dataset(self.merge(files)) as ds:
            self.check_mesh(ds, 2, [100, 200, 300])

    def test_all_files_missing_is_reported(self):
        self.merge([self.root / "model_0005_map.nc"], error="no existing input files")

    def test_unreadable_existing_file_is_not_skipped(self):
        path = self.root / "model_0000_map.nc"
        path.write_text("not a NetCDF file", encoding="ascii")
        self.merge([path], error="could not open file")

    def test_duplicate_partition_is_reported(self):
        path = self.partition(2)
        self.merge([path, path], error="duplicate partition number")


if __name__ == "__main__":
    unittest.main()
