#!/usr/bin/env python3
"""Generate the deterministic D-Flow FM immersed-boundary verification cases."""

from __future__ import annotations

import argparse
import json
import math
from dataclasses import dataclass
from pathlib import Path
from typing import Any
from xml.sax.saxutils import escape

import numpy as np
from netCDF4 import Dataset


FAMILY_ROOT = Path(__file__).resolve().parents[1]
PLAN_BETA = math.atan(43.0 / 80.0)
GRAVITY = 9.81
CHANNEL_QS = 5.0
CHANNEL_CHEZY = 65.0
CHANNEL_DEPTH = 4.0
CHANNEL_SLOPE = (CHANNEL_QS / (CHANNEL_CHEZY * CHANNEL_DEPTH**1.5)) ** 2
ANNULUS_INNER_RADIUS = 50.0
ANNULUS_WIDTH = 20.0
ANNULUS_CHEZY = 40.0
ANNULUS_SLOPE = -0.001
ANNULUS_DZ_DTHETA = ANNULUS_SLOPE * 60.0
ANNULUS_DISCHARGE = 40.0
WIND_SPEED = 10.0
WIND_DRAG = 0.0012
AIR_DENSITY = 1.2
WATER_DENSITY = 1000.0
FILL_INT = -999
FILL_FLOAT = -999.0


@dataclass(frozen=True)
class CaseSpec:
    case_id: str
    plan_id: str
    name: str
    kind: str
    variant: str
    dx: float
    parallel: bool = False
    tangential_correction: int = 0

    @property
    def uses_ibm(self) -> bool:
        return self.variant == "ibm"

    @property
    def duration_hours(self) -> float:
        if self.kind == "channel":
            return 8.0
        if self.kind == "annulus":
            return 2.0
        if self.kind == "wind":
            return 12.0
        return 0.5


def build_case_specs() -> list[CaseSpec]:
    cases = [
        CaseSpec("c01_channel_aligned", "I1", "Aligned channel control", "channel", "drypoints", 25.0),
        CaseSpec("c02_channel_aligned_ibm", "I1b", "Aligned channel with IBM enabled", "channel", "ibm", 25.0),
        CaseSpec(
            "c03_channel_oblique_staircase",
            "I2",
            "Oblique channel with staircase banks",
            "channel",
            "drypoints",
            25.0,
        ),
        CaseSpec("c04_channel_oblique_cutcell", "I3", "Oblique channel with cut cells only", "channel", "cutcell", 25.0),
        CaseSpec("c05_channel_oblique_ibm", "I4", "Oblique channel with IBM", "channel", "ibm", 25.0),
        CaseSpec(
            "c06_channel_oblique_ibm_parallel",
            "I5",
            "Oblique channel with IBM on two MPI ranks",
            "channel",
            "ibm",
            25.0,
            parallel=True,
        ),
        CaseSpec("c07_annulus_staircase", "I7", "Annular channel with staircase banks", "annulus", "drypoints", 2.0),
        CaseSpec("c08_annulus_cutcell", "I7", "Annular channel with cut cells only", "annulus", "cutcell", 2.0),
        CaseSpec("c09_annulus_ibm", "I7", "Annular channel with IBM", "annulus", "ibm", 2.0),
        CaseSpec("c10_lake_at_rest_ibm", "I8", "Oblique lake at rest with IBM", "lake", "ibm", 25.0),
        CaseSpec("c11_wind_setup_aligned", "I9", "Aligned wind-setup floor", "wind", "drypoints", 25.0),
        CaseSpec("c12_wind_setup_staircase", "I9", "Oblique wind setup with staircase banks", "wind", "drypoints", 25.0),
        CaseSpec("c13_wind_setup_cutcell", "I9", "Oblique wind setup with cut cells only", "wind", "cutcell", 25.0),
        CaseSpec("c14_wind_setup_ibm", "I9", "Oblique wind setup with IBM", "wind", "ibm", 25.0),
        CaseSpec(
            "c15_wind_setup_ibm_tangential",
            "I9",
            "Oblique wind setup with tangential correction",
            "wind",
            "ibm",
            25.0,
            tangential_correction=1,
        ),
    ]

    next_id = 16
    for kind, resolutions, variants in (
        ("channel", (50.0, 12.5), ("staircase", "cutcell", "ibm")),
        ("annulus", (4.0, 1.0), ("staircase", "cutcell", "ibm")),
        ("wind", (50.0, 12.5), ("staircase", "cutcell", "ibm")),
    ):
        prefix = {"channel": "channel_oblique", "annulus": "annulus", "wind": "wind_setup"}[kind]
        for dx in resolutions:
            dx_name = f"dx{str(dx).replace('.', 'p')}"
            for variant in variants:
                suffix = "staircase" if variant == "staircase" else variant
                case_name = f"{prefix}_{suffix}_{dx_name}"
                cases.append(
                    CaseSpec(
                        f"c{next_id:02d}_{case_name}",
                        "I6",
                        f"{kind.title()} {variant} refinement at {dx:g} m",
                        kind,
                        "drypoints" if variant == "staircase" else variant,
                        dx,
                    )
                )
                next_id += 1
    return cases


def _ceil_to_grid(value: float, dx: float) -> float:
    return math.ceil(value / dx - 1.0e-12) * dx


def _floor_to_grid(value: float, dx: float) -> float:
    return math.floor(value / dx + 1.0e-12) * dx


def _polygon_area(polygon: list[tuple[float, float]]) -> float:
    return 0.5 * sum(
        polygon[i][0] * polygon[(i + 1) % len(polygon)][1]
        - polygon[(i + 1) % len(polygon)][0] * polygon[i][1]
        for i in range(len(polygon))
    )


def _validate_polygon(polygon: list[tuple[float, float]], label: str) -> None:
    if len(polygon) < 3:
        raise ValueError(f"{label} has fewer than three vertices")
    if len(polygon) > 100:
        raise ValueError(f"{label} has {len(polygon)} vertices; Cutcelllist polygons are limited to 100")
    if not all(math.isfinite(coord) for point in polygon for coord in point):
        raise ValueError(f"{label} contains a non-finite coordinate")
    if abs(_polygon_area(polygon)) < 1.0e-10:
        raise ValueError(f"{label} has zero area")
    if _polygon_area(polygon) < 0:
        polygon.reverse()


def _rectangle(xmin: float, xmax: float, ymin: float, ymax: float) -> list[tuple[float, float]]:
    return [(xmin, ymin), (xmax, ymin), (xmax, ymax), (xmin, ymax)]


def _cross(a: tuple[float, float], b: tuple[float, float], p: tuple[float, float]) -> float:
    return (b[0] - a[0]) * (p[1] - a[1]) - (b[1] - a[1]) * (p[0] - a[0])


def _clip_right_of_edge(
    polygon: list[tuple[float, float]], a: tuple[float, float], b: tuple[float, float]
) -> list[tuple[float, float]]:
    """Clip a CCW polygon to the half-plane right of the directed edge a->b."""
    clipped: list[tuple[float, float]] = []
    for index, end in enumerate(polygon):
        start = polygon[index - 1]
        start_value = _cross(a, b, start)
        end_value = _cross(a, b, end)
        start_inside = start_value <= 1.0e-9
        end_inside = end_value <= 1.0e-9

        if start_inside != end_inside:
            denominator = start_value - end_value
            if abs(denominator) > 1.0e-15:
                fraction = start_value / denominator
                clipped.append(
                    (
                        start[0] + fraction * (end[0] - start[0]),
                        start[1] + fraction * (end[1] - start[1]),
                    )
                )
        if end_inside:
            clipped.append(end)

    return _remove_duplicate_vertices(clipped)


def _remove_duplicate_vertices(polygon: list[tuple[float, float]]) -> list[tuple[float, float]]:
    result: list[tuple[float, float]] = []
    for point in polygon:
        if not result or math.dist(result[-1], point) > 1.0e-9:
            result.append(point)
    if len(result) > 1 and math.dist(result[0], result[-1]) <= 1.0e-9:
        result.pop()
    return result


def _outside_polygons(
    fluid_polygon: list[tuple[float, float]], bounds: tuple[float, float, float, float]
) -> list[list[tuple[float, float]]]:
    xmin, xmax, ymin, ymax = bounds
    domain = _rectangle(xmin, xmax, ymin, ymax)
    return [
        _clip_right_of_edge(domain, fluid_polygon[i], fluid_polygon[(i + 1) % len(fluid_polygon)])
        for i in range(len(fluid_polygon))
    ]


def _annulus_parameters(dx: float) -> tuple[float, float]:
    rout = ANNULUS_INNER_RADIUS + ANNULUS_WIDTH
    radii = np.linspace(ANNULUS_INNER_RADIUS, rout, max(5001, int(20000 / dx)))
    exponent = (ANNULUS_CHEZY**2 / (GRAVITY * radii)) * ANNULUS_DZ_DTHETA
    integral = float(np.trapz(np.exp(1.5 * exponent) / np.sqrt(radii), radii))
    depth_scale = (
        ANNULUS_DISCHARGE / (ANNULUS_CHEZY * math.sqrt(-ANNULUS_DZ_DTHETA) * integral)
    ) ** (2.0 / 3.0)
    return rout, depth_scale


def _annulus_outer_polygons(
    center: tuple[float, float], radius: float, half_box: float, dx: float
) -> list[list[tuple[float, float]]]:
    polygons = []

    def circle_point(theta: float) -> tuple[float, float]:
        return (center[0] + radius * math.cos(theta), center[1] + radius * math.sin(theta))

    def square_point(theta: float) -> tuple[float, float]:
        direction_x = math.cos(theta)
        direction_y = math.sin(theta)
        scale = half_box / max(abs(direction_x), abs(direction_y))
        return (center[0] + scale * direction_x, center[1] + scale * direction_y)

    corners = [
        (center[0] + half_box, center[1] + half_box, math.pi / 4.0),
        (center[0] - half_box, center[1] + half_box, 3.0 * math.pi / 4.0),
        (center[0] - half_box, center[1] - half_box, 5.0 * math.pi / 4.0),
        (center[0] + half_box, center[1] - half_box, 7.0 * math.pi / 4.0),
    ]
    for sector in range(8):
        theta1 = sector * math.pi / 4.0
        theta2 = (sector + 1) * math.pi / 4.0
        segments = max(2, math.ceil(radius * (theta2 - theta1) / dx))
        inner_arc = [circle_point(theta) for theta in np.linspace(theta2, theta1, segments + 1)]
        outer_path = [square_point(theta1)]
        outer_path.extend(
            (x, y)
            for x, y, angle in corners
            if theta1 + 1.0e-10 < angle < theta2 - 1.0e-10
        )
        outer_path.append(square_point(theta2))
        polygon = _remove_duplicate_vertices(inner_arc + outer_path)
        _validate_polygon(polygon, f"annulus outer sector {sector + 1}")
        polygons.append(polygon)
    return polygons


def _annulus_inner_polygons(
    center: tuple[float, float], radius: float, dx: float
) -> list[list[tuple[float, float]]]:
    polygons = []
    for sector in range(8):
        theta1 = sector * math.pi / 4.0
        theta2 = (sector + 1) * math.pi / 4.0
        segments = max(2, math.ceil(radius * (theta2 - theta1) / dx))
        arc = [
            (center[0] + radius * math.cos(theta), center[1] + radius * math.sin(theta))
            for theta in np.linspace(theta1, theta2, segments + 1)
        ]
        polygon = [center, *arc]
        _validate_polygon(polygon, f"annulus inner sector {sector + 1}")
        polygons.append(polygon)
    return polygons


def _annulus_gap_polygons(
    center: tuple[float, float], inner_radius: float, outer_radius: float, dx: float
) -> list[list[tuple[float, float]]]:
    polygons = []
    for sector, (theta1, theta2) in enumerate(((math.pi / 4.0, math.pi / 2.0), (math.pi / 2.0, 3.0 * math.pi / 4.0)), 1):
        outer_segments = max(2, math.ceil(outer_radius * (theta2 - theta1) / dx))
        inner_segments = max(2, math.ceil(inner_radius * (theta2 - theta1) / dx))
        outer_arc = [
            (center[0] + outer_radius * math.cos(theta), center[1] + outer_radius * math.sin(theta))
            for theta in np.linspace(theta1, theta2, outer_segments + 1)
        ]
        inner_arc = [
            (center[0] + inner_radius * math.cos(theta), center[1] + inner_radius * math.sin(theta))
            for theta in np.linspace(theta2, theta1, inner_segments + 1)
        ]
        polygon = _remove_duplicate_vertices(outer_arc + inner_arc)
        _validate_polygon(polygon, f"annulus north-gap sector {sector}")
        polygons.append(polygon)
    return polygons


def _point_in_polygon(
    x: np.ndarray, y: np.ndarray, polygon: list[tuple[float, float]]
) -> np.ndarray:
    inside = np.zeros(x.shape, dtype=bool)
    for index, (x1, y1) in enumerate(polygon):
        x2, y2 = polygon[(index + 1) % len(polygon)]
        crosses = (y1 > y) != (y2 > y)
        x_intersection = (x2 - x1) * (y - y1) / (y2 - y1 + np.finfo(float).tiny) + x1
        inside ^= crosses & (x < x_intersection)
    return inside


def _geometry(case: CaseSpec) -> tuple[tuple[float, float, float, float], list[list[tuple[float, float]]], dict[str, Any]]:
    if case.kind == "channel":
        aligned = case.case_id.startswith("c01_") or case.case_id.startswith("c02_")
        beta = 0.0 if aligned else PLAN_BETA
        half_width_y = 225.0 if aligned else 250.0
        center_y = 1000.0 if aligned else 310.0
        length = 3000.0
        xmax = length
        ymax = 2000.0 if aligned else _ceil_to_grid(
            center_y + length * math.tan(beta) + half_width_y + 2.0 * case.dx, case.dx
        )
        bounds = (0.0, xmax, 0.0, ymax)
        y_center_end = center_y + length * math.tan(beta)
        lower = [(0.0, center_y - half_width_y), (xmax, y_center_end - half_width_y)]
        upper = [(0.0, center_y + half_width_y), (xmax, y_center_end + half_width_y)]
        polygons = [
            [(0.0, 0.0), (xmax, 0.0), lower[1], lower[0]],
            [upper[0], upper[1], (xmax, ymax), (0.0, ymax)],
        ]
        centerline = lambda x: center_y + x * math.tan(beta)
        half_width_normal = half_width_y * math.cos(beta)
        cross_x = 1500.0
        cross_y = centerline(cross_x)
        normal = (-math.sin(beta), math.cos(beta))
        cross_section = [
            (cross_x - half_width_normal * normal[0], cross_y - half_width_normal * normal[1]),
            (cross_x + half_width_normal * normal[0], cross_y + half_width_normal * normal[1]),
        ]
        inflow = {
            "name": "inflow",
            "quantity": "dischargebnd",
            "point_type": "Q",
            "points": [(0.0, 0.0), (0.0, ymax)],
            "values": [CHANNEL_QS * (2.0 * half_width_normal)],
        }
        outflow_levels = [
            -CHANNEL_SLOPE * (xmax * math.cos(beta) + y * math.sin(beta)) + CHANNEL_DEPTH for y in (0.0, ymax)
        ]
        outflow = {
            "name": "outflow",
            "quantity": "waterlevelbnd",
            "point_type": "Z",
            "points": [(xmax, 0.0), (xmax, ymax)],
            "values": outflow_levels,
        }
        observations = [
            (float(x), centerline(float(x)), f"center_{int(x):04d}")
            for x in np.arange(250.0, length, 250.0)
        ]
        observations.extend(
            [
                (cross_x, center_y + cross_x * math.tan(beta) - half_width_y + case.dx / 2.0, "bank_lower_near"),
                (cross_x, center_y + cross_x * math.tan(beta) + half_width_y - case.dx / 2.0, "bank_upper_near"),
            ]
        )
        metadata = {
            "kind": "channel",
            "aligned": aligned,
            "beta_rad": beta,
            "beta_deg": math.degrees(beta),
            "length_m": length,
            "centerline_y0_m": center_y,
            "half_width_y_m": half_width_y,
            "width_normal_m": 2.0 * half_width_normal,
            "discharge_m3_s": CHANNEL_QS * 2.0 * half_width_normal,
            "unit_discharge_m2_s": CHANNEL_QS,
            "depth_m": CHANNEL_DEPTH,
            "chezy_m_half_s": CHANNEL_CHEZY,
            "bed_slope": CHANNEL_SLOPE,
            "bed": {
                "type": "linear_channel",
                "slope": CHANNEL_SLOPE,
                "beta_rad": beta,
                "reference_elevation_m": 0.0,
            },
            "initial_water_level": "bed_plus_depth",
            "boundaries": [inflow, outflow],
            "observations": observations,
            "cross_sections": [{"name": "mid_channel", "points": cross_section}],
            "fluid_polygon": None,
        }
        return bounds, polygons, metadata

    if case.kind == "annulus":
        inner = ANNULUS_INNER_RADIUS
        outer, depth_scale = _annulus_parameters(case.dx)
        center = (0.0, 0.0)
        half_box = _ceil_to_grid(outer + 6.0 * case.dx, case.dx)
        bounds = (-half_box, half_box, -half_box, half_box)
        polygons = (
            _annulus_outer_polygons(center, outer, half_box, case.dx)
            + _annulus_inner_polygons(center, inner, case.dx)
            + _annulus_gap_polygons(center, inner, outer, case.dx)
        )
        inflow_theta = math.pi / 4.0
        outflow_theta = 3.0 * math.pi / 4.0

        def radial_points(theta: float) -> list[tuple[float, float]]:
            return [
                (radius * math.cos(theta), radius * math.sin(theta))
                for radius in (inner, outer)
            ]

        def bed_at(theta: float) -> float:
            theta_clockwise = (inflow_theta - theta) % (2.0 * math.pi)
            return 1.0 - depth_scale + ANNULUS_DZ_DTHETA * theta_clockwise

        exponent = ANNULUS_CHEZY**2 * ANNULUS_DZ_DTHETA / (GRAVITY * np.asarray([inner, outer]))
        outflow_levels = [
            bed_at(outflow_theta) + depth_scale * math.exp(float(exponent[index]))
            for index in range(2)
        ]
        boundaries = [
            {
                "name": "inflow",
                "quantity": "dischargebnd",
                "point_type": "Q",
                "points": radial_points(inflow_theta),
                "values": [ANNULUS_DISCHARGE],
            },
            {
                "name": "outflow",
                "quantity": "waterlevelbnd",
                "point_type": "Z",
                "points": radial_points(outflow_theta),
                "values": outflow_levels,
            },
        ]
        mid_radius = 0.5 * (inner + outer)
        cross_theta = 3.0 * math.pi / 2.0
        cross_section = [
            (radius * math.cos(cross_theta), radius * math.sin(cross_theta))
            for radius in (inner, outer)
        ]
        observations = [
            (
                mid_radius * math.cos(theta),
                mid_radius * math.sin(theta),
                f"annulus_{index:02d}",
            )
            for index, theta in enumerate(np.linspace(3.0 * math.pi / 4.0, 9.0 * math.pi / 4.0, 13))
        ]
        metadata = {
            "kind": "annulus",
            "center_m": list(center),
            "inner_radius_m": inner,
            "outer_radius_m": outer,
            "channel_width_m": ANNULUS_WIDTH,
            "chezy_m_half_s": ANNULUS_CHEZY,
            "bed_slope": ANNULUS_SLOPE,
            "dz0_dtheta_m": ANNULUS_DZ_DTHETA,
            "discharge_m3_s": ANNULUS_DISCHARGE,
            "depth_scale_m": depth_scale,
            "inflow_angle_deg": 45.0,
            "outflow_angle_deg": 135.0,
            "open_gap_deg": 90.0,
            "bed": {
                "type": "annular",
                "center_m": list(center),
                "depth_scale_m": depth_scale,
                "dz0_dtheta_m": ANNULUS_DZ_DTHETA,
                "inflow_angle_rad": inflow_theta,
            },
            "initial_water_level_m": 1.0,
            "boundaries": boundaries,
            "observations": observations,
            "cross_sections": [{"name": "annulus_south", "points": cross_section}],
            "fluid_polygon": None,
        }
        return bounds, polygons, metadata

    if case.kind in {"wind", "lake"}:
        aligned = case.kind == "wind" and case.case_id.startswith("c11_")
        beta = 0.0 if aligned else PLAN_BETA
        center = (1000.0, 1000.0)
        length = 2000.0
        width = 1000.0
        axis = (math.cos(beta), math.sin(beta))
        normal = (-math.sin(beta), math.cos(beta))
        fluid_polygon = [
            (
                center[0] - 0.5 * length * axis[0] - 0.5 * width * normal[0],
                center[1] - 0.5 * length * axis[1] - 0.5 * width * normal[1],
            ),
            (
                center[0] + 0.5 * length * axis[0] - 0.5 * width * normal[0],
                center[1] + 0.5 * length * axis[1] - 0.5 * width * normal[1],
            ),
            (
                center[0] + 0.5 * length * axis[0] + 0.5 * width * normal[0],
                center[1] + 0.5 * length * axis[1] + 0.5 * width * normal[1],
            ),
            (
                center[0] - 0.5 * length * axis[0] + 0.5 * width * normal[0],
                center[1] - 0.5 * length * axis[1] + 0.5 * width * normal[1],
            ),
        ]
        xmin = _floor_to_grid(min(point[0] for point in fluid_polygon) - 2.0 * case.dx, case.dx)
        xmax = _ceil_to_grid(max(point[0] for point in fluid_polygon) + 2.0 * case.dx, case.dx)
        ymin = _floor_to_grid(min(point[1] for point in fluid_polygon) - 2.0 * case.dx, case.dx)
        ymax = _ceil_to_grid(max(point[1] for point in fluid_polygon) + 2.0 * case.dx, case.dx)
        bounds = (xmin, xmax, ymin, ymax)
        polygons = _outside_polygons(fluid_polygon, bounds)
        if case.kind == "lake":
            bed = {
                "type": "lake_plane",
                "center_m": list(center),
                "slope_x": -0.0002,
                "slope_y": -0.0001,
                "bank_top_m": 10.0,
            }
            initial_water_level = 1.0
            observations = [(center[0], center[1], "basin_center")]
        else:
            bed = {"type": "flat", "elevation_m": 0.0}
            initial_water_level = 2.0
            observations = [(center[0], center[1], "basin_center")]
        metadata = {
            "kind": case.kind,
            "aligned": aligned,
            "beta_rad": beta,
            "beta_deg": math.degrees(beta),
            "center_m": list(center),
            "length_m": length,
            "width_m": width,
            "fluid_polygon": fluid_polygon,
            "bed": bed,
            "initial_water_level_m": initial_water_level,
            "observations": observations,
            "cross_sections": [],
            "boundaries": [],
        }
        if case.kind == "wind":
            metadata["wind_speed_m_s"] = WIND_SPEED
            metadata["wind_direction_deg"] = 0.0
            metadata["wind_drag_coefficient"] = WIND_DRAG
            metadata["air_density_kg_m3"] = AIR_DENSITY
            metadata["water_density_kg_m3"] = WATER_DENSITY
        return bounds, polygons, metadata

    raise ValueError(f"Unsupported case kind: {case.kind}")


def _bed_values(metadata: dict[str, Any], x: np.ndarray, y: np.ndarray) -> np.ndarray:
    bed = metadata["bed"]
    if bed["type"] == "linear_channel":
        beta = bed["beta_rad"]
        return -bed["slope"] * (x * math.cos(beta) + y * math.sin(beta))
    if bed["type"] == "annular":
        center_x, center_y = bed["center_m"]
        theta = np.arctan2(y - center_y, x - center_x)
        theta_clockwise = (bed["inflow_angle_rad"] - theta) % (2.0 * math.pi)
        return (
            1.0
            - bed["depth_scale_m"]
            + bed["dz0_dtheta_m"] * theta_clockwise
        )
    if bed["type"] == "lake_plane":
        center_x, center_y = bed["center_m"]
        plane = bed["slope_x"] * (x - center_x) + bed["slope_y"] * (y - center_y)
        wet = _point_in_polygon(x, y, metadata["fluid_polygon"])
        return np.where(wet, plane, bed["bank_top_m"])
    return np.full(x.shape, bed["elevation_m"], dtype=float)


def _grid(bounds: tuple[float, float, float, float], dx: float) -> dict[str, Any]:
    xmin, xmax, ymin, ymax = bounds
    nx_float = (xmax - xmin) / dx
    ny_float = (ymax - ymin) / dx
    nx = round(nx_float)
    ny = round(ny_float)
    if abs(nx_float - nx) > 1.0e-9 or abs(ny_float - ny) > 1.0e-9:
        raise ValueError(f"Grid bounds {bounds} are not aligned to dx={dx}")
    x_nodes_1d = xmin + np.arange(nx + 1, dtype=float) * dx
    y_nodes_1d = ymin + np.arange(ny + 1, dtype=float) * dx
    node_x, node_y = np.meshgrid(x_nodes_1d, y_nodes_1d)
    face_x, face_y = np.meshgrid(x_nodes_1d[:-1] + dx / 2.0, y_nodes_1d[:-1] + dx / 2.0)
    face_nodes = []
    for row in range(ny):
        row_start = row * (nx + 1)
        for column in range(nx):
            lower_left = row_start + column + 1
            lower_right = lower_left + 1
            upper_left = lower_left + nx + 1
            upper_right = upper_left + 1
            face_nodes.append([lower_left, lower_right, upper_right, upper_left])

    edge_faces: dict[tuple[int, int], list[int]] = {}
    for face_index, face in enumerate(face_nodes, 1):
        for index in range(4):
            edge = tuple(sorted((face[index], face[(index + 1) % 4])))
            edge_faces.setdefault(edge, []).append(face_index)
    edge_nodes = list(edge_faces)
    face_to_node_x = node_x.ravel()[np.asarray(face_nodes, dtype=int) - 1]
    face_to_node_y = node_y.ravel()[np.asarray(face_nodes, dtype=int) - 1]
    face_x_flat = face_x.ravel()
    face_y_flat = face_y.ravel()

    return {
        "nx": nx,
        "ny": ny,
        "node_x": node_x.ravel(),
        "node_y": node_y.ravel(),
        "face_x": face_x_flat,
        "face_y": face_y_flat,
        "face_nodes": np.asarray(face_nodes, dtype=np.int32),
        "face_x_bounds": face_to_node_x,
        "face_y_bounds": face_to_node_y,
        "edge_nodes": np.asarray(edge_nodes, dtype=np.int32),
        "edge_faces": edge_faces,
    }


def _create_variable(
    dataset: Dataset,
    name: str,
    dimensions: tuple[str, ...],
    dtype: str,
    fill_value: int | float | None = None,
    **attributes: Any,
):
    kwargs = {} if fill_value is None else {"fill_value": fill_value}
    variable = dataset.createVariable(name, dtype, dimensions, **kwargs)
    for key, value in attributes.items():
        setattr(variable, key, value)
    return variable


def _write_mesh(path: Path, grid: dict[str, Any], force: bool) -> None:
    if path.exists() and not force:
        raise FileExistsError(f"{path} exists; rerun with --force to regenerate")
    if path.exists():
        path.unlink()

    n_nodes = len(grid["node_x"])
    n_faces = len(grid["face_nodes"])
    n_edges = len(grid["edge_nodes"])
    dataset = Dataset(path, "w", format="NETCDF4")
    try:
        dataset.Conventions = "CF-1.8 UGRID-1.0"
        dataset.title = "Generated Cartesian immersed-boundary verification mesh"
        dataset.createDimension("Two", 2)
        dataset.createDimension("nmesh2d_node", n_nodes)
        dataset.createDimension("nmesh2d_edge", n_edges)
        dataset.createDimension("nmesh2d_face", n_faces)
        dataset.createDimension("max_nmesh2d_face_nodes", 4)

        topology = _create_variable(dataset, "mesh2d", (), "i4")
        topology[...] = 0
        topology.cf_role = "mesh_topology"
        topology.long_name = "Topology data of 2D network"
        topology.topology_dimension = 2
        topology.node_coordinates = "mesh2d_node_x mesh2d_node_y"
        topology.node_dimension = "nmesh2d_node"
        topology.max_face_nodes_dimension = "max_nmesh2d_face_nodes"
        topology.edge_node_connectivity = "mesh2d_edge_nodes"
        topology.edge_dimension = "nmesh2d_edge"
        topology.edge_coordinates = "mesh2d_edge_x mesh2d_edge_y"
        topology.face_node_connectivity = "mesh2d_face_nodes"
        topology.face_dimension = "nmesh2d_face"
        topology.face_coordinates = "mesh2d_face_x mesh2d_face_y"

        crs = _create_variable(dataset, "wgs84", (), "i4")
        crs[...] = 0
        crs.setncattr("name", "wgs84")
        crs.epsg = 28992
        crs.grid_mapping_name = "Unknown projected"
        crs.longitude_of_prime_meridian = 0.0
        crs.semi_major_axis = 6378137.0
        crs.semi_minor_axis = 6356752.314245
        crs.inverse_flattening = 298.257223563
        crs.EPSG_code = "EPSG:28992"
        crs.value = "value is equal to EPSG code"

        node_x = _create_variable(
            dataset,
            "mesh2d_node_x",
            ("nmesh2d_node",),
            "f8",
            units="m",
            standard_name="projection_x_coordinate",
            long_name="x-coordinate of mesh nodes",
            mesh="mesh2d",
            location="node",
        )
        node_y = _create_variable(
            dataset,
            "mesh2d_node_y",
            ("nmesh2d_node",),
            "f8",
            units="m",
            standard_name="projection_y_coordinate",
            long_name="y-coordinate of mesh nodes",
            mesh="mesh2d",
            location="node",
        )
        node_x[:] = grid["node_x"]
        node_y[:] = grid["node_y"]

        node_z = _create_variable(
            dataset,
            "mesh2d_node_z",
            ("nmesh2d_node",),
            "f8",
            fill_value=FILL_FLOAT,
            mesh="mesh2d",
            location="node",
            coordinates="mesh2d_node_x mesh2d_node_y",
            standard_name="altitude",
            long_name="z-coordinate of mesh nodes",
            units="m",
            grid_mapping="wgs84",
        )
        node_z[:] = FILL_FLOAT

        face_nodes = _create_variable(
            dataset,
            "mesh2d_face_nodes",
            ("nmesh2d_face", "max_nmesh2d_face_nodes"),
            "i4",
            fill_value=FILL_INT,
            cf_role="face_node_connectivity",
            mesh="mesh2d",
            location="face",
            long_name="Mapping from every face to its corner nodes (counterclockwise)",
            start_index=1,
        )
        face_nodes[:] = grid["face_nodes"]

        edge_nodes = _create_variable(
            dataset,
            "mesh2d_edge_nodes",
            ("nmesh2d_edge", "Two"),
            "i4",
            fill_value=FILL_INT,
            cf_role="edge_node_connectivity",
            mesh="mesh2d",
            location="edge",
            long_name="Mapping from every edge to the two nodes that it connects",
            start_index=1,
        )
        edge_nodes[:] = grid["edge_nodes"]

        edge_faces_array = np.full((n_edges, 2), FILL_INT, dtype=np.int32)
        for edge_index, edge in enumerate(grid["edge_nodes"]):
            adjacent = grid["edge_faces"][tuple(int(node) for node in edge)]
            edge_faces_array[edge_index, : len(adjacent)] = adjacent[:2]
        edge_faces = _create_variable(
            dataset,
            "mesh2d_edge_faces",
            ("nmesh2d_edge", "Two"),
            "i4",
            fill_value=FILL_INT,
            cf_role="edge_face_connectivity",
            long_name="Mapping from every edge to the faces that it separates",
            start_index=1,
        )
        edge_faces[:] = edge_faces_array

        edge_node_indices = grid["edge_nodes"] - 1
        edge_x_bounds = grid["node_x"][edge_node_indices]
        edge_y_bounds = grid["node_y"][edge_node_indices]
        for name, values, long_name, standard_name in (
            ("mesh2d_edge_x", edge_x_bounds.mean(axis=1), "characteristic x-coordinate of mesh edge", "projection_x_coordinate"),
            ("mesh2d_edge_y", edge_y_bounds.mean(axis=1), "characteristic y-coordinate of mesh edge", "projection_y_coordinate"),
        ):
            variable = _create_variable(
                dataset,
                name,
                ("nmesh2d_edge",),
                "f8",
                units="m",
                standard_name=standard_name,
                long_name=long_name,
                mesh="mesh2d",
                location="edge",
                bounds=f"{name}_bnd",
            )
            variable[:] = values
        for name, values, long_name, standard_name in (
            ("mesh2d_edge_x_bnd", edge_x_bounds, "x-coordinate bounds of 2D mesh edge", "projection_x_coordinate"),
            ("mesh2d_edge_y_bnd", edge_y_bounds, "y-coordinate bounds of 2D mesh edge", "projection_y_coordinate"),
        ):
            variable = _create_variable(
                dataset,
                name,
                ("nmesh2d_edge", "Two"),
                "f8",
                fill_value=FILL_FLOAT,
                units="m",
                standard_name=standard_name,
                long_name=long_name,
                mesh="mesh2d",
                location="edge",
            )
            variable[:] = values

        for name, values, long_name, standard_name in (
            ("mesh2d_face_x", grid["face_x"], "Characteristic x-coordinate of mesh face", "projection_x_coordinate"),
            ("mesh2d_face_y", grid["face_y"], "Characteristic y-coordinate of mesh face", "projection_y_coordinate"),
        ):
            variable = _create_variable(
                dataset,
                name,
                ("nmesh2d_face",),
                "f8",
                units="m",
                standard_name=standard_name,
                long_name=long_name,
                mesh="mesh2d",
                location="face",
                bounds=f"{name}_bnd",
            )
            variable[:] = values
        for name, values, long_name, standard_name in (
            ("mesh2d_face_x_bnd", grid["face_x_bounds"], "x-coordinate bounds of 2D mesh face", "projection_x_coordinate"),
            ("mesh2d_face_y_bnd", grid["face_y_bounds"], "y-coordinate bounds of 2D mesh face", "projection_y_coordinate"),
        ):
            variable = _create_variable(
                dataset,
                name,
                ("nmesh2d_face", "max_nmesh2d_face_nodes"),
                "f8",
                fill_value=FILL_FLOAT,
                units="m",
                standard_name=standard_name,
                long_name=long_name,
                mesh="mesh2d",
                location="face",
            )
            variable[:] = values
    finally:
        dataset.close()


def _serialize_polygons(polygons: list[list[tuple[float, float]]]) -> str:
    lines: list[str] = []
    for index, polygon in enumerate(polygons, 1):
        _validate_polygon(polygon, f"solid polygon {index}")
        lines.extend([f"solid_{index:03d}", f"{len(polygon)} 3"])
        lines.extend(f"{x:.10f} {y:.10f} 1.0" for x, y in polygon)
    return "\n".join(lines) + "\n"


def _write_text(path: Path, text: str, force: bool) -> None:
    if path.exists():
        current = path.read_text(encoding="utf-8")
        if current == text:
            return
        if not force:
            raise FileExistsError(f"{path} differs from generated content; rerun with --force to overwrite")
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8", newline="\n")


def _write_samples(path: Path, x: np.ndarray, y: np.ndarray, values: np.ndarray, force: bool) -> None:
    if path.exists() and not force:
        raise FileExistsError(f"{path} exists; rerun with --force to regenerate")
    if path.exists():
        path.unlink()
    data = np.column_stack((x, y, values))
    with path.open("w", encoding="utf-8", newline="\n") as stream:
        np.savetxt(stream, data, fmt=("%.8f", "%.8f", "%.12g"))


def _write_boundary_pli(path: Path, name: str, points: list[tuple[float, float]], point_type: str, force: bool) -> None:
    lines = [name, f"{len(points):6d} 2"]
    for index, (x, y) in enumerate(points, 1):
        lines.append(f"{x:.10f} {y:.10f} {point_type} T {name}_point{index}")
    _write_text(path, "\n".join(lines) + "\n", force)


def _write_polyline_file(path: Path, sections: list[dict[str, Any]], force: bool) -> None:
    lines: list[str] = []
    for section in sections:
        points = section["points"]
        lines.extend([section["name"], f"{len(points):6d} 2"])
        lines.extend(f"{x:.10f} {y:.10f}" for x, y in points)
    _write_text(path, "\n".join(lines) + "\n" if lines else "", force)


def _time_series_block(
    name: str, quantity: str, unit: str, value: float, end_minutes: float
) -> list[str]:
    return [
        "[Forcing]",
        f"name              = {name}",
        "function          = timeseries",
        "timeInterpolation = linear",
        "offset            = 0.0",
        "factor            = 1.0",
        "quantity          = time",
        "unit              = MINUTES SINCE 2000-01-01 00:00:00",
        f"quantity          = {quantity}",
        f"unit              = {unit}",
        f"0.0 {value:.12g}",
        f"{end_minutes:.12g} {value:.12g}",
        "",
    ]


def _write_boundary_files(input_dir: Path, boundary: dict[str, Any], duration_hours: float, force: bool) -> None:
    name = boundary["name"]
    _write_boundary_pli(
        input_dir / f"{name}.pli",
        name,
        boundary["points"],
        boundary["point_type"],
        force,
    )
    end_minutes = duration_hours * 60.0
    quantity = boundary["quantity"]
    unit = "m3/s" if quantity == "dischargebnd" else "m"
    values = boundary["values"]
    if quantity == "dischargebnd":
        forcing_names = [f"{name}_0001"]
    else:
        forcing_names = [f"{name}_{index:04d}" for index in range(1, len(values) + 1)]
    blocks = [
        _time_series_block(forcing_name, quantity, unit, value, end_minutes)
        for forcing_name, value in zip(forcing_names, values, strict=True)
    ]
    text = "\n".join(
        [
            "# generated by f600_ibm/doc/generate_cases.py",
            "[General]",
            "fileVersion = 1.01",
            "fileType    = boundConds",
            "",
            *[line for block in blocks for line in block],
        ]
    )
    _write_text(input_dir / f"{name}.bc", text, force)


def _write_external_forcing(input_dir: Path, metadata: dict[str, Any], case: CaseSpec, force: bool) -> None:
    lines = [
        "# generated by f600_ibm/doc/generate_cases.py",
        "[General]",
        "fileVersion = 2.01",
        "fileType    = extForce",
        "",
        "[Initial]",
        "quantity             = bedlevel",
        "forcingFile          = bedlevel.xyz",
        "forcingFileType      = sample",
        "interpolationMethod  = triangulation",
        "locationType         = 2d",
        "",
        "[Initial]",
        "quantity             = initialWaterLevel",
        "forcingFile          = initial_waterlevel.xyz",
        "forcingFileType      = sample",
        "interpolationMethod  = triangulation",
        "locationType         = 2d",
        "",
    ]
    for boundary in metadata["boundaries"]:
        lines.extend(
            [
                "[Boundary]",
                f"quantity     = {boundary['quantity']}",
                f"locationFile = {boundary['name']}.pli",
                f"forcingFile  = {boundary['name']}.bc",
                "",
            ]
        )
    if case.kind == "wind":
        for quantity in ("windx", "windy"):
            lines.extend(
                [
                    "[Meteo]",
                    f"quantity             = {quantity}",
                    f"forcingFile          = {quantity}.wnd",
                    "forcingFileType      = uniform",
                    "interpolationMethod  = linearSpaceTime",
                    "operand              = O",
                    "extrapolationAllowed = 0",
                    "",
                ]
            )
            value = WIND_SPEED if quantity == "windx" else 0.0
            end_minutes = case.duration_hours * 60.0
            _write_text(
                input_dir / f"{quantity}.wnd",
                f"0.0 {value:.12g}\n{end_minutes:.12g} {value:.12g}\n",
                force,
            )
    _write_text(input_dir / "forcing.ext", "\n".join(lines), force)


def _mdu_text(case: CaseSpec, metadata: dict[str, Any]) -> str:
    duration_seconds = case.duration_hours * 3600.0
    map_interval = min(3600.0, duration_seconds)
    history_interval = min(600.0, duration_seconds)
    lines = [
        "[model]",
        "Program = D-Flow FM",
        "AutoStart = 0",
        "",
        "[geometry]",
        "NetFile = mesh2d_net.nc",
        "BedlevType = 1",
        "Jsferic = 0",
        "Kmx = 0",
        "Bamin = 1e-6",
    ]
    if case.variant == "drypoints":
        lines.append("DryPointsFile = solids.pol")
    else:
        lines.append("Cutcelllist = solid.lst")

    lines.extend(
        [
            "",
            "[numerics]",
            "CFLMax = 0.7",
            "AdvecType = 33",
            "TimeStepType = 2",
            "Teta0 = 0.55",
            f"Icgsolver = {6 if case.parallel else 4}",
        ]
    )
    if case.variant in {"cutcell", "ibm"}:
        lines.append("Baorgfracmin = 0.1")
    if case.uses_ibm:
        lines.extend(
            [
                "ImmersedBoundary = 1",
                "ImmersedBoundaryMinAreaFrac = 0.1",
                f"ImmersedBoundaryTangentialCorrection = {case.tangential_correction}",
            ]
        )

    chezy = ANNULUS_CHEZY if case.kind == "annulus" else CHANNEL_CHEZY
    lines.extend(
        [
            "",
            "[physics]",
            "UnifFrictType = 0",
            f"UnifFrictCoef = {chezy:.12g}",
            "Vicouv = 0.0",
            "Smagorinsky = 0.0",
            "Irov = 0",
            f"Rhomean = {WATER_DENSITY:.12g}",
            "",
            "[time]",
            "RefDate = 20000101",
            "Tzone = 0",
            "Tunit = H",
            "TStart = 0",
            f"TStop = {case.duration_hours:.12g}",
            "DtUser = 60",
            "DtMax = 60",
            "DtInit = 1",
            "",
            "[external forcing]",
            "ExtForceFileNew = forcing.ext",
            "",
            "[output]",
            "OutputDir = dflowfmoutput",
            f"MapFile = {case.case_id}_map.nc",
            f"HisFile = {case.case_id}_his.nc",
            f"MapInterval = {map_interval:.12g} 0 {duration_seconds:.12g}",
            f"HisInterval = {history_interval:.12g} 0 {duration_seconds:.12g}",
            "MapFormat = 4",
            "Wrihis_balance = 1",
            "Wrihis_waterlevel_s1 = 1",
            "Wrimap_waterlevel_s1 = 1",
            "Wrimap_velocity_vector = 1",
            "Wrimap_flow_flux_q1 = 1",
            "Wrimap_wind = 1",
        ]
    )
    if metadata["observations"]:
        lines.append(f"ObsFile = {case.case_id}_obs.xyn")
    if metadata["cross_sections"]:
        lines.append(f"CrsFile = {case.case_id}_crs.pli")
    if case.kind == "wind":
        lines.extend(
            [
                "",
                "[wind]",
                "ICdtyp = 1",
                f"Cdbreakpoints = {WIND_DRAG:.12g}",
                f"Rhoair = {AIR_DENSITY:.12g}",
            ]
        )
    return "\n".join(lines) + "\n"


def _dimr_xml(case: CaseSpec) -> str:
    process = "0 1" if case.parallel else "0"
    mpi = "\n        <mpiCommunicator>DFM_COMM_DFMWORLD</mpiCommunicator>" if case.parallel else ""
    return (
        '<?xml version="1.0" encoding="utf-8" standalone="yes"?>\n'
        '<dimrConfig xmlns="http://schemas.deltares.nl/dimr" '
        'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" '
        'xsi:schemaLocation="http://schemas.deltares.nl/dimr '
        'http://content.oss.deltares.nl/schemas/dimr-1.0.xsd">\n'
        "    <documentation>\n"
        "        <fileVersion>1.00</fileVersion>\n"
        "        <createdBy>f600_ibm/doc/generate_cases.py</createdBy>\n"
        "    </documentation>\n"
        "    <control>\n"
        '        <start name="DFlowFM"/>\n'
        "    </control>\n"
        '    <component name="DFlowFM">\n'
        "        <library>dflowfm</library>\n"
        f"        <process>{process}</process>{mpi}\n"
        "        <workingDir>.</workingDir>\n"
        f"        <inputFile>{case.case_id}.mdu</inputFile>\n"
        "    </component>\n"
        "</dimrConfig>\n"
    )


def _tex_escape(value: str) -> str:
    replacements = {
        "\\": r"\textbackslash{}",
        "_": r"\_",
        "%": r"\%",
        "&": r"\&",
        "#": r"\#",
        "$": r"\$",
        "{": r"\{",
        "}": r"\}",
    }
    return "".join(replacements.get(char, char) for char in value)


def _case_doc(case: CaseSpec, metadata: dict[str, Any]) -> str:
    return (
        "\\section{" + _tex_escape(case.name) + "}\n"
        "\\label{sec:" + _tex_escape(case.case_id) + "}\n\n"
        "\\begin{description}\n"
        f"\\item[Plan ID] {_tex_escape(case.plan_id)}\n"
        f"\\item[Grid spacing] {case.dx:g} m\n"
        f"\\item[Geometry] {_tex_escape(case.variant)}; "
        f"IBM {'enabled' if case.uses_ibm else 'disabled'}\n"
        f"\\item[Simulation duration] {case.duration_hours:g} h\n"
        f"\\item[Purpose] {_tex_escape(case.name)}\n"
        "\\end{description}\n\n"
        "The generated input directory contains the UGRID mesh, solid polygon geometry, "
        "sampled bed and initial water level, external forcings, observation locations, "
        "and the model configuration. Reference outputs and TestBench XML entries are "
        "added after the case passes the staged acceptance criteria.\n"
    )


def _write_case_documentation(case: CaseSpec, metadata: dict[str, Any], doc_dir: Path, force: bool) -> None:
    text = _case_doc(case, metadata)
    _write_text(
        doc_dir / "casedoc.tex",
        "\\documentclass{article}\n"
        "\\begin{document}\n"
        "\\input{chapters/case_text.tex}\n"
        "\\end{document}\n",
        force,
    )
    _write_text(doc_dir / "chapters" / "case_text.tex", text, force)


def _write_family_documentation(cases: list[CaseSpec], force: bool) -> None:
    docs_dir = FAMILY_ROOT / "doc"
    table_rows = [
        "| Case | Plan ID | Grid | Variant | Purpose |",
        "|---|---|---:|---|---|",
    ]
    tex_rows = ["\\begin{description}"]
    for case in cases:
        table_rows.append(
            f"| `{case.case_id}` | {case.plan_id} | {case.dx:g} m | {case.variant} | {case.name} |"
        )
        tex_rows.append(
            f"\\item[\\texttt{{{_tex_escape(case.case_id)}}}] "
            f"{_tex_escape(case.plan_id)}; {case.dx:g} m; "
            f"{_tex_escape(case.variant)}; {_tex_escape(case.name)}."
        )
    tex_rows.append("\\end{description}")
    readme = (
        "# D-Flow FM immersed-boundary verification cases\n\n"
        "Inputs and case documentation are generated deterministically by "
        "`doc/generate_cases.py` from the parameters in §7.2 of the implementation plan. "
        "The generator creates c01–c15 for I1–I9 and c16–c33 for the I6 resolution study.\n\n"
        "Regenerate with the TestBench virtual environment from this directory:\n\n"
        "```bash\n"
        "python generate_cases.py --force\n"
        "```\n\n"
        "The generated inputs use `bedlevel` and `initialWaterLevel` sample blocks in the "
        "new-format `.ext` file. Cut-cell polygons are split into files with no more than "
        "100 vertices each. DVC pointers cover each case's `input/` and `doc/` folders.\n\n"
        "Reference outputs and TestBench config entries are intentionally not generated: "
        "they are added only after solver runs pass the acceptance criteria. "
        "`Wrimap_immersed_boundary` is likewise deferred until the W5 map diagnostics exist.\n\n"
        + "\n".join(table_rows)
        + "\n"
    )
    functionality = (
        "\\documentclass{article}\n"
        "\\begin{document}\n"
        "\\section{D-Flow FM immersed-boundary method}\n"
        "This functionality group contains deterministic verification inputs for the "
        "Cartesian immersed-boundary method. It covers aligned and oblique channels, "
        "an annular channel, a lake at rest, and wind-driven closed basins. The input and "
        "documentation directories are versioned with DVC. Reference outputs and TestBench "
        "configuration entries are added after acceptance.\n"
        "\\input{chapters/testcases.tex}\n"
        "\\end{document}\n"
    )
    _write_text(docs_dir / "README.md", readme, force)
    _write_text(docs_dir / "functionality_report.tex", functionality, force)
    _write_text(
        docs_dir / "chapters" / "testcases.tex",
        "\n".join(tex_rows) + "\n",
        force,
    )


def _write_case(case: CaseSpec, force: bool) -> None:
    case_dir = FAMILY_ROOT / case.case_id
    input_dir = case_dir / "input"
    doc_dir = case_dir / "doc"
    input_dir.mkdir(parents=True, exist_ok=True)
    doc_dir.mkdir(parents=True, exist_ok=True)

    bounds, polygons, metadata = _geometry(case)
    for index, polygon in enumerate(polygons, 1):
        _validate_polygon(polygon, f"{case.case_id} solid polygon {index}")
    grid = _grid(bounds, case.dx)
    _write_mesh(input_dir / "mesh2d_net.nc", grid, force)

    if case.variant == "drypoints":
        _write_text(input_dir / "solids.pol", _serialize_polygons(polygons), force)
    else:
        polygon_names = []
        for index, polygon in enumerate(polygons, 1):
            polygon_name = f"solid_{index:03d}.pol"
            polygon_names.append(polygon_name)
            _write_text(input_dir / polygon_name, _serialize_polygons([polygon]), force)
        _write_text(input_dir / "solid.lst", "\n".join(polygon_names) + "\n", force)

    bed = _bed_values(metadata, grid["face_x"], grid["face_y"])
    if metadata["kind"] == "channel":
        initial = bed + CHANNEL_DEPTH
    elif metadata["kind"] == "annulus":
        initial = np.full(bed.shape, 1.0)
    else:
        initial = np.full(bed.shape, metadata["initial_water_level_m"])
    _write_samples(input_dir / "bedlevel.xyz", grid["face_x"], grid["face_y"], bed, force)
    _write_samples(input_dir / "initial_waterlevel.xyz", grid["face_x"], grid["face_y"], initial, force)

    for boundary in metadata["boundaries"]:
        _write_boundary_files(input_dir, boundary, case.duration_hours, force)
    _write_external_forcing(input_dir, metadata, case, force)
    _write_polyline_file(input_dir / f"{case.case_id}_crs.pli", metadata["cross_sections"], force)
    if metadata["observations"]:
        observation_text = "".join(
            f"{x:.10f} {y:.10f} {name}\n" for x, y, name in metadata["observations"]
        )
        _write_text(input_dir / f"{case.case_id}_obs.xyn", observation_text, force)

    metadata.update(
        {
            "case_id": case.case_id,
            "plan_id": case.plan_id,
            "name": case.name,
            "variant": case.variant,
            "dx_m": case.dx,
            "duration_hours": case.duration_hours,
            "parallel": case.parallel,
            "tangential_correction": case.tangential_correction,
            "mesh_bounds_m": list(bounds),
            "mesh_cells": [grid["nx"], grid["ny"]],
            "map_file": f"{case.case_id}_map.nc",
            "history_file": f"{case.case_id}_his.nc",
        }
    )
    _write_text(
        input_dir / "case_metadata.json",
        json.dumps(metadata, indent=2, sort_keys=True, allow_nan=False) + "\n",
        force,
    )
    _write_text(input_dir / f"{case.case_id}.mdu", _mdu_text(case, metadata), force)
    _write_text(input_dir / "dimr.xml", _dimr_xml(case), force)
    _write_case_documentation(case, metadata, doc_dir, force)


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--force",
        action="store_true",
        help="overwrite generated files without deleting unrelated files",
    )
    return parser.parse_args()


def main() -> None:
    args = _parse_args()
    cases = build_case_specs()
    _write_family_documentation(cases, args.force)
    for case in cases:
        _write_case(case, args.force)
        print(f"Generated {case.case_id}: {case.plan_id}, dx={case.dx:g} m, {case.variant}")
    print(f"Generated {len(cases)} cases under {FAMILY_ROOT}")


if __name__ == "__main__":
    main()
