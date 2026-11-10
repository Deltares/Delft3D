# Structure BMI fields

Structure quantities are addressed by collection and structure **ID**, for
example `generalstructures/Gate01/flow_area`, using `get_var`, or the equivalent
`get_compound_field("generalstructures", "Gate01", "flow_area", pointer)`.
Collection and field names are case-insensitive in the compound getter; IDs
retain their original case.

Existing control fields (including `crestLevel`, `gateLowerEdgeLevel`,
`gateHeight`, `gateOpeningWidth`, `capacity`, and `valveOpeningHeight`) retain
their names and field indices. New output fields are appended to the field
enumeration; `get_var_shape(collection)[1]` reports the total field count.
The new fields are read-only double-precision scalars. Through the compound
API they have rank zero; the existing slash-separated `get_var` API describes
them as one-element arrays.

| Output field | Quantity / units | Collections |
|---|---|---|
| `discharge` | Oriented discharge, m3/s | pumps, weirs, orifices, gates, generalstructures, culverts |
| `velocity` | Discharge divided by flow area, m/s | weirs, orifices, generalstructures, culverts |
| `flow_area` | Total flow area, m2 | weirs, orifices, generalstructures, culverts |
| `water_level_up` | Width-averaged water level on the structure's upstream side, m | pumps, weirs, orifices, gates, generalstructures, culverts |
| `water_level_down` | Width-averaged water level on the downstream side, m | pumps, weirs, orifices, gates, generalstructures, culverts |
| `head_difference` | Width-averaged upstream-minus-downstream water level, m | pumps, weirs, orifices, generalstructures, culverts |
| `opening_height` | Current hydraulic opening height, m (not gate door height) | gates, orifices, generalstructures, culverts |
| `force_difference` | Hydrostatic force difference **per unit width**, N/m | weirs, orifices, generalstructures |
| `water_level_at_crest` | Width-averaged water level on crest, m | weirs, orifices, generalstructures |
| `crest_level` | Culvert crest level, m | culverts |
| `gate_lower_edge_level` | Culvert gate lower-edge level, m | culverts |

`force_difference` is the existing history quantity, not pressure in Pa. It
integrates the hydrostatic loading above the crest and is not named
`pressure_difference`. Upstream/downstream refer to the structure orientation,
not necessarily the instantaneous flow direction. Pump `head_difference`
is not the separately defined delivery-minus-suction pump head.

## Availability and lifetime

The new fields point into the existing structure history arrays. They require
a positive `HisInterval` and the corresponding structure history output setting
(for example `Wrihis_structure_pump`). BMI reads do not enable history output,
allocate output arrays, recompute hydraulics, or perform MPI collectives.
Values are initialized and refreshed by the normal structure-output update
path. Missing/dry values retain the kernel's existing missing-value convention.
Unavailable reads return a null pointer and log a warning.

Legacy cgen-based weirs and general structures do not compute opening height,
force difference, or water level on crest in these arrays; those fields are not
advertised for these collections. Pump history does not calculate flow area or
velocity. The gate `opening_height` is the current hydraulic flow height from
the gate history layout. Gate history does not maintain the general structure
area, velocity, head-difference, crest water-level, or force fields, so these
are not advertised for `gates`.

Pointers remain valid while the initialized model owns the history arrays and
reflect subsequent timestep updates. Do not retain or dereference them after
finalization or model reinitialization. Existing control fields are unaffected
by the new history-output availability restriction.
