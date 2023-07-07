Delft3D-FLOW: xz/yz is average of 4 depth points
DFlowFM: xz/yz is the cell circumcentre middle point
To exclude these differences:
In subroutine .../wave/.../read_netcdf_grd:
Activate the (commented out) line:
! call replacecoordinates("zxzyd3d.txt", mmax, nmax, xb, yb)

See also directory https://repos.deltares.nl/repos/DSCTestbench/cases/trunk/e26_dflowfm-wave/f02_fm_cases/test4_extrapWind/dfm/wave/zxzy_to_ccxccy
