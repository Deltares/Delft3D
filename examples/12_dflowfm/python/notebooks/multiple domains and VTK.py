# -*- coding: utf-8 -*-
# <nbformat>3.0</nbformat>

# <headingcell level=3>

# Read D-FLOW FM model output from NetCDF and render using VTK

# <codecell>

import src.vtk_utils as vtk_utils

# <codecell>

grid =  vtk_utils.read_grid('../../test_data/par32/par32_dt0d6_24h_0000_map.nc')
actor =  vtk_utils.create_grid_actor(grid)

# <codecell>

vtk_utils.show(actor, zoom = 1.95, cells=False, edges=True)

# isolines=True,
# wireframe=True) 
# camera_position=grid.GetPoint(0)

# from vtk.util import numpy_support
# xyz = numpy_support.vtk_to_numpy(grid.GetPoints().GetData())
# plot(xyz)

# <codecell>

path = '../../test_data/par32/par32_dt0d6_24h_0000_map.nc'
time_step = 0

grid_water =  vtk_utils.read_water_grid_from_nc(grid, time_step, path)

vtk_utils.write_grid_to_vtk(grid_water, path + "_water_" + str(time_step) + ".vtk", "water_level")
# show(create_grid_actor(grid_water), zoom = 1.5, wireframe=True)

# <codecell>

import glob

# for f in []:
for f in glob.glob('./*.nc'):
    grid =  vtk_utils.read_grid(f)
    vtk_utils.write_grid_to_vtk(grid, f + '.vtk')

# <codecell>

pylab.rcParams['figure.figsize'] = (15.0, 8.0)
pylab.rcParams['image.aspect'] = 'equal'

files = glob.glob('../../test_data/par32/*.nc')

p = ProgressBar(len(files))

for i, f in enumerate(files):
    x, y, z, cells = read_grid_parameters_from_nc(f)
    plot(x, y, marker=',', linestyle='')
    p.animate(i)
  
# plt.xlim(10000, 40000)
# plt.ylim(0, 30000)

plt.show()

