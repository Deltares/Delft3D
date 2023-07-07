# -*- coding: utf-8 -*-
# <nbformat>3.0</nbformat>

# <markdowncell>

# Example of running Dflow FM from python
# =======================================
# 
# This example shows how to get data out of Dflow FM from python. We use the bmi library which exposes the data from a running model, we use netCDF for reading the input file and we use matplotlib for plotting the grid. 

# <codecell>

import os
import openearthtools.modelapi.bmi
import netCDF4
import matplotlib.pyplot as plt
# Locate the libdflow dll
DIRNAME = os.path.dirname('.')
LIBFMNAME = os.path.join(DIRNAME, '../../src/.libs/libdflowfm.dylib')
# This is a simulation of a river bend.
rundir = '/Users/fedorbaart/Documents/checkouts/cases_unstruc/e00_unstruc/f04_bottomfriction/c016_2DConveyance_bend/input'

# <codecell>

# Open the input file
ds = netCDF4.Dataset(os.path.join(rundir, 'bend1_net.nc'))
# Read the variables of the vertices
nx = ds.variables['NetNode_x'][:]
ny = ds.variables['NetNode_y'][:]
nz = ds.variables['NetNode_z'][:]
# The elements are created by linking the vertices together
nelemnode = ds.variables['NetElemNode'][:]
cellcoords = []
for elem in nelemnode:
    elemx = nx[elem[~elem.mask]-1]
    elemy = ny[elem[~elem.mask]-1]
    cell = np.c_[elemx, elemy]
    cellcoords.append(cell)
# We're not showing any data on faces (edges) in this example. 
ds.close()

# <codecell>

# Now we can plot the grid
fig, ax = plt.subplots(1,1)
# Use cell index as data to show where the highest cells are
data = np.arange(len(cellcoords))
# Create the unstructured grid
cells = matplotlib.collections.PolyCollection(cellcoords, edgecolors='black', linewidths=1, alpha=0.4, cmap='Greys')
# Set the data to the grid
cells.set_array(data)
# Add it to the plot
ax.add_collection(cells)
# Create a colorbar for the cell indices
cb = plt.colorbar(cells, ax=ax)
cb.set_label('Cell index')
# let's set the colorbar back to 1
cb.set_alpha(1)
cb.draw_all()
# Plot the nodes with a point (deepest point is at 0, 0 is at 6)
sc = ax.scatter(nx,ny, c=nz, cmap='gist_earth', vmin=0, vmax=12)
# and add a colorbar for the z
cb = plt.colorbar(sc, ax=ax)
cb.set_label('Node z')

# <markdowncell>

# Now let's run the model
# -----------------------
# 
# And let's read the data from memory

# <codecell>

# Wrap the dflow_fm library with the python BMI api (taking into account fortran memory order)
fm = openearthtools.modelapi.bmi.BMIFortran(libname=LIBFMNAME, rundir=rundir)
# Initialize the model with the input file
fm.initialize('bendprof.mdu')

# <codecell>

nx = fm.get_1d_double('xk')
ny = fm.get_1d_double('yk')

nelemnode = fm.get_2d_int('netelemnode')
cellcoords = []
for elem in nelemnode:
    elemx = nx[elem-1]
    elemy = ny[elem-1]
    cell = np.c_[elemx, elemy]
    cellcoords.append(cell)
    
cx, cy = np.array([x.mean(0) for x in cellcoords]).T

# <codecell>

# Ok let's put this plotting in a function
def fmplot(cellcoords, variable, ax, **kwargs):
    # I have to recreate the grid for each plot, or do I?
    cells = matplotlib.collections.PolyCollection(cellcoords, **kwargs)

    cells.set_array(variable)
    ax.add_collection(cells)
    # Scale the axis to meet the data
    ax.autoscale()
    # add a colorbar
    cb = plt.colorbar(cells, ax=ax)   
    # set the labels
    cb.set_label('water level [m]')
    ax.set_xlabel("x[m]")
    ax.set_ylabel("y[m]")
    title = "Time:{}".format(fm.get_current_time())
    ax.set_title(title)

# <codecell>

# Now we can get information from the model
print("""Current time: {}
Reference time: {}""".format(fm.get_current_time(), fm.get_string_attribute("refdat")))

# <codecell>

# Get the cell locations (without boundary)
xzw = fm.get_1d_double('xzw')
yzw = fm.get_1d_double('yzw')



# Make a plot of the first 3 timesteps
fig, axes = plt.subplots(1, 4, figsize=(20,4), sharex=True, sharey=True)
# Loop over axes
dt = 20.0

    
for ax in axes.flat:
    # Read the variable s1 
    # make a copy because it will be updated by the model, the variable is a pointer directly into fortran memory
    for i in range(10):
        fm.set_1d_double_at_index('s1', i, 20)
    s1 = fm.get_1d_double('s1').copy()
    fmplot(cellcoords, variable=s1, ax=ax, clim=(0,7), cmap='Blues')
    ux = fm.get_1d_double('ucx').copy()
    uy = fm.get_1d_double('ucy').copy()
    qv = ax.quiver(cx,cy, ux,uy, np.sqrt(ux**2+uy**2), clim=(0,7), units='xy')
    cb = plt.colorbar(qv, ax=ax)
    cb.set_label('velocity')
    # Update the model
    fm.update(dt)

# <codecell>

cx.shape

# <codecell>

shape =  fm.get_var_shape('netelemnode')
import ctypes
arraytype = np.ctypeslib.ndpointer(dtype='int32', ndim=2,shape=shape,flags='F_CONTIGUOUS')
data = arraytype()

fm.lib.get_2d_int.argtypes=[ctypes.c_char_p, ctypes.POINTER(arraytype)]
fm.lib.get_2d_int('netelemnode', data)
d = np.asarray(data)
np.reshape(d.ravel(), shape, order='F')

# <codecell>

np.asfortranarray?


# <codecell>

np.ascontiguousarray(d)

# <codecell>


