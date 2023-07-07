# -*- coding: utf-8 -*-
# <nbformat>3.0</nbformat>

# <headingcell level=2>

# Multiple D-FLOW FM output files (show, merge)

# <markdowncell>

# The first few pages is just plotting grids, define plot_grid function to plot various grid features

# <codecell>

import netCDF4 as nc
    
def plot_grid(ax, path, points=False, point_color='k', point_size=4, point_alpha=0.6, point_marker='o',
              cells=True, cell_color='black', cell_alpha=0.2, cell_values=None, cells_colorbar=False,
              cell_centers=False, cell_links=False, cell_labels=False, cell_labels_variable='domain', 
              boundary_edges=False):

    # read grid
    ds = nc.Dataset(path)
    x = ds.variables['NetNode_x'][:]
    y = ds.variables['NetNode_y'][:]
    z = ds.variables['NetNode_z'][:]
    cell_points = ds.variables['NetElemNode'][:]
    
    if points:
        ax.plot(x, y, point_color + point_marker, markersize=point_size, linestyle='', alpha=point_alpha)

    if cells:
        # construct collection of polygon vertices, list of lists of (x,y) tuples
        cell_sizes = (cell_points > 0).sum(1)
        cell_count = len(cell_points)
        
        all_cell_coords = []
        for i in range(cell_count):
            cell_size = cell_sizes[i]
            cell_coords = []
            for j in range(cell_size):
                cell_coords.append((x[cell_points[i, j] - 1], y[cell_points[i, j] - 1]))
            all_cell_coords.append(cell_coords)

        polygons = matplotlib.collections.PolyCollection(all_cell_coords)
        polygons.set_alpha(cell_alpha)
        
        if cell_values != None:            
            #polygons.clim=(min(cell_values), max(cell_values))
            polygons.set_array(cell_values)
        else:
            polygons.set_facecolor(cell_color)
        
        if cells_colorbar:
            cb = plt.colorbar(polygons, ax=ax)   
            cb.set_label('cell values')
        
        polygons.set_edgecolor(cell_color)    
        ax.add_collection(polygons)
    
    if cell_centers:
        xcc = ds.variables['FlowElem_xcc'][:]
        ycc = ds.variables['FlowElem_ycc'][:]        
        ax.plot(xcc, ycc, 'bo', markersize=4, linestyle='', alpha=point_alpha)
        
    if cell_labels:
        xcc = ds.variables['FlowElem_xcc'][:]
        ycc = ds.variables['FlowElem_ycc'][:]
        # cell_labels_list = range(len(xcc))

        if cell_labels_variable == 'domain':
            cell_labels_list = ds.variables['FlowElemDomain'][:]
        elif cell_labels_variable == 'index':
            cell_labels_list = np.arange(len(xcc)) + 1
            
        for xc, yc, n in zip(xcc, ycc, cell_labels_list):
            ax.annotate('{}'.format(n), xy=(xc, yc), xytext=(-3, 0), ha='right', textcoords='offset points', 
                        fontweight='bold', fontsize=9)
        
    if cell_links:
        flow_links = ds.variables['FlowLink'][:]
        link_x = xcc[flow_links - 1]
        link_y = ycc[flow_links - 1]

        for line_x, line_y in zip(link_x, link_y):
            ax.add_line(matplotlib.lines.Line2D(line_x, line_y, alpha=0.7, linewidth=2, linestyle=':'))

    if boundary_edges:
        net_links = ds.variables['NetLink'][:]
        net_link_x = x[net_links - 1]
        net_link_y = y[net_links - 1]
        bnd_links = ds.variables['BndLink'][:]
        bnd_link_x = net_link_x[bnd_links-1, :]
        bnd_link_y = net_link_y[bnd_links-1, :]

        for line_x, line_y in zip(bnd_link_x, bnd_link_y):
            ax.add_line(matplotlib.lines.Line2D(line_x, line_y, alpha=0.8, color=cell_color, linewidth=3))
        
    ds.close()

    ax.autoscale()
   

# <markdowncell>

# Set global variables

# <codecell>

# set default figure size
pylab.rcParams['figure.figsize'] = (20.0, 9.0)

# set default input files to use for demos
import glob
grid_file = '../../test_data/dd_example/dd_example_net.nc'
domain_files = glob.glob('../../test_data/dd_example/output/dd_example_*_map.nc')

# <markdowncell>

# Plot a single grid

# <codecell>

f, ax = plt.subplots(1, 1)
plot_grid(ax, grid_file, points=False)

# <markdowncell>

# Plot multiple grids

# <codecell>

f, ax = plt.subplots(1, 1)
plot_grid(ax, grid_file, points=False)

plot_grid(ax, domain_files[0], cell_color='g', point_color='b', points=False, boundary_edges=True, cells=False)
plot_grid(ax, domain_files[1], cell_color='y', point_color='y', points=False, boundary_edges=True, cells=False,
          cell_labels=True, cell_centers=True)
plot_grid(ax, domain_files[2], cell_color='r', point_color='r', points=False, boundary_edges=True, cells=False)
plot_grid(ax, domain_files[3], cell_color='b', point_color='b', points=False, boundary_edges=True, cells=False)

# <codecell>

f, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, sharex='col', sharey='row')

plot_grid(ax1, grid_file, points=False)
plot_grid(ax1, domain_files[0], cell_color='g', point_color='g', boundary_edges=True, points=False)

plot_grid(ax2, grid_file, points=False)
plot_grid(ax2, domain_files[1], cell_color='y', point_color='y', boundary_edges=True, points=False)

plot_grid(ax3, grid_file, points=False)
plot_grid(ax3, domain_files[2], cell_color='r', point_color='r', boundary_edges=True, points=False)

plot_grid(ax4, grid_file, points=False)
plot_grid(ax4, domain_files[3], cell_color='b', point_color='b', boundary_edges=True, points=False)

# <codecell>

f, ax = plt.subplots(1, 1)

plot_grid(ax, domain_files[0], cell_color='g', point_color='g', points=False, 
          cells=True, cell_centers=True, cell_links=True, cell_labels=True,
          boundary_edges=True)

# xlim(100, 400) 
# ylim(300, 500) 

# <codecell>

f, ax = plt.subplots(1, 1)

plot_grid(ax, domain_files[1], cell_color='y', point_color='y', points=False, 
          cells=True, cell_centers=True, cell_links=True, cell_labels=True,
          boundary_edges=True)

# <markdowncell>

# Now let's merge output NetCDF files generated by DD version of D-FLOW FM

# <codecell>

import numpy as np
import netCDF4 as nc
import pandas as pd
import glob
import gc

input_files = glob.glob('../../test_data/dd_example/output/dd_example_*_map.nc')
output_file = '../../test_data/dd_example/output/dd_example_map_merged.nc'

# crashes on 4.5Gb (~10mln cells) grid. We will have to used x64 version of Python
# input_files = glob.glob('../../test_data/par32/par32_dt0d6_24h_*_map.nc')
#output_file = '/home/don/merged.nc'

# <markdowncell>

# The algorithm detects unique points, cells, edges. Then computes new global indices and then reads all variables from all 
# sub-domain grids and writes then into a single output file.

# <codecell>

# I. Points (nNetNode)
df_points = pd.DataFrame()

for i, f in enumerate(input_files):
    ds = nc.Dataset(f)
    x = ds.variables['NetNode_x'][:]
    y = ds.variables['NetNode_y'][:]
    z = ds.variables['NetNode_z'][:]
    ds.close()
    
    df = pd.DataFrame({
                       'x' : x, 
                       'y' : y,
                       'z' : z,
                       'domain' : i,
                       'local_index' : np.arange(len(x)) + 1,
                       'new_index' : -1                       
                       })
    
    df_points = pd.concat([df_points, df])    

df_points.index = np.arange(len(df_points))    

# include only unique points
df_points['is_new'] = df_points.duplicated(cols=['x', 'y']) == False 

# compute new index for all but excluded points
df_points.new_index.ix[df_points['is_new']] = np.arange(len(df_points['is_new'])) + 1

# compute new index for excluded points using lookup
left = df_points[df_points['is_new'] == False]
right = df_points[df_points['is_new'] == True]

right.index=pd.MultiIndex.from_tuples(zip(right.x, right.y))

j = left.join(right, on=['x', 'y'], how='left', sort=False, rsuffix='_r')
df_points.new_index.ix[df_points['is_new'] == False] = j.new_index_r

gc.collect()

# <codecell>

# II. Cells (nNetElem)
df_cells = pd.DataFrame()
for i, f in enumerate(input_files):
    ds = nc.Dataset(f)
    cell_points = ds.variables['NetElemNode'][:]
    xcc = ds.variables['FlowElem_xcc'][:]
    ycc = ds.variables['FlowElem_ycc'][:]
    max_cell_points = len(ds.dimensions['nNetElemMaxNode'])
    cell_domain = ds.variables['FlowElemDomain'][:]
    ds.close()
    
    df = pd.DataFrame({
                       'cell_center_x' : xcc, 
                       'cell_center_y' : ycc,
                       'cell_domain' : cell_domain,
                       'domain' : i,
                       'is_new' : cell_domain == i, # include only cells from this sub-domain
                       'local_index' : np.arange(len(xcc))
                       })
    
    # add columns for cell points
    for j in range(max_cell_points):
        df['cell_points' + str(j)] = pd.Series(cell_points[:,j], index=df.index)
         
    df_cells = pd.concat([df_cells, df])    

df_cells.index = np.arange(len(df_cells))    
    
# replace local point indices by the global ones
right = df_points.copy()
right.index = pd.MultiIndex.from_tuples(zip(right.domain, right.local_index))

for j in range(max_cell_points):
    s = 'cell_points' + str(j)
    j = df_cells.join(right, on=['domain', s], rsuffix='_r', sort=False)
    df_cells[s + '_new'] = j.new_index
    
gc.collect()

# <codecell>

# III. Edges (nNetLink)

df_edges = pd.DataFrame()
for i, f in enumerate(input_files):
    ds = nc.Dataset(f)
    edge_points = ds.variables['NetLink'][:]
    edge_type = ds.variables['NetLinkType'][:]
    ds.close()
    
    df = pd.DataFrame({
                       'from_point' : pd.Series(edge_points[:,0]), 
                       'to_point' : pd.Series(edge_points[:,1]), 
                       'domain' : i
                       })
    
    df_edges = pd.concat([df_edges, df])    

df_edges.index = np.arange(len(df_edges))

# replace local point indices by the global ones
right = df_points.copy()
right.index = pd.MultiIndex.from_tuples(zip(right.domain, right.local_index))

# from
j = df_edges.join(right, on=['domain', 'from_point'], rsuffix='_r')
df_edges['from_point_new'] = j['new_index']

# to
j = df_edges.join(right, on=['domain', 'to_point'], rsuffix='_r')
df_edges['to_point_new'] = j['new_index']

# exclude duplicates
df_edges['is_new'] = df_edges.duplicated(cols=['from_point_new', 'to_point_new']) == False 

gc.collect()

# <codecell>

def merge_variables(v_name):
    df_values = pd.DataFrame()
    for i, f in enumerate(input_files):
        ds = nc.Dataset(f)
        values = ds.variables[v_name][:]
        ds.close()
        df = pd.DataFrame(values)
        df_values = pd.concat([df_values, df])    
    df_values.index = np.arange(len(df_values))
    return df_values
    
def write_merged_variable_values_points(v_name, v):
    df_values = merge_variables(v_name)
    v[:] = df_values[df_points['is_new']][0]

def write_merged_variable_values_grid_cells(v_name, v):
    df_values = merge_variables(v_name)
    for i in range(max_cell_points):
        v[:, i] = df_cells[df_cells['is_new']]['cell_points' + str(i) + '_new'].values
       
def write_merged_variable_values_on_cells(v_name, v):
    df_values = merge_variables(v_name)
    v[:] = df_values[df_cells['is_new']][0].values
    
def write_merged_variable_values_t(v_name, v):
    ds = nc.Dataset(input_files[0])
    nt=len(ds.dimensions['time'])
    v[:]=ds.variables[v_name][:]
    ds.close()
    
def write_merged_variable_values_on_cells_t(v_name, v):
    ds = nc.Dataset(input_files[0])
    nt=len(ds.dimensions['time'])
    ds.close()
    
    new_cell_indices = df_cells['is_new']
    for n in range(nt):
        df_values = pd.DataFrame()
        for i, f in enumerate(input_files):
            ds = nc.Dataset(f)
            values = ds.variables[v_name][n, :]
            df = pd.DataFrame(values)
            df_values = pd.concat([df_values, df])    
            ds.close()
        df_values.index = np.arange(len(df_values))
        v[n, :] = df_values[new_cell_indices][0].values
        
def write_merged_variable_values_empty(v_name, v):
    pass

# write merged output netcdf file
ds_o = nc.Dataset(output_file, 'w', format = 'NETCDF3_CLASSIC')

ds = nc.Dataset(input_files[0])

# create attributes
[ds_o.setncattr(a, getattr(ds, a)) for a in ds.ncattrs()] 

# create dimensions
dimensions = {}
dimensions['nNetNode'] = len(df_points[df_points['is_new']])
dimensions['nNetLink'] = len(df_edges[df_edges['is_new']])
dimensions['nNetLinkPts'] = len(ds.dimensions['nNetLinkPts'])
dimensions['nBndLink'] = len(ds.dimensions['nBndLink']) # TODO: implement
dimensions['nNetElem'] = len(df_cells[df_cells['is_new']])
dimensions['nNetElemMaxNode'] = len(ds.dimensions['nNetElemMaxNode'])
dimensions['nFlowElem'] = dimensions['nNetElem']
dimensions['nFlowElemMaxNode'] = dimensions['nNetElemMaxNode']
dimensions['nFlowElemContourPts'] = len(ds.dimensions['nFlowElemContourPts'])
dimensions['nFlowLink'] = len(ds.dimensions['nFlowLink']) # TODO: implement
dimensions['nFlowLinkPts'] = len(ds.dimensions['nFlowLinkPts']) # TODO: implement
dimensions['time'] = None 

for (k, v) in dimensions.iteritems():
    ds_o.createDimension(k, v)
    
# create variables
write_variable_values = {}
write_variable_values[(u'nNetNode',)] = write_merged_variable_values_points
write_variable_values[(u'nNetLink', u'nNetLinkPts')] = write_merged_variable_values_empty # TODO: implement
write_variable_values[(u'nNetLink',)] = write_merged_variable_values_empty # TODO: implement
write_variable_values[(u'nNetElem', u'nNetElemMaxNode')] = write_merged_variable_values_grid_cells
write_variable_values[(u'nBndLink',)] = write_merged_variable_values_empty # TODO: implement
write_variable_values[(u'nFlowElem',)] = write_merged_variable_values_on_cells
write_variable_values[(u'nFlowElem', u'nFlowElemContourPts')] = write_merged_variable_values_empty # TODO:
write_variable_values[(u'nFlowLink', u'nFlowLinkPts')] = write_merged_variable_values_empty # TODO: implement
write_variable_values[(u'nFlowLink',)] = write_merged_variable_values_empty # TODO: implement
write_variable_values[(u'time',)] = write_merged_variable_values_t
write_variable_values[(u'time', u'nFlowElem')] = write_merged_variable_values_on_cells_t
write_variable_values[(u'time', u'nFlowLink')] = write_merged_variable_values_empty # TODO: implement
write_variable_values[()] = write_merged_variable_values_empty

variables_to_write=['NetNode_x','NetNode_y','NetNode_z','NetLink', 
                    'NetElemNode','FlowElem_xcc','FlowElem_ycc',
                    'FlowElemDomain','time','s1','s0','taus','ucx','ucy']

for (k, v) in ds.variables.items():
    if k in variables_to_write:
        print 'Writing variable: ' + k + ' ...'
        v_new = ds_o.createVariable(k, v.datatype, v.dimensions)
        [v_new.setncattr(a, getattr(v, a)) for a in v.ncattrs()] 
        write_variable_values[v.dimensions](k, v_new)

ds.close()
ds_o.close()

# <codecell>

del df_points
del df_cells
del df_edges

# <codecell>

with open('merge_points.html', 'w') as f:
    df_points.to_html(f)
    
with open('merge_cells.html', 'w') as f:
    df_cells.to_html(f)
    
with open('merge_edges.html', 'w') as f:
    df_edges.to_html(f
                     )

# <codecell>

grid_file = '../../test_data/dd_example/output/dd_example_map_merged.nc'
f, ax = plt.subplots(1, 1)

values=df_cells[df_cells['is_new']]['domain'].values
plot_grid(ax, grid_file, points=True, cell_centers=True, cell_values=values, cells_colorbar=True)

# <codecell>

grid_file = '../../test_data/dd_example/output/dd_example_map_merged.nc'
f, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2)

ds = nc.Dataset(grid_file)

values=ds.variables['s1'][100,:]
plot_grid(ax1, grid_file, cell_values=values, cells_colorbar=True)

values=ds.variables['s1'][110,:]
plot_grid(ax2, grid_file, cell_values=values, cells_colorbar=True)

values=ds.variables['s1'][120,:]
plot_grid(ax3, grid_file, cell_values=values, cells_colorbar=True)

values=ds.variables['s1'][130,:]
plot_grid(ax4, grid_file, cell_values=values, cells_colorbar=True)

ds.close()

# <codecell>

grid_file = '../../test_data/dd_example/output/dd_example_0000_map.nc'
f, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2)

ds = nc.Dataset(grid_file)

values=ds.variables['s1'][100,:]
plot_grid(ax1, grid_file, cell_values=values, cells_colorbar=True)

values=ds.variables['s1'][110,:]
plot_grid(ax2, grid_file, cell_values=values, cells_colorbar=True)

values=ds.variables['s1'][120,:]
plot_grid(ax3, grid_file, cell_values=values, cells_colorbar=True)

values=ds.variables['s1'][130,:]
plot_grid(ax4, grid_file, cell_values=values, cells_colorbar=True)

ds.close()

# <codecell>

# 32 domains, ~10M points, plot points only

files = glob.glob('../../test_data/par32/par32_dt0d6_24h_*_map.nc')

f, ax = plt.subplots(1, 1)

for i, f in enumerate(files):
    plot_grid(ax, f, points=True, cells=False, point_color='k', point_marker='.', point_size=0.1)
    
plot_grid(ax, files[0], points=True, cells=False, point_color='g', point_marker='.', point_size=0.1)
plot_grid(ax, files[5], points=True, cells=False, point_color='b', point_marker='.', point_size=0.1)

