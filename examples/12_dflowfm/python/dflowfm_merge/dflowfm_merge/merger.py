import netCDF4 as nc
import numpy as np
import pandas as pd
import gc

class merger(object):
    """Merges multiple output files of D-FLOW FM into a single file"""

    def __init__(self, input_files, output_file):
        self.input_files = input_files
        self.output_file = output_file

    def merge_points(self):
        """ I. Points (nNetNode) """
        self.df_points = pd.DataFrame()

        for i, f in enumerate(self.input_files):
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
    
            self.df_points = pd.concat([self.df_points, df])    
    
        self.df_points.index = np.arange(len(self.df_points))    

        # include only unique points
        self.df_points['is_new'] = self.df_points.duplicated(cols=['x', 'y']) == False 

        # compute new index for all but excluded points
        self.df_points.new_index.ix[self.df_points['is_new']] = np.arange(len(self.df_points['is_new'])) + 1

        # compute new index for excluded points using lookup
        left = self.df_points[self.df_points['is_new'] == False]
        right = self.df_points[self.df_points['is_new'] == True]

        right.index = pd.MultiIndex.from_tuples(zip(right.x, right.y))

        j = left.join(right, on=['x', 'y'], how='left', sort=False, rsuffix='_r')
        self.df_points.new_index.ix[self.df_points['is_new'] == False] = j.new_index_r

        gc.collect()

    def merge_cells(self):
        """ II. Cells (nNetElem) """
        self.df_cells = pd.DataFrame()
        for i, f in enumerate(self.input_files):
            ds = nc.Dataset(f)
            cell_points = ds.variables['NetElemNode'][:]
            xcc = ds.variables['FlowElem_xcc'][:]
            ycc = ds.variables['FlowElem_ycc'][:]
            self.max_cell_points = len(ds.dimensions['nNetElemMaxNode'])
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
            for j in range(self.max_cell_points):
                df['cell_points' + str(j)] = pd.Series(cell_points[:,j], index=df.index)
         
            self.df_cells = pd.concat([self.df_cells, df])    

        self.df_cells.index = np.arange(len(self.df_cells))    
    
        # replace local point indices by the global ones
        right = self.df_points.copy()
        right.index = pd.MultiIndex.from_tuples(zip(right.domain, right.local_index))

        for j in range(self.max_cell_points):
            s = 'cell_points' + str(j)
            j = self.df_cells.join(right, on=['domain', s], rsuffix='_r', sort=False)
            self.df_cells[s + '_new'] = j.new_index
    
        gc.collect()

    def merge_cell_edges(self):
        """ III. Edges (nNetLink) """

        self.df_edges = pd.DataFrame()
        for i, f in enumerate(self.input_files):
            ds = nc.Dataset(f)
            edge_points = ds.variables['NetLink'][:]
            edge_type = ds.variables['NetLinkType'][:]
            ds.close()
    
            df = pd.DataFrame({
                               'from_point' : pd.Series(edge_points[:,0]), 
                               'to_point' : pd.Series(edge_points[:,1]), 
                               'domain' : i
                               })
    
            self.df_edges = pd.concat([self.df_edges, df])    

        self.df_edges.index = np.arange(len(self.df_edges))

        # replace local point indices by the global ones
        right = self.df_points.copy()
        right.index = pd.MultiIndex.from_tuples(zip(right.domain, right.local_index))

        # from
        j = self.df_edges.join(right, on=['domain', 'from_point'], rsuffix='_r')
        self.df_edges['from_point_new'] = j['new_index']

        # to
        j = self.df_edges.join(right, on=['domain', 'to_point'], rsuffix='_r')
        self.df_edges['to_point_new'] = j['new_index']

        # exclude duplicates
        self.df_edges['is_new'] = self.df_edges.duplicated(cols=['from_point_new', 'to_point_new']) == False 

        gc.collect()

    def merge_files(self):
        # write merged output netcdf file
        ds_o = nc.Dataset(self.output_file, 'w', format = 'NETCDF3_CLASSIC')

        ds = nc.Dataset(self.input_files[0])

        # create attributes
        [ds_o.setncattr(a, getattr(ds, a)) for a in ds.ncattrs()] 

        # create dimensions
        dimensions = {}
        dimensions['nNetNode'] = len(self.df_points[self.df_points['is_new']])
        dimensions['nNetLink'] = len(self.df_edges[self.df_edges['is_new']])
        dimensions['nNetLinkPts'] = len(ds.dimensions['nNetLinkPts'])
        dimensions['nBndLink'] = len(ds.dimensions['nBndLink']) # TODO: implement
        dimensions['nNetElem'] = len(self.df_cells[self.df_cells['is_new']])
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
        write_variable_values[(u'nNetNode',)] = self.write_merged_variable_values_points
        write_variable_values[(u'nNetLink', u'nNetLinkPts')] = self.write_merged_variable_values_empty # TODO: implement
        write_variable_values[(u'nNetLink',)] = self.write_merged_variable_values_empty # TODO: implement
        write_variable_values[(u'nNetElem', u'nNetElemMaxNode')] = self.write_merged_variable_values_grid_cells
        write_variable_values[(u'nBndLink',)] = self.write_merged_variable_values_empty # TODO: implement
        write_variable_values[(u'nFlowElem',)] = self.write_merged_variable_values_on_cells
        write_variable_values[(u'nFlowElem', u'nFlowElemContourPts')] = self.write_merged_variable_values_empty # TODO:
        write_variable_values[(u'nFlowLink', u'nFlowLinkPts')] = self.write_merged_variable_values_empty # TODO: implement
        write_variable_values[(u'nFlowLink',)] = self.write_merged_variable_values_empty # TODO: implement
        write_variable_values[(u'time',)] = self.write_merged_variable_values_t
        write_variable_values[(u'time', u'nFlowElem')] = self.write_merged_variable_values_on_cells_t
        write_variable_values[(u'time', u'nFlowLink')] = self.write_merged_variable_values_empty # TODO: implement
        write_variable_values[()] = self.write_merged_variable_values_empty

        variables_to_write = ['NetNode_x','NetNode_y','NetNode_z','NetLink', 
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


    def merge_variables(self, v_name):
        df_values = pd.DataFrame()
        for i, f in enumerate(self.input_files):
            ds = nc.Dataset(f)
            values = ds.variables[v_name][:]
            ds.close()
            df = pd.DataFrame(values)
            df_values = pd.concat([df_values, df])    
        df_values.index = np.arange(len(df_values))
        return df_values
    
    def write_merged_variable_values_points(self, v_name, v):
        df_values = self.merge_variables(v_name)
        v[:] = df_values[self.df_points['is_new']][0]

    def write_merged_variable_values_grid_cells(self, v_name, v):
        df_values = self.merge_variables(v_name)
        for i in range(self.max_cell_points):
            v[:, i] = self.df_cells[self.df_cells['is_new']]['cell_points' + str(i) + '_new'].values
       
    def write_merged_variable_values_on_cells(self, v_name, v):
        df_values = self.merge_variables(v_name)
        v[:] = df_values[self.df_cells['is_new']][0].values
    
    def write_merged_variable_values_t(self, v_name, v):
        ds = nc.Dataset(self.input_files[0])
        nt = len(ds.dimensions['time'])
        v[:] = ds.variables[v_name][:]
        ds.close()
    
    def write_merged_variable_values_on_cells_t(self, v_name, v):
        ds = nc.Dataset(self.input_files[0])
        nt = len(ds.dimensions['time'])
        ds.close()
    
        new_cell_indices = self.df_cells['is_new']
        for n in range(nt):
            df_values = pd.DataFrame()
            for i, f in enumerate(self.input_files):
                ds = nc.Dataset(f)
                values = ds.variables[v_name][n, :]
                df = pd.DataFrame(values)
                df_values = pd.concat([df_values, df])    
                ds.close()
            df_values.index = np.arange(len(df_values))
            v[n, :] = df_values[new_cell_indices][0].values
        
    def write_merged_variable_values_empty(self, v_name, v):
        pass

    def run(self):
        self.merge_points()
        self.merge_cells()
        self.merge_cell_edges()
        self.merge_files()


