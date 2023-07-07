from progress_bar import ProgressBar  
import glob
import netCDF4
import os
import numpy as np

from vtk import (vtkRenderer, vtkLODActor, vtkRenderWindow, vtkWindowToImageFilter, vtkCubeAxesActor, 
                 vtkOutlineFilter, vtkPolyDataMapper, vtkContourFilter, vtkScalarBarActor,
                 vtkPNGWriter, vtkPoints, vtkPolygon, vtkUnstructuredGridReader, vtkUnstructuredGridWriter,
                 vtkUnstructuredGrid, vtkIdList, vtkElevationFilter, vtkDataSetMapper, vtkActor)

import beaker.cache
cachemanager = beaker.cache.CacheManager()
   
@cachemanager.cache('vtk')
def make_renderer_and_window(w, h):
    rendererWindow = vtkRenderWindow()
    rendererWindow.SetOffScreenRendering(1)
    rendererWindow.SetSize(w, h)
    
    renderer = vtkRenderer()
    renderer.SetBackground(1.0, 1.0, 1.0)
    rendererWindow.AddRenderer(renderer)
    
    return renderer, rendererWindow
    
def show(actor, w = 1000, h = 500, zoom = 1.0, isolines = False, cells = True, edges = False, camera_position = None, orientation = [0.0, 0.0, 0.0]):
    renderer, rendererWindow = make_renderer_and_window(w, h)

    camera = renderer.GetActiveCamera()
    
    if cells:
        actor.SetVisibility(1)
        actor.GetProperty().SetRepresentationToSurface();
        if edges:
            actor.GetProperty().EdgeVisibilityOn();
        else:
            actor.GetProperty().EdgeVisibilityOff();
    elif edges:
        actor.SetVisibility(1) 
        actor.GetProperty().SetRepresentationToWireframe();
    else:
        actor.SetVisibility(0)

    renderer.AddActor(actor)

    # create axes
    bounds = actor.GetMapper().GetInput().GetBounds()
    axes = vtkCubeAxesActor()
    axes.SetBounds(bounds)
    axes.SetCamera(camera)
    axes.XAxisLabelVisibilityOn()
    axes.YAxisLabelVisibilityOn()
    axes.ZAxisLabelVisibilityOn()
    axes.DrawXGridlinesOff()
    axes.DrawYGridlinesOff()
    axes.DrawZGridlinesOff()
    axes.GetLabelTextProperty(0).SetColor(0.1, 0.1, 0.1)
    axes.GetXAxesGridlinesProperty().SetColor(0.1, 0.1, 0.1)
    axes.GetYAxesGridlinesProperty().SetColor(0.1, 0.1, 0.1)
    axes.GetZAxesGridlinesProperty().SetColor(0.1, 0.1, 0.1)
    axes.GetXAxesLinesProperty().SetColor(0.1, 0.1, 0.1)
    axes.GetYAxesLinesProperty().SetColor(0.1, 0.1, 0.1)
    axes.GetZAxesLinesProperty().SetColor(0.1, 0.1, 0.1)

    renderer.AddActor(axes)
    
    if isolines:
        # create isolines
        iso = vtkContourFilter()
        iso.SetInput(actor.GetMapper().GetInput()) 
        iso.SetNumberOfContours(10) 
        
        r = actor.GetMapper().GetInput().GetPointData().GetScalars().GetRange()
        start = r[0]
        delta = 0.1 * (r[1] - start)
        for i in range(10):
            iso.SetValue(i, start + delta * i)
    
        iso_mapper = vtkPolyDataMapper()
        iso_mapper.SetInputConnection(iso.GetOutputPort())
     
        iso_actor = vtkActor()
        iso_actor.SetMapper(iso_mapper)    
    
        renderer.AddActor(iso_actor)
        
        scale_bar = vtkScalarBarActor()
        scale_bar.SetLookupTable(iso_mapper.GetLookupTable())
        scale_bar.GetPositionCoordinate().SetCoordinateSystemToNormalizedViewport()
        scale_bar.GetPositionCoordinate().SetValue(0.1,0.01)
        scale_bar.SetOrientationToHorizontal()
        scale_bar.SetWidth(0.8)
        scale_bar.SetHeight(0.17)
        renderer.AddActor2D(scale_bar)

    # 1. The FocalPoint - this controls the point the camera "looks" at.
    # 2. The Position - this controls _where_ the camera is in space.
    # 3. The ViewUp - this controls what the "up" direction is (i.e., the direction that goes bottom-to-top in the view).
    renderer.ResetCamera()
    camera.Zoom(zoom)

    a = camera.GetOrientation()
    camera.Roll(-a[2])
    camera.Elevation(-a[0])
    camera.Azimuth(a[1])

    camera.Elevation(orientation[0])
    camera.Azimuth(-orientation[1])
    camera.Roll(orientation[2])

    camera.ParallelProjectionOff()

    if camera_position:
        old_position = camera.GetPosition()
        camera.SetPosition(camera_position[0], camera_position[1], old_position[2])
        camera.SetFocalPoint(camera_position[0], camera_position[1], camera_position[2])

    # render

    rendererWindow.Render()
     
    windowToImageFilter = vtkWindowToImageFilter()
    windowToImageFilter.SetInput(rendererWindow)
    windowToImageFilter.Update()

    writer = vtkPNGWriter()
    writer.SetWriteToMemory(1)
    writer.SetInputConnection(windowToImageFilter.GetOutputPort())
    writer.Write()
    data = str(buffer(writer.GetResult()))
    
    renderer.RemoveActor(actor)
    
    renderer.RemoveActor(axes)

    if isolines:
        renderer.RemoveActor(iso_actor)

    
    from IPython.display import Image
    return Image(data)

def create_grid_actor(grid):
    elevation_filter = vtkElevationFilter()
    elevation_filter.SetInput(grid)
    
    mapper = vtkDataSetMapper()
    mapper.SetInput(elevation_filter.GetOutput())

    actor = vtkActor()
    actor.SetMapper(mapper)

    bounds = actor.GetBounds()
    elevation_filter.SetLowPoint(0, 0, bounds[4])
    elevation_filter.SetHighPoint(0, 0, bounds[5])
    elevation_filter.Update();

    return actor
    
def write_grid_to_vtk(grid, path, variable="Elevation"):
    grid.GetPointData().GetScalars().SetName(variable)

    writer = vtkUnstructuredGridWriter()
    writer.SetFileName(path)
    writer.SetInput(grid)
    # writer.SetFileTypeToBinary() # buggy in Paraview
    writer.Write()

    print path

def read_grid_from_vtk(path):
    reader = vtkUnstructuredGridReader()
    reader.SetFileName(path)

    return reader.GetOutput()

def read_grid_from_nc(path):
    # read from netcdf (slow creation)
    x, y, z, cells = read_grid_parameters_from_nc(path)
    
    points = vtkPoints()
    points.SetNumberOfPoints(len(x))

    for i in range(0, len(x)):
        points.InsertPoint(i, x[i], y[i], z[i])

    grid = vtkUnstructuredGrid()
    grid.SetPoints(points)
    grid.Allocate(cells.shape[0], cells.shape[0])
    cell_type = vtkPolygon().GetCellType()
    cell_sizes = (cells > 0).sum(1)
    for cell, cell_size in zip(cells, cell_sizes):
        if cell_size < 3:
            continue
        
        ids = vtkIdList()
        ids.SetNumberOfIds(cell_size)
        for i, pointid in enumerate(cell):
            ids.SetId(i, pointid - 1)
        
        grid.InsertNextCell(cell_type, ids)
        
    return grid

def read_grid(path):
    # read from vtk file
    if os.path.exists(path + '.vtk'):
        return read_grid_from_vtk(path + '.vtk')
    else:
        return read_grid_from_nc(path)
    
def read_grid_parameters_from_nc(path):
    ds = netCDF4.Dataset(path)
    x = ds.variables['NetNode_x'][:]
    y = ds.variables['NetNode_y'][:]
    z = ds.variables['NetNode_z'][:]
    cells = ds.variables['NetElemNode'][:] # cell vertex indices
    ds.close()
    
    return x, y, z, cells

def read_water_grid_from_nc(bathymetry_grid, time_step, path):
    ds = netCDF4.Dataset(path)
    cells = ds.variables['NetElemNode'][:]
    s1 = ds.variables['s1'][:]
    ds.close()

    cell_sizes = (cells > 0).sum(1)

    water_levels = s1[time_step,:]

    grid_water = vtkUnstructuredGrid()
    grid_water.DeepCopy(bathymetry_grid)
    grid_water.GetNumberOfPoints()
    data = grid_water.GetPoints().GetData()
    point_data = grid_water.GetPointData().GetArray(0)

    grid_water.GetPointData().GetScalars().SetName("water_level")

    print data.GetComponentName(2)
    
    # update z
    for cell_index, y in enumerate(water_levels):
        cell_size = cell_sizes[cell_index]

        for cell_point in range(cell_size):
            point_index = cells[cell_index, cell_point] - 1
            data.SetComponent(point_index, 2, y)
            point_data.SetValue(point_index, y)

    point_data.Modified()

    return grid_water
