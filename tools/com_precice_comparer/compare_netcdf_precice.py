#!/usr/bin/env python3.12
"""
Compare water levels and bed levels from NetCDF COM file vs preCICE VTU exports.

The COM file is a NetCDF file containing D-Flow FM output data.

IMPORTANT NOTE:
The preCICE VTU files and COM NetCDF file may be written at different frequencies.
For example, the COM file might have 3001 timesteps while preCICE only exports 25 files.
The timestep indices between these files do NOT correspond directly.
You must ensure you are comparing data from the same simulation time!

This script:
1. Reads water levels (s1) and bed levels (FlowElem_bl) from NetCDF COM file
2. Reads water levels (water_levels) and bed levels (bed_levels) from preCICE VTU export
3. Compares the data on both grids
4. Generates comparison plots and statistics
5. Visualizes grid structures (FM and SWAN)

Usage:
    python compare_netcdf_precice.py \
        --netcdf-file <path/to/f34_com.nc> \
        --vtu-file <path/to/fm_flow_nodes-fm.dt1.vtu> \
        --netcdf-timestep 1 \
        --output-dir <output_dir>
"""

import argparse
import sys
from pathlib import Path
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import tri
import warnings

# Try importing required dependencies
try:
    from netCDF4 import Dataset
    HAS_NETCDF = True
except ImportError:
    HAS_NETCDF = False
    warnings.warn("netCDF4 not available. Install with: pip install netCDF4")

try:
    import pyvista as pv
    HAS_PYVISTA = True
except ImportError:
    HAS_PYVISTA = False

try:
    import meshio
    HAS_MESHIO = True
except ImportError:
    HAS_MESHIO = False

if not HAS_PYVISTA and not HAS_MESHIO:
    warnings.warn("Neither pyvista nor meshio available. Install one with: pip install pyvista OR pip install meshio")


class NetCDFCOMReader:
    """Reader for NetCDF COM files."""
    
    def __init__(self, netcdf_file):
        self.netcdf_file = Path(netcdf_file)
        self.ds = None
        
    def open(self):
        """Open NetCDF file."""
        if not HAS_NETCDF:
            raise ImportError("netCDF4 is required. Install with: pip install netCDF4")
        
        if not self.netcdf_file.exists():
            raise FileNotFoundError(f"NetCDF file not found: {self.netcdf_file}")
        
        self.ds = Dataset(self.netcdf_file, 'r')
        return self
    
    def close(self):
        """Close NetCDF file."""
        if self.ds:
            self.ds.close()
    
    def __enter__(self):
        return self.open()
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()
    
    def get_dimensions(self):
        """Get dimension information."""
        if not self.ds:
            raise RuntimeError("NetCDF file not opened. Call open() first.")
        
        dims = {}
        for dim_name in self.ds.dimensions:
            dims[dim_name] = len(self.ds.dimensions[dim_name])
        return dims
    
    def get_variables(self):
        """Get list of variables."""
        if not self.ds:
            raise RuntimeError("NetCDF file not opened. Call open() first.")
        
        return list(self.ds.variables.keys())
    
    def get_time_info(self):
        """Get time dimension information."""
        if not self.ds:
            raise RuntimeError("NetCDF file not opened. Call open() first.")
        
        time_var = self.ds.variables['time']
        n_timesteps = len(time_var)
        times = time_var[:]
        
        info = {
            'n_timesteps': n_timesteps,
            'times': times,
            'units': time_var.units if hasattr(time_var, 'units') else 'unknown'
        }
        
        return info
    
    def read_grid_coordinates(self):
        """Read grid coordinates (FlowElem_xcc, FlowElem_ycc)."""
        if not self.ds:
            raise RuntimeError("NetCDF file not opened. Call open() first.")
        
        x = self.ds.variables['FlowElem_xcc'][:]
        y = self.ds.variables['FlowElem_ycc'][:]
        
        # Create 3D points array (x, y, z=0)
        points = np.column_stack([x, y, np.zeros_like(x)])
        
        return points
    
    def read_water_levels(self, timestep=0):
        """
        Read water levels (s1) at a specific timestep.
        
        Args:
            timestep: Timestep index (0-based)
        
        Returns:
            dict with 'points' (coordinates) and 'water_levels' (s1 values)
        """
        if not self.ds:
            raise RuntimeError("NetCDF file not opened. Call open() first.")
        
        # Check timestep validity
        time_info = self.get_time_info()
        if timestep >= time_info['n_timesteps']:
            raise ValueError(
                f"Timestep {timestep} out of range. "
                f"File has {time_info['n_timesteps']} timesteps (0-{time_info['n_timesteps']-1})"
            )
        
        # Read coordinates
        points = self.read_grid_coordinates()
        
        # Read water levels at this timestep
        s1 = self.ds.variables['s1'][timestep, :]
        
        data = {
            'points': points,
            'water_levels': s1,
            'timestep': timestep,
            'time': time_info['times'][timestep],
            'n_points': len(s1)
        }
        
        return data
    
    def read_bed_levels(self):
        """
        Read bed levels (FlowElem_bl).
        
        Note: Bed levels are typically time-independent in D-Flow FM.
        
        Returns:
            dict with 'points' (coordinates) and 'bed_levels' (FlowElem_bl values)
        """
        if not self.ds:
            raise RuntimeError("NetCDF file not opened. Call open() first.")
        
        # Read coordinates
        points = self.read_grid_coordinates()
        
        # Read bed levels
        bl = self.ds.variables['FlowElem_bl'][:]
        
        data = {
            'points': points,
            'bed_levels': bl,
            'n_points': len(bl)
        }
        
        return data


class VTUReader:
    """Reader for preCICE VTU export files."""
    
    def __init__(self, vtu_file):
        self.vtu_file = Path(vtu_file)
        
    def read_pyvista(self):
        """Read VTU file using PyVista."""
        if not HAS_PYVISTA:
            raise ImportError("PyVista is required. Install with: pip install pyvista")
        
        if not self.vtu_file.exists():
            raise FileNotFoundError(f"VTU file not found: {self.vtu_file}")
        
        mesh = pv.read(self.vtu_file)
        
        data = {
            'points': np.array(mesh.points),
            'n_points': mesh.n_points,
            'n_cells': mesh.n_cells,
            'cells': None,
            'point_data': {}
        }
        
        # Extract cells/connectivity for triangular meshes
        if mesh.n_cells > 0:
            try:
                # Extract triangles (cell type 5 in VTK)
                cells = []
                for i in range(mesh.n_cells):
                    cell = mesh.get_cell(i)
                    if cell.type == 5:  # VTK_TRIANGLE
                        cells.append(cell.point_ids)
                if cells:
                    data['cells'] = np.array(cells)
            except Exception as e:
                print(f"Warning: Could not extract cells: {e}")
        
        # Extract point data
        for name in mesh.point_data.keys():
            data['point_data'][name] = np.array(mesh.point_data[name])
        
        return data
    
    def read_meshio(self):
        """Read VTU file using meshio."""
        if not HAS_MESHIO:
            raise ImportError("meshio is required. Install with: pip install meshio")
        
        if not self.vtu_file.exists():
            raise FileNotFoundError(f"VTU file not found: {self.vtu_file}")
        
        mesh = meshio.read(self.vtu_file)
        
        data = {
            'points': mesh.points,
            'n_points': len(mesh.points),
            'cells': None,
            'point_data': mesh.point_data
        }
        
        # Extract triangle connectivity
        if mesh.cells:
            triangles = []
            for cell_block in mesh.cells:
                if cell_block.type == 'triangle':
                    triangles.extend(cell_block.data)
            if triangles:
                data['cells'] = np.array(triangles)
        
        return data
    
    def read(self):
        """Read VTU file using available library."""
        if HAS_PYVISTA:
            return self.read_pyvista()
        elif HAS_MESHIO:
            return self.read_meshio()
        else:
            raise ImportError("Either PyVista or meshio must be installed")
    
    def extract_water_levels(self):
        """Extract water levels from VTU file."""
        data = self.read()
        
        if 'water_levels' not in data['point_data']:
            available_fields = list(data['point_data'].keys())
            raise ValueError(
                f"'water_levels' field not found in VTU file. "
                f"Available fields: {available_fields}"
            )
        
        result = {
            'points': data['points'],
            'water_levels': data['point_data']['water_levels'],
            'n_points': data['n_points']
        }
        
        return result
    
    def extract_bed_levels(self):
        """Extract bed levels from VTU file."""
        data = self.read()
        
        if 'bed_levels' not in data['point_data']:
            available_fields = list(data['point_data'].keys())
            raise ValueError(
                f"'bed_levels' field not found in VTU file. "
                f"Available fields: {available_fields}"
            )
        
        result = {
            'points': data['points'],
            'bed_levels': data['point_data']['bed_levels'],
            'n_points': data['n_points']
        }
        
        return result


class WaterLevelComparer:
    """Compare water levels and bed levels between NetCDF COM file and preCICE VTU."""
    
    def __init__(self, output_dir=None):
        self.output_dir = Path(output_dir) if output_dir else Path('.')
        self.output_dir.mkdir(parents=True, exist_ok=True)
    
    def plot_bed_level_comparison(self, netcdf_data, vtu_data, save_as=None):
        """Plot bed level comparison between NetCDF and VTU."""
        fig, axes = plt.subplots(1, 3, figsize=(20, 6))
        
        # NetCDF bed levels
        netcdf_points = netcdf_data['points']
        netcdf_bl = netcdf_data['bed_levels']
        
        # VTU bed levels
        vtu_points = vtu_data['points']
        vtu_bl = vtu_data['bed_levels']
        
        # Plot 1: NetCDF bed levels
        sc1 = axes[0].scatter(netcdf_points[:, 0], netcdf_points[:, 1], 
                             c=netcdf_bl, s=30, cmap='terrain', edgecolor='k', linewidth=0.3)
        axes[0].set_xlabel('X (m)')
        axes[0].set_ylabel('Y (m)')
        axes[0].set_title(f'NetCDF Bed Levels\\n({len(netcdf_bl)} points)')
        axes[0].set_aspect('equal')
        axes[0].grid(True, alpha=0.3)
        plt.colorbar(sc1, ax=axes[0], label='Bed Level (m)')
        
        # Add statistics
        netcdf_stats = f'Min: {np.min(netcdf_bl):.3f} m\\nMax: {np.max(netcdf_bl):.3f} m\\nMean: {np.mean(netcdf_bl):.3f} m'
        axes[0].text(0.02, 0.98, netcdf_stats,
                    transform=axes[0].transAxes,
                    verticalalignment='top',
                    bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5),
                    fontsize=9)
        
        # Plot 2: VTU bed levels
        sc2 = axes[1].scatter(vtu_points[:, 0], vtu_points[:, 1], 
                             c=vtu_bl, s=30, cmap='terrain', edgecolor='k', linewidth=0.3)
        axes[1].set_xlabel('X (m)')
        axes[1].set_ylabel('Y (m)')
        axes[1].set_title(f'VTU Bed Levels\\n({len(vtu_bl)} points)')
        axes[1].set_aspect('equal')
        axes[1].grid(True, alpha=0.3)
        plt.colorbar(sc2, ax=axes[1], label='Bed Level (m)')
        
        # Add statistics
        vtu_stats = f'Min: {np.min(vtu_bl):.3f} m\\nMax: {np.max(vtu_bl):.3f} m\\nMean: {np.mean(vtu_bl):.3f} m'
        axes[1].text(0.02, 0.98, vtu_stats,
                    transform=axes[1].transAxes,
                    verticalalignment='top',
                    bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5),
                    fontsize=9)
        
        # Plot 3: Scatter plot comparison
        axes[2].scatter(netcdf_bl, vtu_bl, alpha=0.5, s=20)
        
        # Add diagonal line (perfect match)
        min_bl = min(np.min(netcdf_bl), np.min(vtu_bl))
        max_bl = max(np.max(netcdf_bl), np.max(vtu_bl))
        axes[2].plot([min_bl, max_bl], [min_bl, max_bl], 'r--', label='Perfect Match', linewidth=2)
        
        axes[2].set_xlabel('NetCDF Bed Level (m)')
        axes[2].set_ylabel('VTU Bed Level (m)')
        axes[2].set_title('Bed Level Correlation')
        axes[2].grid(True, alpha=0.3)
        axes[2].legend()
        axes[2].set_aspect('equal')
        
        # Calculate correlation statistics
        if len(netcdf_bl) == len(vtu_bl):
            diff = vtu_bl - netcdf_bl
            rmse = np.sqrt(np.mean(diff**2))
            mae = np.mean(np.abs(diff))
            corr_stats = f'RMSE: {rmse:.4f} m\\nMAE: {mae:.4f} m\\nMax Diff: {np.max(np.abs(diff)):.4f} m'
            axes[2].text(0.02, 0.98, corr_stats,
                        transform=axes[2].transAxes,
                        verticalalignment='top',
                        bbox=dict(boxstyle='round', facecolor='lightblue', alpha=0.7),
                        fontsize=9)
        
        plt.tight_layout()
        
        if save_as:
            output_path = self.output_dir / save_as
            plt.savefig(output_path, dpi=150, bbox_inches='tight')
            print(f"Saved: {output_path}")
            plt.close()
        else:
            plt.show()
    
    def plot_grid_comparison(self, fm_vtu_data, wave_vtu_data=None, save_as=None):
        """
        Plot FM and SWAN grids side by side and overlaid from VTU files.
        
        Args:
            fm_vtu_data: Dict with 'points' and 'cells' from FM VTU file
            wave_vtu_data: Optional dict with 'points' from SWAN/wave VTU file
            save_as: Filename to save plot
        """
        fig, axes = plt.subplots(1, 3, figsize=(24, 8))
        
        # Extract FM grid coordinates
        fm_points = fm_vtu_data['points']
        fm_x, fm_y = fm_points[:, 0], fm_points[:, 1]
        fm_cells = fm_vtu_data.get('cells')
        
        # Plot 1: FM grid with connectivity
        if fm_cells is not None and len(fm_cells) > 0:
            from matplotlib.tri import Triangulation
            triang_fm = Triangulation(fm_x, fm_y, triangles=fm_cells)
            axes[0].triplot(triang_fm, 'b-', alpha=0.4, linewidth=0.5)
            axes[0].plot(fm_x, fm_y, 'bo', markersize=3, alpha=0.6, label='FM nodes')
        else:
            axes[0].scatter(fm_x, fm_y, c='blue', s=20, alpha=0.6, marker='o', label='FM nodes')
        
        axes[0].set_xlabel('X [m]')
        axes[0].set_ylabel('Y [m]')
        axes[0].set_title(f'FM Grid from VTU\n({len(fm_points)} nodes, {len(fm_cells) if fm_cells is not None else 0} cells)')
        axes[0].set_aspect('equal')
        axes[0].grid(True, alpha=0.3)
        axes[0].legend()
        
        # Plot 2: SWAN/Wave grid (if provided)
        if wave_vtu_data is not None:
            wave_points = wave_vtu_data['points']
            wave_x, wave_y = wave_points[:, 0], wave_points[:, 1]
            axes[1].scatter(wave_x, wave_y, c='red', s=20, alpha=0.6, marker='o', label='SWAN nodes')
            axes[1].set_xlabel('X [m]')
            axes[1].set_ylabel('Y [m]')
            axes[1].set_title(f'SWAN Grid from VTU\n({len(wave_points)} nodes)')
            axes[1].set_aspect('equal')
            axes[1].grid(True, alpha=0.3)
            axes[1].legend()
            
            # Plot 3: Overlay
            if fm_cells is not None and len(fm_cells) > 0:
                triang_fm = Triangulation(fm_x, fm_y, triangles=fm_cells)
                axes[2].triplot(triang_fm, 'b-', alpha=0.2, linewidth=0.3, label='FM cells')
            axes[2].plot(fm_x, fm_y, 'bo', markersize=2, alpha=0.5, label='FM nodes')
            axes[2].plot(wave_x, wave_y, 'r^', markersize=3, alpha=0.7, label='SWAN nodes')
            axes[2].set_xlabel('X [m]')
            axes[2].set_ylabel('Y [m]')
            axes[2].set_title('Grid Overlay')
            axes[2].set_aspect('equal')
            axes[2].grid(True, alpha=0.3)
            axes[2].legend()
        else:
            # No wave grid - just show FM grid in different views
            axes[1].text(0.5, 0.5, 'No SWAN/Wave VTU file provided', 
                        ha='center', va='center', transform=axes[1].transAxes)
            axes[1].axis('off')
            
            axes[2].text(0.5, 0.5, 'No SWAN/Wave VTU file provided', 
                        ha='center', va='center', transform=axes[2].transAxes)
            axes[2].axis('off')
        
        plt.tight_layout()
        
        if save_as:
            output_path = self.output_dir / save_as
            plt.savefig(output_path, dpi=200, bbox_inches='tight')
            print(f"Saved: {output_path}")
            plt.close()
        else:
            plt.show()
    
    def plot_comparison(self, netcdf_data, vtu_data, save_as=None):
        """
        Compare water levels from NetCDF and VTU.
        
        Args:
            netcdf_data: Dict with 'points' and 'water_levels' from NetCDF
            vtu_data: Dict with 'points' and 'water_levels' from VTU
            save_as: Filename to save plot
        """
        fig, axes = plt.subplots(2, 2, figsize=(20, 16))
        
        # Extract data
        nc_points = netcdf_data['points']
        nc_wl = netcdf_data['water_levels']
        nc_x, nc_y = nc_points[:, 0], nc_points[:, 1]
        
        vtu_points = vtu_data['points']
        vtu_wl = vtu_data['water_levels']
        vtu_x, vtu_y = vtu_points[:, 0], vtu_points[:, 1]
        
        # Determine common color scale
        vmin = min(np.min(nc_wl), np.min(vtu_wl))
        vmax = max(np.max(nc_wl), np.max(vtu_wl))
        
        # Plot 1: NetCDF water levels (scatter plot at grid points)
        sc1 = axes[0, 0].scatter(nc_x, nc_y, c=nc_wl, s=40, cmap='viridis', 
                                  vmin=vmin, vmax=vmax, edgecolors='black', linewidth=0.5)
        axes[0, 0].set_title(f'Water Levels (s1) - NetCDF COM File\nTimestep: {netcdf_data.get("timestep", "?")}, Time: {netcdf_data.get("time", "?")} s')
        axes[0, 0].set_xlabel('X [m]')
        axes[0, 0].set_ylabel('Y [m]')
        axes[0, 0].set_aspect('equal')
        axes[0, 0].grid(True, alpha=0.3)
        plt.colorbar(sc1, ax=axes[0, 0], label='Water Level [m]')
        
        # Plot 2: VTU water levels (scatter plot at grid points)
        sc2 = axes[0, 1].scatter(vtu_x, vtu_y, c=vtu_wl, s=40, cmap='viridis', 
                                  vmin=vmin, vmax=vmax, edgecolors='black', linewidth=0.5)
        axes[0, 1].set_title('Water Levels - preCICE VTU Export')
        axes[0, 1].set_xlabel('X [m]')
        axes[0, 1].set_ylabel('Y [m]')
        axes[0, 1].set_aspect('equal')
        axes[0, 1].grid(True, alpha=0.3)
        plt.colorbar(sc2, ax=axes[0, 1], label='Water Level [m]')
        
        # Plot 3: Histogram comparison
        axes[1, 0].hist([nc_wl, vtu_wl], bins=50, alpha=0.7, 
                       label=['NetCDF COM', 'preCICE VTU'], color=['blue', 'red'])
        axes[1, 0].set_xlabel('Water Level [m]')
        axes[1, 0].set_ylabel('Frequency')
        axes[1, 0].set_title('Water Level Distribution Comparison')
        axes[1, 0].legend()
        axes[1, 0].grid(True, alpha=0.3)
        
        # Plot 4: Statistics
        stats_text = f"""
NetCDF COM File Statistics (s1):
  Number of points: {len(nc_wl)}
  Min:  {np.min(nc_wl):.6f} m
  Max:  {np.max(nc_wl):.6f} m
  Mean: {np.mean(nc_wl):.6f} m
  Std:  {np.std(nc_wl):.6f} m

preCICE VTU Statistics (water_levels):
  Number of points: {len(vtu_wl)}
  Min:  {np.min(vtu_wl):.6f} m
  Max:  {np.max(vtu_wl):.6f} m
  Mean: {np.mean(vtu_wl):.6f} m
  Std:  {np.std(vtu_wl):.6f} m

Comparison:
  Mean difference:     {np.mean(nc_wl) - np.mean(vtu_wl):.6e} m
  Max absolute diff:   {np.max(np.abs(nc_wl - vtu_wl)):.6e} m
  RMS difference:      {np.sqrt(np.mean((nc_wl - vtu_wl)**2)):.6e} m
  Correlation coeff:   {np.corrcoef(nc_wl, vtu_wl)[0, 1]:.6f}
        """
        
        axes[1, 1].text(0.05, 0.5, stats_text, fontsize=10, 
                       family='monospace', verticalalignment='center',
                       transform=axes[1, 1].transAxes)
        axes[1, 1].axis('off')
        
        plt.tight_layout()
        
        if save_as:
            output_path = self.output_dir / save_as
            plt.savefig(output_path, dpi=150, bbox_inches='tight')
            print(f"Saved: {output_path}")
            plt.close()
        else:
            plt.show()
    
    def plot_difference_map(self, netcdf_data, vtu_data, save_as=None):
        """
        Plot spatial difference map between NetCDF and VTU water levels.
        
        Args:
            netcdf_data: Dict with 'points' and 'water_levels' from NetCDF
            vtu_data: Dict with 'points' and 'water_levels' from VTU
            save_as: Filename to save plot
        """
        # Check if grids are identical
        if netcdf_data['n_points'] != vtu_data['n_points']:
            print(f"Warning: Grid sizes differ (NetCDF: {netcdf_data['n_points']}, VTU: {vtu_data['n_points']})")
            print("Cannot create point-by-point difference map.")
            return
        
        # Check if coordinates match
        coord_diff = np.max(np.abs(netcdf_data['points'] - vtu_data['points']))
        if coord_diff > 1e-6:
            print(f"Warning: Coordinates differ by up to {coord_diff:.3e} m")
            print("Point ordering may not match. Difference map may be misleading.")
        
        # Compute difference
        diff = netcdf_data['water_levels'] - vtu_data['water_levels']
        
        fig, axes = plt.subplots(1, 2, figsize=(20, 8))
        
        # Difference map (scatter plot at grid points)
        x, y = netcdf_data['points'][:, 0], netcdf_data['points'][:, 1]
        
        sc = axes[0].scatter(x, y, c=diff, s=40, cmap='RdBu_r', 
                            edgecolors='black', linewidth=0.5)
        axes[0].set_title('Water Level Difference (NetCDF - VTU)')
        axes[0].set_xlabel('X [m]')
        axes[0].set_ylabel('Y [m]')
        axes[0].set_aspect('equal')
        axes[0].grid(True, alpha=0.3)
        plt.colorbar(sc, ax=axes[0], label='Difference [m]')
        
        # Histogram of differences
        axes[1].hist(diff, bins=50, color='green', alpha=0.7, edgecolor='black')
        axes[1].axvline(0, color='red', linestyle='--', linewidth=2, label='Zero difference')
        axes[1].set_xlabel('Difference [m]')
        axes[1].set_ylabel('Frequency')
        axes[1].set_title('Distribution of Differences')
        axes[1].legend()
        axes[1].grid(True, alpha=0.3)
        
        # Add statistics text
        stats_text = f'Mean: {np.mean(diff):.3e} m\nStd: {np.std(diff):.3e} m\nMax: {np.max(np.abs(diff)):.3e} m'
        axes[1].text(0.02, 0.98, stats_text, transform=axes[1].transAxes,
                    verticalalignment='top', bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
        
        plt.tight_layout()
        
        if save_as:
            output_path = self.output_dir / save_as
            plt.savefig(output_path, dpi=150, bbox_inches='tight')
            print(f"Saved: {output_path}")
            plt.close()
        else:
            plt.show()


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description='Compare water levels from NetCDF COM file vs preCICE VTU export',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Compare timestep 1 from NetCDF with dt1 VTU file
  python compare_netcdf_precice.py \\
      --netcdf-file f34_com.nc \\
      --vtu-file fm_flow_nodes-fm.dt1.vtu \\
      --netcdf-timestep 1

  # Specify output directory
  python compare_netcdf_precice.py \\
      --netcdf-file f34_com.nc \\
      --vtu-file fm_flow_nodes-fm.dt1.vtu \\
      --netcdf-timestep 1 \\
      --output-dir ./comparison_results
        """
    )
    parser.add_argument(
        '--netcdf-file',
        type=str,
        required=True,
        help='Path to NetCDF COM file (e.g., f34_com.nc)'
    )
    parser.add_argument(
        '--vtu-file',
        type=str,
        required=True,
        help='Path to preCICE FM VTU file (e.g., fm_flow_nodes-fm.dt1.vtu)'
    )
    parser.add_argument(
        '--wave-vtu-file',
        type=str,
        help='Path to preCICE SWAN/Wave VTU file (e.g., com_mesh-wave.dt1.vtu)'
    )
    parser.add_argument(
        '--netcdf-timestep',
        type=int,
        default=0,
        help='Timestep index in NetCDF file (0-based, default: 0)'
    )
    parser.add_argument(
        '--output-dir',
        type=str,
        default='comparison_plots',
        help='Output directory for plots (default: comparison_plots)'
    )
    
    args = parser.parse_args()
    
    # Check dependencies
    if not HAS_NETCDF:
        print("ERROR: netCDF4 is required but not installed.")
        print("Install with: pip install netCDF4")
        sys.exit(1)
    
    if not (HAS_PYVISTA or HAS_MESHIO):
        print("ERROR: Either PyVista or meshio must be installed.")
        print("Install with: pip install pyvista  OR  pip install meshio")
        sys.exit(1)
    
    print("=" * 60)
    print("NetCDF COM vs preCICE VTU Comparison")
    print("=" * 60)
    
    # Read NetCDF data
    print(f"\n1. Reading NetCDF file: {args.netcdf_file}")
    nc_reader = NetCDFCOMReader(args.netcdf_file)
    
    with nc_reader as reader:
        # Print file info
        dims = reader.get_dimensions()
        print(f"   Dimensions: {dims}")
        
        time_info = reader.get_time_info()
        print(f"   Time steps: {time_info['n_timesteps']}")
        print(f"   Time units: {time_info['units']}")
        
        # Read water levels at specified timestep
        print(f"\n   Reading water levels at timestep {args.netcdf_timestep}...")
        nc_data = reader.read_water_levels(args.netcdf_timestep)
        print(f"   - Number of points: {nc_data['n_points']}")
        print(f"   - Time value: {nc_data['time']} s")
        print(f"   - Water level range: [{np.min(nc_data['water_levels']):.3f}, {np.max(nc_data['water_levels']):.3f}] m")
        
        # Read bed levels
        print(f"\n   Reading bed levels...")
        nc_bed_data = reader.read_bed_levels()
        print(f"   - Number of points: {nc_bed_data['n_points']}")
        print(f"   - Bed level range: [{np.min(nc_bed_data['bed_levels']):.3f}, {np.max(nc_bed_data['bed_levels']):.3f}] m")
    
    # Read FM VTU data
    print(f"\n2. Reading FM VTU file: {args.vtu_file}")
    fm_vtu_reader = VTUReader(args.vtu_file)
    fm_vtu_full = fm_vtu_reader.read()
    vtu_data = fm_vtu_reader.extract_water_levels()
    print(f"   - Number of points: {vtu_data['n_points']}")
    print(f"   - Number of cells: {fm_vtu_full.get('n_cells', 0)}")
    if fm_vtu_full.get('cells') is not None:
        print(f"   - Number of triangles: {len(fm_vtu_full['cells'])}")
    print(f"   - Water level range: [{np.min(vtu_data['water_levels']):.3f}, {np.max(vtu_data['water_levels']):.3f}] m")
    
    # Read bed levels from VTU
    print(f"\n   Reading bed levels from VTU...")
    vtu_bed_data = fm_vtu_reader.extract_bed_levels()
    print(f"   - Bed level range: [{np.min(vtu_bed_data['bed_levels']):.3f}, {np.max(vtu_bed_data['bed_levels']):.3f}] m")
    
    # Read SWAN/Wave VTU data if provided
    wave_vtu_full = None
    if args.wave_vtu_file:
        print(f"\n   Reading SWAN/Wave VTU file: {args.wave_vtu_file}")
        wave_vtu_reader = VTUReader(args.wave_vtu_file)
        wave_vtu_full = wave_vtu_reader.read()
        print(f"   - Number of points: {wave_vtu_full['n_points']}")
        print(f"   - Number of cells: {wave_vtu_full.get('n_cells', 0)}")
    
    # Compare
    print("\n3. Generating comparison plots...")
    comparer = WaterLevelComparer(args.output_dir)
    
    # Plot grid comparison (FM vs SWAN from VTU files)
    print("   - Plotting grid comparison...")
    comparer.plot_grid_comparison(
        fm_vtu_full,
        wave_vtu_full,
        save_as='grid_comparison.png'
    )
    
    # Plot bed level comparison
    print("   - Plotting bed level comparison...")
    comparer.plot_bed_level_comparison(
        nc_bed_data,
        vtu_bed_data,
        save_as='bed_level_comparison.png'
    )
    
    # Plot water level comparison
    print("   - Plotting water level comparison...")
    comparer.plot_comparison(
        nc_data,
        vtu_data,
        save_as='water_level_comparison.png'
    )
    
    # Plot difference map
    print("   - Plotting difference map...")
    comparer.plot_difference_map(
        nc_data,
        vtu_data,
        save_as='water_level_difference.png'
    )
    
    print("\n" + "=" * 60)
    print(f"Comparison complete! Plots saved to: {args.output_dir}")
    print("=" * 60)


if __name__ == '__main__':
    main()
