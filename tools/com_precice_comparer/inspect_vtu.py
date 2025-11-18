#!/usr/bin/env python3
"""
Simple example script showing how to inspect preCICE VTU exports.
This can help you understand the structure before running the full comparison.
"""

import sys
from pathlib import Path

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


def inspect_vtu_file(vtu_path):
    """Inspect and print information about a VTU file."""
    vtu_path = Path(vtu_path)
    
    if not vtu_path.exists():
        print(f"ERROR: File not found: {vtu_path}")
        return
    
    print("=" * 70)
    print(f"Inspecting VTU file: {vtu_path.name}")
    print("=" * 70)
    
    if HAS_PYVISTA:
        print("\nUsing PyVista to read the file...\n")
        mesh = pv.read(vtu_path)
        
        print(f"Number of points: {mesh.n_points}")
        print(f"Number of cells: {mesh.n_cells}")
        print(f"Bounds (X, Y, Z): {mesh.bounds}")
        
        print("\n--- Point Data Arrays ---")
        if mesh.point_data:
            for name, data in mesh.point_data.items():
                print(f"  {name}:")
                print(f"    Shape: {data.shape}")
                print(f"    Min: {data.min():.6e}")
                print(f"    Max: {data.max():.6e}")
                print(f"    Mean: {data.mean():.6e}")
        else:
            print("  (No point data)")
        
        print("\n--- Cell Data Arrays ---")
        if mesh.cell_data:
            for name, data in mesh.cell_data.items():
                print(f"  {name}:")
                print(f"    Shape: {data.shape}")
                print(f"    Min: {data.min():.6e}")
                print(f"    Max: {data.max():.6e}")
                print(f"    Mean: {data.mean():.6e}")
        else:
            print("  (No cell data)")
        
        print("\n--- Cell Types ---")
        print(f"  {mesh.get_cell(0).type}")
        
    elif HAS_MESHIO:
        print("\nUsing meshio to read the file...\n")
        mesh = meshio.read(vtu_path)
        
        print(f"Number of points: {len(mesh.points)}")
        print(f"Number of cells: {sum(len(c.data) for c in mesh.cells)}")
        
        print("\n--- Point Data Arrays ---")
        if mesh.point_data:
            for name, data in mesh.point_data.items():
                print(f"  {name}:")
                print(f"    Shape: {data.shape}")
                print(f"    Min: {data.min():.6e}")
                print(f"    Max: {data.max():.6e}")
                print(f"    Mean: {data.mean():.6e}")
        else:
            print("  (No point data)")
        
        print("\n--- Cell Data Arrays ---")
        if mesh.cell_data:
            for name, data_list in mesh.cell_data.items():
                print(f"  {name}: {len(data_list)} cell blocks")
        else:
            print("  (No cell data)")
        
        print("\n--- Cell Types ---")
        for cell_block in mesh.cells:
            print(f"  {cell_block.type}: {len(cell_block.data)} cells")
    
    else:
        print("ERROR: Neither PyVista nor meshio is installed.")
        print("Install with: pip install pyvista")
        return
    
    print("\n" + "=" * 70)


def inspect_directory(directory):
    """Inspect all VTU files in a directory."""
    directory = Path(directory)
    
    if not directory.exists():
        print(f"ERROR: Directory not found: {directory}")
        return
    
    vtu_files = sorted(directory.glob("*.vtu"))
    
    if not vtu_files:
        print(f"No VTU files found in: {directory}")
        return
    
    print(f"\nFound {len(vtu_files)} VTU files:")
    for i, f in enumerate(vtu_files, 1):
        print(f"  {i}. {f.name}")
    
    print("\n")
    
    # Inspect first file as example
    if vtu_files:
        inspect_vtu_file(vtu_files[0])


def main():
    """Main entry point."""
    if len(sys.argv) < 2:
        print("Usage:")
        print("  python inspect_vtu.py <file.vtu>")
        print("  python inspect_vtu.py <directory>")
        print("\nExample:")
        print("  python inspect_vtu.py precice-exports/")
        print("  python inspect_vtu.py precice-exports/fm-mesh-fm_flow_nodes.dt0.vtu")
        sys.exit(1)
    
    path = Path(sys.argv[1])
    
    if path.is_file():
        inspect_vtu_file(path)
    elif path.is_dir():
        inspect_directory(path)
    else:
        print(f"ERROR: Path not found: {path}")
        sys.exit(1)


if __name__ == '__main__':
    main()
