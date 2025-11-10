\page EC_module_dot_diagrams EC-Module Documentation Diagrams
This page describes the DOT diagrams used in EC-Module documentation.

# EC-Module Documentation Diagrams

This directory contains 4 DOT files for generating professional diagrams in Doxygen documentation of the EC-Module in Delft3D.

## Files Overview

### 1. `ec_module_architecture.dot`
\dotfile ec_module_architecture.dot "System Architecture"
**Purpose**: Shows the overall architecture and role of EC-Module within the Delft3D suite
**Key Features**:
- Central data exchange hub
- Connections to D-Flow FM, D-Water Quality, D-Morphology, D-Waves
- Infrastructure components (Grid Geometry, NetCDF I/O)
- **Important**: Shows that preCICE integration is currently only in D-Waves (not EC-Module)

### 2. `ec_module_dataflow.dot`
\dotfile ec_module_dataflow.dot "Data Processing Pipeline"
**Purpose**: Illustrates the complete data processing pipeline within EC-Module
**Key Features**:
- Data sources (NetCDF, ASCII, BC files)
- File readers and parsers
- EC-Module core processing
- Spatial interpolation methods
- Target model outputs
- Time synchronization

### 3. `ec_module_dependencies.dot`
\dotfile ec_module_dependencies.dot "Dependency Structure"
**Purpose**: Shows the hierarchical dependency structure
**Key Features**:
- Four-level dependency hierarchy
- Core dependencies (deltares_common, gridgeom, kdtree2, io_netcdf)
- Implementation libraries (precision, netcdf-fortran, string_module)
- System libraries (NetCDF-C, HDF5, MPI, BLAS/LAPACK)

### 4. `ec_module_interpolation.dot`
\dotfile ec_module_interpolation.dot "Interpolation Algorithms"
**Purpose**: Detailed view of spatial and temporal interpolation algorithms
**Key Features**:
- Input data types (point, grid, time series)
- Spatial methods (triangulation, bilinear, nearest neighbor, averaging)
- Temporal methods (linear, block, harmonic)
- Coordinate systems (Cartesian, spherical, curvilinear)
- Core algorithms and target applications

### 5. `ec_module_deep_integration.dot` 
\dotfile ec_module_deep_integration.dot "Instance Management and Memory Sharing"
**Purpose**: Deep integration patterns showing how EC-Module is instantiated and integrated within D-Flow FM
**Key Features**:
- **Singleton Pattern**: Single EC instance (`ecInstancePtr`) per D-Flow FM process
- **Global Data Structures**: Direct memory sharing with flow arrays (no data copying)
- **Memory References**: C_LOC pointers for real-time updates to flow variables
- **DIMR Orchestration**: Component management and process coordination
- **Shared Grid Geometry**: Coordinate system setup and spatial coupling
- **Runtime Patterns**: Update cycles, boundary synchronization, temporal coordination

## Usage in Doxygen

### Method 1: Inline DOT graphs
Add directly to your Doxygen comments:

```
/**
 * @brief EC-Module Architecture Overview
 * 
 * @dot
 * digraph ec_architecture {
 *     // Copy content from ec_module_architecture.dot here
 *     // (remove the outer digraph declaration)
 * }
 * @enddot
 */
```

### Method 2: External DOT files
Reference external files in Doxygen:

```
/**
 * @brief EC-Module Architecture
 * @dotfile ec_module_architecture.dot "EC-Module Architecture"
 */
```

### Method 3: Include in documentation pages
Add to your main documentation page:

```
/**
 * @mainpage EC-Module Documentation
 * 
 * @section architecture Architecture Overview
 * @dotfile ec_module_architecture.dot "System Architecture"
 * 
 * @section integration Deep Integration Patterns
 * @dotfile ec_module_deep_integration.dot "Instance Management and Memory Sharing"
 * 
 * @section dataflow Data Flow
 * @dotfile ec_module_dataflow.dot "Data Processing Pipeline"
 * 
 * @section dependencies Dependencies
 * @dotfile ec_module_dependencies.dot "Dependency Structure"
 * 
 * @section interpolation Interpolation Methods
 * @dotfile ec_module_interpolation.dot "Interpolation Algorithms"
 */
```

## Doxygen Configuration

Ensure your Doxyfile has these settings:

```
HAVE_DOT = YES
DOT_PATH = /path/to/dot
CALL_GRAPH = YES
CALLER_GRAPH = YES
DOT_IMAGE_FORMAT = svg
DOT_TRANSPARENT = YES
DOT_MULTI_TARGETS = YES
```

## Customization

Each DOT file includes:
- **Color coding** for different component types
- **Clustering** for logical grouping
- **Multiple edge styles** (solid, dashed, dotted)
- **Shape variety** (boxes, records, ellipses)
- **Comprehensive labels** with detailed information

You can modify:
- Colors by changing `fillcolor` attributes
- Layout by adjusting `rankdir` (TB, LR, BT, RL)
- Node shapes using `shape` attribute
- Edge styles using `style` attribute
- Clustering by modifying `subgraph cluster_*` sections

## Current State Accuracy

These diagrams reflect the **current implementation** as of the analysis:
- ✅ **EC-Module serves as central data exchange hub**
- ✅ **Strong integration with all Delft3D components**
- ✅ **Comprehensive interpolation infrastructure**
- ⚠️  **preCICE integration is currently only in D-Waves module, not EC-Module**
- ✅ **Modular architecture with clear dependency hierarchy**
- ✅ **Singleton pattern**: One `ecInstancePtr` per D-Flow FM process
- ✅ **Direct memory sharing**: No data copying, uses C_LOC pointers
- ✅ **DIMR orchestration**: Component management at process level
- ✅ **Shared grid geometry**: Coordinate system and spatial coupling
- ✅ **Real-time updates**: Direct array updates through pointer references

## Generated Formats

Doxygen with Graphviz can generate these diagrams in multiple formats:
- **SVG** (recommended for web documentation)
- **PNG** (for PDF documentation)  
- **EPS** (for LaTeX documentation)
- **PDF** (for standalone diagrams)

The diagrams are optimized for professional technical documentation and provide comprehensive visual representation of the EC-Module's role and implementation within the Delft3D suite.