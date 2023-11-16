%Locations
%   * Observation points/Cross Section Scalar
%     id+z -> VZ<T>, TV, IdT(V),IdTV,IdV<T> plots allowed, V<T> table allowed
%     xy+z -> XY(V)<T>, XYZ(V)<T> plots allowed
%   * Transects/Ray
%     xy+z  (0D, 1DH, 2DV)
%     id+z  (0D, 1DH, 2DV)
%   * Grid/Mesh (optionally multi domain)
%     xy+z -> XYZ(V)<T>,VZ<T>,dV<T>,TV
%     xyz   (3D)
%   * Network
%     xy+z  (1DH, 2DV)
%     xyz   (1D)
%
%===============================================
Data.Grid.Type  = 'Struct3D';

Data.Grid.XDim  = {'M' 'N' 'K'};
Data.Grid.X     = '[...data...]';
Data.Grid.XUnit = 'm';

Data.Grid.YDim  = {'M' 'N' 'K'};
Data.Grid.Y     = '[...data...]';
Data.Grid.YUnit = 'm';

Data.Grid.ZDim  = {'M' 'N' 'K'};
Data.Grid.Z     = '[...data...]';
Data.Grid.ZUnit = 'm';

Struct3DLocations = {'Voxels3D', 'Faces3D', 'Lines3D', 'Points3D'};
%-----------------------------------------------
%uncommon (only in encountered CFX/Fluent)
%===============================================
Data.Grid.Type  = 'Struct2D+';

Data.Grid.XDim  = {'M' 'N'};
Data.Grid.X     = '[...data...]';
Data.Grid.XUnit = 'm';

Data.Grid.YDim  = {'M' 'N'};
Data.Grid.Y     = '[...data...]';
Data.Grid.YUnit = 'm';

Data.Grid.ZDim  = {'M' 'N' 'Time'}; %or {'M' 'N' 'K' 'Time'}
Data.Grid.Z     = '[...data...]';
Data.Grid.ZUnit = 'm';

Data.Grid.ADim  = {'M' 'N'};
Data.Grid.Aggr  = '[...data...]';

Struct2DLocations = {'Voxels3D', ...
    'Faces3D','HFaces3D','VFaces3D', ...
    'Lines3D','HLines3D','VLines3D', ...
    'Points3D', ...
    'Faces2D','Lines2D','Points2D'};
%-----------------------------------------------
%horizontal slice/vertical average: Struct2D
%vertical slice: Unstruct1D+
%point: Unstruct0D+
%subdomain: Struct2D+, or Unstruct2D+
%isosurf: Unstruct2D
%===============================================
Data.Grid.Type  = 'Unstruct3D';

%-----------------------------------------------
%uncommon therefore not yet defined (CFX/Fluent)
%===============================================
Data.Grid.Type  = 'Unstruct2D+';

Data.Grid.XDim  = {'NPnt'};
Data.Grid.X     = '[...data...]';
Data.Grid.XUnit = 'm';

Data.Grid.YDim  = {'NPnt'};
Data.Grid.Y     = '[...data...]';
Data.Grid.YUnit = 'm';

Data.Grid.ZDim  = {'NCell' 'K' 'Time'};
Data.Grid.Z     = '[...data...]';
Data.Grid.ZUnit = 'm';

Data.Grid.NPntDim  = {'NCell'};
Data.Grid.NPnt     = '[...data...]';
Data.Grid.PntIdDim = {'NPntCell'};
Data.Grid.PntId    = '[...data...]';

Data.Grid.ADim  = {'NCell'};
Data.Grid.Aggr  = '[...data...]';

Unstruct2DLocations = {'VL','FXY','FZ','ND','VL2D','FXY2D','ND2D'};
%-----------------------------------------------
%horizontal slice/vertical average: Unstruct2D
%vertical slice: Unstruct1D+
%point: Unstruct0D+
%subdomain: Unstruct2D+
%isosurf: Unstruct2D
%===============================================
Data.Grid.Type  = 'Unstruct0D+';

Data.Grid.XDim  = {'NPnt'}; %or {'NPnt' 'Time'}
Data.Grid.X     = '[...data...]';
Data.Grid.XUnit = 'm';

Data.Grid.YDim  = {'NPnt'}; %or {'NPnt' 'Time'}
Data.Grid.Y     = '[...data...]';
Data.Grid.YUnit = 'm';

Data.Grid.ZDim  = {'NPnt' 'Time'}; %or {'NPnt' 'K' 'Time'}
Data.Grid.Z     = '[...data...]';
Data.Grid.ZUnit = 'm';


Unstruct0DLocations = {'VLines3D','Points3D', ...
    'Points2D'};
%-----------------------------------------------
%horizontal slice/vertical average: Unstruct0D
%vertical slice: <none>
%point: <none>
%subdomain: Unstruct0D+
%isosurf: Unstruct0D
%===============================================
Data.Grid.Type  = 'Unstruct1D+';

Data.Grid.XDim  = {'NPnt'};
Data.Grid.X     = '[...data...]';
Data.Grid.XUnit = 'm';

Data.Grid.YDim  = {'NPnt'};
Data.Grid.Y     = '[...data...]';
Data.Grid.YUnit = 'm';

Data.Grid.ZDim  = {'NPnt' 'Time'}; %or {'NPnt' 'K' 'Time'}
Data.Grid.Z     = '[...data...]';
Data.Grid.ZUnit = 'm';

Data.Grid.SegDim = {'NSeg' 2};
Data.Grid.Seg    = '[...data...]';

Unstruct1DLocations = {'Faces3D', ...
    'Lines3D','HLines3D','VLines3D', ...
    'Points3D', ...
    'Lines2D','Points2D'};
%-----------------------------------------------
%horizontal slice/vertical average: Unstruct1D
%vertical slice: <none>
%point: Unstruct0D+
%subdomain: Unstruct1D+
%isosurf: Unstruct1D
%===============================================

Data.Name = 'velocity';
Data.Unit = 'm/s';

Data.Value(1).Dimensions = {'M' 'N' 'K' 'Time'};
Data.Value(1).Data       = '[...data...]';
Data.Value(1).LocDefined = 'FM';

Data.Value(2).Dimensions = {'M' 'N' 'K' 'Time'};
Data.Value(2).Data       = '[...data...]';
Data.Value(2).LocDefined = 'FN';

Data.Dimensions(1).Name   = 'Time';
Data.Dimensions(1).Values = '[...data...]';
Data.Dimensions(1).Unit   = 's';

Data.Dimensions(2).Name   = 'M';
Data.Dimensions(2).Values = '[...data...]';
Data.Dimensions(2).Unit   = '-';

Data.Dimensions(3).Name   = 'N';
Data.Dimensions(3).Values = '[...data...]';
Data.Dimensions(3).Unit   = '-';

Data.Dimensions(4).Name   = 'K';
Data.Dimensions(4).Values = '[...data...]';
Data.Dimensions(4).Unit   = '-';

Data.Dimensions(5).Name   = 'Freq';
Data.Dimensions(5).Values = '[...data...]';
Data.Dimensions(5).Unit   = '1/s';
