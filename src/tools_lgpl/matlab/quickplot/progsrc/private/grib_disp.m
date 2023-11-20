function varargout = grib_disp(FI,varargin)
%GRIB_DISP Display meta-info from grib file
%
%      FI = grib('open',FILENAME,<DEBUG_FID>)
%           grib_disp(FI,<BLOCKNR>,<keyword,value>)
%
%   display meta-info from grib file. When BLOCKNR is not
%   given, all blocks are displayed one by one. BLOCKNR
%   can be an array of blocks, e.g. [1 2].
%
%   The following <keyword,value> pairs are implemented:
%   * type: 'disp'  display all meta-info from grib file
%           'list'  generate list with one line per block
%           'meta'  extract values of one property, e.g. extract
%                    all P1 values: >> grib('meta',FI,BLOCKNR,'fieldname','P1');
%
%   See also: GRIB, GRIB_FIND

%----- LGPL --------------------------------------------------------------------
%
%   Copyright (C) 2011-2023 Stichting Deltares.
%
%   This library is free software; you can redistribute it and/or
%   modify it under the terms of the GNU Lesser General Public
%   License as published by the Free Software Foundation version 2.1.
%
%   This library is distributed in the hope that it will be useful,
%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%   Lesser General Public License for more details.
%
%   You should have received a copy of the GNU Lesser General Public
%   License along with this library; if not, see <http://www.gnu.org/licenses/>.
%
%   contact: delft3d.support@deltares.nl
%   Stichting Deltares
%   P.O. Box 177
%   2600 MH Delft, The Netherlands
%
%   All indications and logos of, and references to, "Delft3D" and "Deltares"
%   are registered trademarks of Stichting Deltares, and remain the property of
%   Stichting Deltares. All rights reserved.
%
%-------------------------------------------------------------------------------
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$

% TO DO: optionally write disp or list to file rather than to screen

%% input

OPT.blocks    = 1:length(FI.Block);
OPT.type      = 'disp';
OPT.pause     = 1;
OPT.fieldname = 'P1'; % for type==meta

if mod(nargin,2)==0
    OPT.blocks = varargin{1};
    nextarg = 2;
else
    nextarg = 1;
end

OPT = setProperty(OPT,varargin{nextarg:end});

%% header

if strcmpi(OPT.type,'list')
    disp('#  ParamID LevelID Level.Value P1 ParamName')
    disp('-- ------- ------- ----------- -- ---------')
elseif strcmpi(OPT.type,'meta')
    out = {[]};
end

%% display

n = 0;

for ibl = [OPT.blocks]
    
    n = n + 1;
    
    if strcmpi(OPT.type,'disp')
        
        disp(['-------------- Block   : ',num2str(ibl),' in: ''', FI.FileName,''''])
        disp(['Message                : ',   char(FI.Block(ibl).Info.Message               )]);%: {}   `
        disp(['GribTablesVersion      : ',num2str(FI.Block(ibl).Info.GribTablesVersion     )]);%: 1
        disp(['CentreID               : ',num2str(FI.Block(ibl).Info.CentreID              )]);%: 96
        disp(['CentreName             : ',        FI.Block(ibl).Info.CentreName             ]);%: 'Athens'
        disp(['ProcessID              : ',num2str(FI.Block(ibl).Info.ProcessID             )]);%: 1
        disp(['ParamID                : ',num2str(FI.Block(ibl).Info.ParamID               )]);%: 91
        disp(['LevelID                : ',num2str(FI.Block(ibl).Info.LevelID               )]);%: 102
        disp(['LevelName              : ',        FI.Block(ibl).Info.LevelName              ]);%: 'Mean sea level'
        if ~isempty(FI.Block(ibl).Info.Level)
            disp(['Level.Description      : ',num2str(FI.Block(ibl).Info.Level.Description     )]);%: []
            disp(['Level.Value            : ',num2str(FI.Block(ibl).Info.Level.Value           )]);%: []
        else
            disp( 'Level.Description      : []'                                                  );%: []
            disp( 'Level.Value            : []'                                                  );%: []
        end
        disp(['TimeUnitID             : ',num2str(FI.Block(ibl).Info.TimeUnitID            )]);%: 1
        disp(['TimeUnitName           : ',        FI.Block(ibl).Info.TimeUnitName           ]);%: 'Hour'
        disp(['TimeRangeID            : ',num2str(FI.Block(ibl).Info.TimeRangeID           )]);%: 0
        disp(['TimeRangeDescription   : ',        FI.Block(ibl).Info.TimeRangeDescription   ]);%: [1x164 char]
        disp(['P1                     : ',num2str(FI.Block(ibl).Info.P1                    )]);%: 48
        disp(['Date                   : ',datestr(FI.Block(ibl).Info.Date                  )]);%: 7.2942e+005
        disp(['SubCentreID            : ',num2str(FI.Block(ibl).Info.SubCentreID           )]);%: 0
        disp(['ScaleFactorD           : ',num2str(FI.Block(ibl).Info.ScaleFactorD          )]);%: 1
        disp(['ParamName              : ',        FI.Block(ibl).Info.ParamName              ]);%: 'Ice cover (1 = ice, 0 = no ice)'
        disp(['ParamUnit              : ',        FI.Block(ibl).Info.ParamUnit              ]);%: 'Proportion'
        disp(['PackingID              : ',num2str(FI.Block(ibl).Info.PackingID             )]);%: 0
        disp(['PackingName            : ',        FI.Block(ibl).Info.PackingName            ]);%: 'Grid Point - Simple Packing'
        disp(['ValueType              : ',        FI.Block(ibl).Info.ValueType              ]);%: 'Floating Point'
        disp(['ScaleFactorE           : ',num2str(FI.Block(ibl).Info.ScaleFactorE          )]);%: 6.1035e-005
        disp(['MinimumValue           : ',num2str(FI.Block(ibl).Info.MinimumValue          )]);%: 0
        disp(['BitsPerValue           : ',num2str(FI.Block(ibl).Info.BitsPerValue          )]);%: 15
        disp(['OK                     : ',num2str(FI.Block(ibl).Info.OK                    )]);%: 1
        
        disp(['Grid.TypeID            : ',num2str(FI.Block(ibl).Info.Grid.TypeID            )]);%:10
        disp(['Grid.TypeName          : ',        FI.Block(ibl).Info.Grid.TypeName           ]);%:'Rotated latitude/longitude grid'
        disp(['Grid.BaseTypeID        : ',num2str(FI.Block(ibl).Info.Grid.BaseTypeID        )]);%:0
        disp(['Grid.Rotated           : ',num2str(FI.Block(ibl).Info.Grid.Rotated           )]);%:1
        disp(['Grid.Stretched         : ',num2str(FI.Block(ibl).Info.Grid.Stretched         )]);%:0
        disp(['Grid.SpaceView         : ',num2str(FI.Block(ibl).Info.Grid.SpaceView         )]);%:0
        disp(['Grid.Ni                : ',num2str(FI.Block(ibl).Info.Grid.Ni                )]);%:53
        disp(['Grid.Nj                : ',num2str(FI.Block(ibl).Info.Grid.Nj                )]);%:53
        disp(['Grid.Latitude1         : ',num2str(FI.Block(ibl).Info.Grid.Latitude1         )]);%:-22000
        disp(['Grid.Longitude1        : ',num2str(FI.Block(ibl).Info.Grid.Longitude1        )]);%:-3000
        disp(['Grid.DiGiven           : ',num2str(FI.Block(ibl).Info.Grid.DiGiven           )]);%:1
        disp(['Grid.EarthID           : ',num2str(FI.Block(ibl).Info.Grid.EarthID           )]);%:0
        disp(['Grid.EarthDescription  : ',        FI.Block(ibl).Info.Grid.EarthDescription   ]);%:'Spherical R=6367.47 km'
        disp(['Grid.VectorDir         : ',        FI.Block(ibl).Info.Grid.VectorDir          ]);%:'I-J'
        disp(['Grid.Latitude2         : ',num2str(FI.Block(ibl).Info.Grid.Latitude2         )]);%:4000
        disp(['Grid.Longitude2        : ',num2str(FI.Block(ibl).Info.Grid.Longitude2        )]);%:23000
        disp(['Grid.Di                : ',num2str(FI.Block(ibl).Info.Grid.Di                )]);%:500
        disp(['Grid.Dj                : ',num2str(FI.Block(ibl).Info.Grid.Dj                )]);%:500
        disp(['Grid.PointScanPositiveI: ',num2str(FI.Block(ibl).Info.Grid.PointScanPositiveI)]);%:0
        disp(['Grid.PointScanPositiveJ: ',num2str(FI.Block(ibl).Info.Grid.PointScanPositiveJ)]);%:0
        disp(['Grid.PointScanOrder    : ',        FI.Block(ibl).Info.Grid.PointScanOrder     ]);%:'IJ'
        
        if FI.Block(ibl).Info.Grid.Rotated
            disp(['Grid.LatitudeSP        : ',num2str(FI.Block(ibl).Info.Grid.LatitudeSP        )]);%:-30000
            disp(['Grid.LongitudeSP       : ',num2str(FI.Block(ibl).Info.Grid.LongitudeSP       )]);%:-15000
            disp(['Grid.Rotation          : ',num2str(FI.Block(ibl).Info.Grid.Rotation          )]);%:0
            disp(['Grid.VCoord            : ',num2str(FI.Block(ibl).Info.Grid.VCoord(:)'        )]);%:[2x1 double]
            disp(['Grid.BitMaskID         : ',num2str(FI.Block(ibl).Info.Grid.BitMaskID         )]);%:0
        end
        
        if length([OPT.blocks])>1 && OPT.pause
            pausedisp
        end
        
    elseif strcmpi(OPT.type,'list')
        
        if ~isempty(FI.Block(ibl).Info.Level)
            disp([num2str(ibl,'%0.3d'),' ',...
                num2str(FI.Block(ibl).Info.ParamID     ,'%0.3d'),'    ',...
                num2str(FI.Block(ibl).Info.LevelID     ,'%0.3d'),'     ',...
                num2str(FI.Block(ibl).Info.Level.Value ,'%0.3d'),'         ',...
                num2str(FI.Block(ibl).Info.P1          ,'%0.2d'),' ',...
                FI.Block(ibl).Info.ParamName   ]);
        else
            disp([num2str(ibl,'%0.3d'),' ',...
                num2str(FI.Block(ibl).Info.ParamID     ,'%0.3d'),'    ',...
                num2str(FI.Block(ibl).Info.LevelID     ,'%0.3d'),'       -         ',...
                num2str(FI.Block(ibl).Info.P1          ,'%0.2d'),' ',...
                FI.Block(ibl).Info.ParamName   ]);
        end
        
    elseif strcmpi(OPT.type,'meta')
        
        out{n} = FI.Block(ibl).Info.(OPT.fieldname);
        
    end
    
end % bl

if strcmpi(OPT.type,'meta')

    if isnumeric(out{1}) | isstruct(out{1});
    out = cell2mat(out);
    end
    varargout = {out};
   
end

