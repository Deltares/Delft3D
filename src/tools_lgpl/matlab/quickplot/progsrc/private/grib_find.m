function BLOCKNR = grib_find(FI,varargin)
%GRIB_FIND Get block index from meta-info
%
%   BLOCKNR = grib_find(FI,<BLOCKNRS>,<name,value>)
%
%   finds block indices BLOCKNR that match specified 
%   <name,value> combinations. When BLOCKNRS is 
%   specified, only a subset from BLOCKNRS is returned.
%   Multiple <name,value> pairs can be supplied, or 
%   use INTERSECT after multiple grib_block calls.
%   The names refer to numeric or character fieldnames 
%   in the FI.Block(:).Info struct.
%
%   Example:
%
%        FI = grib('open','HIRLAM_SURFACE_1997010200.grib')
%     ind.p = grib('find',FI,'ParamID', 1,'LevelID',105,'P1',0,'OK',1);
%     ind.u = grib('find',FI,'ParamID',33,'LevelID',105,'P1',0,'OK',1);
%     ind.v = grib('find',FI,'ParamID',34,'LevelID',105,'P1',0,'OK',1);
%
%             grib('list',FI,[ind.p ind.u ind.v]);
%
%     [D.p,D.lat,D.lon,D.t] = grib('read',FI,ind.p);
%     [U.u,U.lat,U.lon,U.t] = grib('read',FI,ind.u); % NB staggered grid
%     [V.v,V.lat,V.lon,V.t] = grib('read',FI,ind.v); % NB staggered grid
%
%     pcolor(D.lon,D.lat,D.p);shading interp
%     hold on
%     quiver(D.lon,D.lat,.1.*U.u,.1.*V.v,0,'k'); % neglected staggering & global velocity orientation.
%     title(datestr(D.t))
%
%   See also: GRIB, GRIB_DISP

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

%% Initialize all requiremens

 KEY.Message               = [];  % : 
 KEY.GribTablesVersion     = [];  % : 1
 KEY.CentreID              = [];  % : 96
 KEY.CentreName            = [];  % : Athens
 KEY.ProcessID             = [];  % : 1
 KEY.ParamID               = [];  % : 91
 KEY.LevelID               = [];  % : 102
 KEY.LevelName             = [];  % : Mean sea level
%KEY.Level.Description     = [];  % : []
%KEY.Level.Value           = [];  % : []
 KEY.TimeUnitID            = [];  % : 1
 KEY.TimeUnitName          = [];  % : Hour
 KEY.TimeRangeID           = [];  % : 0
 KEY.TimeRangeDescription  = [];  % : Forecast product valid for reference time + P1 (P1 > 0), or Uninitialized analysis product for reference time (P1 = 0), or Image product for reference time (P1 = 0)
 KEY.P1                    = [];  % : 48
 KEY.Date                  = [];  % : 31-Jan-1997 18:00:00
 KEY.SubCentreID           = [];  % : 0
 KEY.ScaleFactorD          = [];  % : 1
 KEY.ParamName             = [];  % : Ice cover (1 = ice, 0 = no ice)
 KEY.ParamUnit             = [];  % : Proportion
 KEY.PackingID             = [];  % : 0
 KEY.PackingName           = [];  % : Grid Point - Simple Packing
 KEY.ValueType             = [];  % : Floating Point
 KEY.ScaleFactorE          = [];  % : 6.1035e-005
 KEY.MinimumValue          = [];  % : 0
 KEY.BitsPerValue          = [];  % : 15
 KEY.OK                    = [];  % : 1 

%% Take care of block range
blocks   = 1:length(FI.Block);
if mod(nargin,2)==0
   blocks  = varargin{1};
   nextarg = 2;
else
   nextarg = 1;
end

%% Take care of selection values
KEY = setProperty(KEY,varargin{nextarg:end});

fldnames = fieldnames(KEY);
for ifld = 1:length(fieldnames(KEY))
   keyword = fldnames{ifld};
   if isempty(KEY.(keyword))
      KEY = rmfield(KEY,keyword);
   end
end
fldnames = fieldnames(KEY);


%% Loop blocks
BLOCKNR = [];

for ibl = [blocks]
   %disp(['# ',num2str(ibl)])
         
   allOK = 1;      
   for ifld = 1:length(fieldnames(KEY))
      keyword = fldnames{ifld};
      value   = FI.Block(ibl).Info.(keyword);
      if ~isempty(KEY.(keyword))
            
         if isnumeric(value);
            if        (value==KEY.(keyword));
            else
            allOK = 0;
            end
         else
            if strcmpi(value==KEY.(keyword));
            else
            allOK = 0;
            end
         end
   
         %pausedisp
   
      end
         
   end

   if allOK
   BLOCKNR = [BLOCKNR ibl];
   end

end % blocks

%% EOF
