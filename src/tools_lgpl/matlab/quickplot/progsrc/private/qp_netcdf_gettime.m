function [Data,TZshift] = qp_netcdf_gettime(FI,var,index)
%QP_NETCDF_GETTIME Get time data from netcdf file.
%   DATA = QP_NETCDF_GETTIME(FILE,VAR,REQIND) reads data for the
%   time variable VAR and converts it to MATLAB date/time float.

%----- LGPL --------------------------------------------------------------------
%
%   Copyright (C) 2011-2024 Stichting Deltares.
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

if isempty(var)
    error('Variable name is empty.')
elseif ischar(var)
    varstr = var;
    varid = find(strcmp(varstr,{FI.Dataset.Name})) - 1;
    if isempty(varid)
        error('Variable "%s" not found in file.',varstr)
    end
elseif isstruct(var)
    varid = var.Varid;
else
    varid = var;
end
%
Info = FI.Dataset(varid+1);
if nargin<3 || isequal(index,0) || isequal(index,':')
    [Data,errmsg] = qp_netcdf_get(FI,var);
else
    [Data,errmsg] = qp_netcdf_get(FI,var,Info.Dimension,{index});
end
if ~isempty(errmsg)
    error(errmsg)
end
%
iUnits = strcmp('units',{Info.Attribute.Name});
if ~any(iUnits)
    error('No units found on "%s".',var);
end
%
unitStr = Info.Attribute(iUnits).Value;
[refdate,dt,TZshift] = nc_interpret_time_unit(unitStr);
%
Data = refdate + Data*dt;