function [Resource,Key] = expand(Key,Data)
%EXPAND Creates resource structure from Key/FileName

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

ResourceList = {'*' 'getdata_wrapper'};
%#function getdata_wrapper

%% First get function handles and (raw)data for the resource ...
DefaultFunctions = getDefaultFunctions;
if iscellstr(Key)
   %
   % Only the name (and possibly other characteristic strings) given but no
   % resource type. Therefore, we have to try all resource routines
   % specified above until we have found the resource type that matches the
   % one we are trying to access.
   %
   FileName_etc = Key;
   for i=1:size(ResourceList,1)
      Functions = feval(ResourceList{i,2},DefaultFunctions);
      try
         [Data,Key] = feval(Functions.open,FileName_etc{:});
         if ~isempty(Data)
            break
         end
      catch
         Data=[];
      end
   end
   if isempty(Data)
      error('Opening resource failed')
   end
elseif iscell(Key)
    Functions = feval(ResourceList{end,2},DefaultFunctions);
    Data = Key{1};
    Key = [];
    Key.Name = '<unknown>';
else
   %
   % Key specified including resource type: find appropriate function and
   % use it. The getdata_wrapper is currently the default resource type.
   % The key indicates that opening the resource has succeeded before; it
   % could now fail because the resource is no longer present.
   %
   PFunc = ResourceList{end,2};
   for i=1:size(ResourceList,1)
      if isequal(Key.Type,ResourceList{i,1})
         PFunc = ResourceList{end,2};
         break
      end
   end
   Functions = feval(PFunc,DefaultFunctions);
   if nargin<2
      Data = feval(Functions.open,Key);
   end
   if isempty(Data)
      error('Re-opening resource ''%s'' failed.',Key.Name)
   end
end

%% Now create the resource structure ...
Resource.Functions = Functions;
Resource.RawData = Data;
%
% Get dimensions and detect time dimensions.
%
[D,okay] = feval(Functions.dimensions,Data);
if ~okay
    rethrow(lasterror)
end
Dims = {D.Name};
TDimIndex = ismember({D.Type},{'discrete-time','continuous-time'});
TDims = {D(TDimIndex).Name};
Resource.Dimensions = D;
%
% Get locations and verify dimensions.
%
Topologies = ...
    {'Struct3D'     'is'    0
    'Struct2D+'     'vdims' 0
    'Unstruct3D'    'is'    4
    'Unstruct2D+'   'vdims' 3
    'Unstruct1D+'   'vdims' 2
    'Unstruct0D+'   'vdims' 1};
[L,okay] = feval(Functions.locations,Data);
if ~okay
    rethrow(lasterror)
end
for i = 1:length(L)
    iTopo = strmatch(L(i).Topology,Topologies(:,1),'exact');
    spatDims = L(i).SpatialDimensions;
    for d = 1:length(spatDims)
        if ~ismember(spatDims{d},Dims)
            if d>Topologies{iTopo,3}
                vdim = d-Topologies{iTopo,3};
                dimOkay = vdim/3 == round(vdim/3);
            else
                dimOkay = false;
            end
            if ~dimOkay
                error('Spatial dimension ''%s'' for location ''%s'' undefined.',spatDims{d},L(i).Name)
            end
        end
    end
    timeDims = L(i).TimeDimensions;
    for d = 1:length(timeDims)
        if ~ismember(timeDims{d},TDims)
            error('Time dimension ''%s'' for location ''%s'' undefined.',L(i).TimeDimension,L(i).Name)
        end
    end
end
Resource.Locations = L;
%
Q = feval(Functions.quantities,Data);
Locs = {Resource.Locations.Name};
for i = 1:length(Q)
    Loc = strmatch(Q(i).Location,Locs,'exact');
    if isempty(Loc)
        Q(i).SpatialDimensions = {};
    else
        Q(i).SpatialDimensions = intersect(Q(i).Dimensions(:),Resource.Locations(Loc).SpatialDimensions);
    end
    Q(i).TimeDimensions = intersect(Q(i).Dimensions(1,:),TDims);
    Q(i).SubDimensions = setdiff(Q(i).Dimensions(1,:),[Q(i).SpatialDimensions TDims]);
end
Resource.Quantities = Q;

%% Provide the default functions that throw errors ...
function Fcn = getDefaultFunctions
Fcn.open       = @erroropen;
Fcn.dimensions = @errordimensions;
Fcn.locations  = @errorlocations;
Fcn.quantities = @errorquantities;
Fcn.times      = @errortimes;
Fcn.data       = @errordata;
Fcn.grid       = @errorgrid;
Fcn.options    = @erroroptions;
Fcn.plot       = @errorplot;

%% Trivial implementation of default functions
function varargout = erroropen(varargin)                                   %#ok
error('Open function not implemented.')

function varargout = errordimensions(varargin)                             %#ok
error('Dimensions function not implemented.')

function varargout = errorlocations(varargin)                              %#ok
error('Locations function not implemented.')

function varargout = errorquantities(varargin)                             %#ok
error('Quantities function not implemented.')

function varargout = errordata(varargin)                                   %#ok
error('Data function not implemented.')

function varargout = errorgrid(varargin)                                   %#ok
error('Grid function not implemented.')

function varargout = erroroptions(varargin)                                %#ok
error('Options function not implemented.')

function varargout = errorplot(varargin)                                   %#ok
error('Plot function not implemented.')
