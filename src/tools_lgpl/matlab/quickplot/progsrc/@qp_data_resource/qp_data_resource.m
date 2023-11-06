function X=qp_data_resource(varargin)
%QUICKPLOT Data Resource Object.
%
% Object construction
%   qp_data_resource - Construct qp_data_resource object.
%
% General
%   disp             - Display qp_data_resource object.
%   display          - Display qp_data_resource object.
%   subsref          - Subscripted reference qp_data_resource object.
%   fieldnames       - Get object property names.
%
% Helper routines
%   classic          - Convert qp_data object to classic QUICKPLOT data structure.

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

%% Open resource
% Opening the resource will provide
%
% * a resource structure, and
% * a key
%
% The key is contains all information necessary to recreate/reopen the
% resource; it is stored as part of the plot. The resource structure is
% stored by the resourcemanager for reuse during the current session.
[Resource,Key] = expand(varargin);
resourcemanager('store',Key,Resource)

%% Create basic structure
S.Key = Key;
S.LocationNrs = 1:length(Resource.Locations);
S.QuantityNrs = 1:length(Resource.Quantities);
S.DimensIndex = [];
S.Selection = [];
S.Unit = [];
S.Raw = false;

%% Convert structure to object
X = class(S,'qp_data_resource');
