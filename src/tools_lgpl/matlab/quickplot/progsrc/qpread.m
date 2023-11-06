function [varargout]=qpread(varargin)
%QPREAD Read data from various types of data files.
%   DOMAINS = QPREAD(FILE,'domains') returns a list of the domains stored
%   in the data file specified where FILE is a structure obtained from
%   QPFOPEN or VS_USE. If FILE is not specified the last opened NEFIS file
%   is used (same condition applies to the other calls to QPREAD).
%
%   [FIELDNAMES,DIMS,NVAL] = QPREAD(FILE,DOMAINNR) returns the data fields
%   available from the data file for the specified domain number. The
%   variable FIELDNAMES is a cell array of strings containing the names of
%   the data sets in the file. The variable DIMS indicates relevant
%   dimensions for the data fields:
%     First column of Dims : Timestep
%     Second column of Dims: Station name
%     Third column of Dims : M
%     Fourth column of Dims: N
%     Fifth column of Dims : K
%   NVAL equals one for scalar datasets, 2 (or 3) for vector data sets. If
%   there is only one domain, you may choose to drop the DOMAINNR argument
%   in this and all following QPREAD calls.
%
%   FIELDS = QPREAD(FILE,DOMAINNR) returns a structure containing the above
%   mentioned data a some extra information for internal use. The above
%   mentioned arrays can be obtained from FIELDS by
%      FIELDNAMES = transpose({FIELDS.Name})
%      DIMS       = cat(1,FIELDS.DimFlag)
%      NVAL       = cat(1,FIELDS.NVal)
%
%   FIELDSIZE = QPREAD(FILE,DOMAINNR,FIELD,'size') returns an 1 x 5 array
%   containing the size of the five dimensions mentioned above. The FIELD
%   parameter can be either a string (field name) or one structure from the
%   FIELDS array. The latter is neccessary if the field name is not unique.
%   The field name may be shortened provided that the identification
%   remains unique. The same dual selection method applies to all QPREAD
%   calls described below.
%
%   TIMES = QPREAD(FILE,DOMAINNR,FIELD,'times') returns an array of times
%   for the specified data field.
%
%   STATIONS = QPREAD(FILE,DOMAINNR,FIELD,'stations') returns a cell array
%   of strings containing the station names for the specified data field.
%
%   SUBFIELDS = QPREAD(FILE,DOMAINNR,FIELD,'subfields') returns a cell
%   array of strings containing the subfield names for the specified data
%   field. Subfields are sometimes used to store substance names as an
%   additional dimension other than time or space.
%
%   DATA = QPREAD(FILE,DOMAINNR,FIELD,'data',SUBFIELD,T,S,M,N,K) returns
%   the data for the specified field for the selected subfield at the
%   indicated timesteps, specified station, and M, N, K coordinates. The
%   SUBFIELD must be specified if and only if the 'subfields' command
%   described above returns a non-empty list. The other selection
%   parameters (T,S,M,N,K) should be specified if they have a non-zero
%   entry in DIMS and SIZE. For example, for a quantity defined at a
%   station this is often only T and S. For spatial data sets not defined
%   at a station, S should be skipped and an appropriate selection of M, N
%   and K should be specified.
%       T : index of the time(s) in the TIMES array.
%       S : index of the station in the STATIONS array.
%       M : 0 (all points), range or scalar value (m index).
%       N : 0 (all points), range or scalar value (n index).
%       K : 0 (all points), range or scalar value (k index).
%
%   DATA = QPREAD(FILE,DOMAINNR,FIELD,'griddata',SUBFIELD,T,S,M,N,K)
%   returns a structure containing both the data and the grid.
%
%   See also D3D_QP, QPFOPEN, VS_USE.

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

varargout=cell(1,max(nargout,1));
[OK,varargout{:}]=qp_getdata(varargin{:});
if ~OK
   fprintf('Error encountered, check syntax!\n');
   rethrow(lasterror)
end
