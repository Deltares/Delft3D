function trim2rst(trim,i,rst)
%TRIM2RST Extract Delft3D-FLOW restart file from TRIM-file.
%
%   TRIM2RST(TRIMFILE,i,ResartFilename)
%   Read data for time step i from TRIM-file and write
%   data to specified Delft3D-FLOW restart file. The
%   TRIM-file can be specified by means of its name or
%   data structure obtained from VS_USE.
%
%   TRIM2RST(timestep,TRIRSTFILE)
%   Use the last opened nefis file.

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

if nargin==2
    rst=i;
    i=trim;
    trim=vs_use('lastread');
end
if ischar(trim)
    trim=vs_use(trim);
end
if ~isstruct(trim) || ~isfield(trim,'SubType') || ~isequal(trim.SubType,'Delft3D-trim')
    error('Invalid TRIM-file specified.');
end
if ~ischar(rst)
    error('Invalid restart file name specified.');
end

%
% decide whether we are going to write a binary tri-rst file or an ascii
% ini initial conditions file.
%
[p,f,e]=fileparts(rst);
if strcmpi(e,'.ini') && ~strncmpi('tri-rst',f,7)
    fileformat = 'ini';
else
    fileformat = 'rst';
end
C=vs_get(trim,'map-series',{i},'*','nowarn');

c={};
c{1}=C.S1';
i=1;
kmax=size(C.U1,3);
for k=1:kmax
    i=i+1;
    c{i}=C.U1(:,:,k)';
end
for k=1:kmax
    i=i+1;
    c{i}=C.V1(:,:,k)';
end
if isfield(C,'R1') && ~isequal(size(C.R1),[1 1])
    for s=1:size(C.R1,4)
        for k=1:kmax
            i=i+1;
            c{i}=C.R1(:,:,k,s)';
        end
    end
end
if strcmp(fileformat,'rst')
    %
    % turbulence parameters and HLES depth averaged velocities are only
    % written to binary restart file
    %
    if isfield(C,'RTUR1') && ~isequal(size(C.RTUR1),[1 1])
        for s=1:size(C.RTUR1,4)
            for k=1:kmax+1
                i=i+1;
                c{i}=C.RTUR1(:,:,k,s)';
            end
        end
    end
    if isfield(C,'UMNLDF') && ~isequal(size(C.UMNLDF),[1 1]) && ~isempty(C.UMNLDF)
        i=i+1;
        c{i}=C.UMNLDF';
        i=i+1;
        c{i}=C.VMNLDF';
    else
        i=i+1;
        c{i}=0*C.S1';
        i=i+1;
        c{i}=0*C.S1';
    end
    
    trirst('write',rst,c{:})
else
    wldep('write',rst,'format','%16.7e',c{:})
end