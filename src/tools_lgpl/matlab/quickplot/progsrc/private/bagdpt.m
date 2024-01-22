function Out=bagdpt(cmd,varargin)
%BAGDPT Read output files BAGGER-BOS-RIZA bagger option.
%
%   FILEINFO=BAGDPT('read',FILENAME)
%   Open bagger dpt-file (bagdpt.<case>) and returns a
%   structure containing the data in the file.

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

if nargin==0
    error('Missing command.');
end

switch cmd
    case 'read'
        Out=Local_bagdptread(varargin{:});
    otherwise
        error('Unknown command')
end


function Structure=Local_bagdptread(filename,quiet)
if nargin<2
    quiet=0;
end
Structure.Check='NotOK';
Structure.FileType='baggerdpt';

if nargin==0 || strcmp(filename,'?')
    [fname,fpath]=uigetfile('bagdpt.*','Select bagger history file');
    if ~ischar(fname),
        return
    end
    filename=[fpath,fname];
end
Structure.FileName=filename;

fid=fopen(filename,'r','n','US-ASCII');
if fid<0
    error('Cannot open %s.',filename)
end
try
    Line=fgetl(fid); %   Time volume in depot gebaggerd volume Volume teruggestort
    if ~isequal(lower(Line(1:23)),'   time volume in depot')
        fclose(fid);
        return
    end
    
    Line=fgetl(fid); %  [tscale] [m3] [m3] [m3]
    Data=fscanf(fid,'%f',[4 inf])';
    fclose(fid);
catch
    fclose(fid);
    rethrow(lasterror)
end
Structure.Time=Data(:,1);
Structure.VolDepot=Data(:,2);
Structure.VolDredge=Data(:,3);
Structure.VolDumped=Data(:,4);
Structure.Check='OK';
