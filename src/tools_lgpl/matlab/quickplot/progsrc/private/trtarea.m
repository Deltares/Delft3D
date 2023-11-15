function varargout=trtarea(cmd,varargin)
%TRTAREA Read Delft3D trachytope/WAQUA ecotope area files.

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
    error('Missing command string.')
end
varargout=cell(1,nargout);

switch cmd
    case 'read'
        varargout{1}=Local_arearead(varargin{:});
    case 'write'
        Out=Local_areawrite(varargin{:});
        if nargout>0
            varargout{1}=Out;
        end
    otherwise
        error('Unknown command.')
end


function Struct=Local_arearead(filename,grid)
if nargin==0
    error('Missing file name.')
end
fid=fopen(filename,'r','n','US-ASCII');
if fid<0
    return
end
Struct.FileName=filename;
Struct.FileType='trtarea';
maxRecords=10000;
Record=zeros(maxRecords,6);
i=0;
idx4=[1:2 5:6];
while ~feof(fid)
    i=i+1;
    if i>maxRecords
        maxRecords=maxRecords+10000;
        Record(maxRecords,1)=0;
    end
    Temp=sscanf(fgetl(fid),'%f',[1 6]);
    if length(Temp)==4
        if ~isequal(round(Temp(1:3)),Temp(1:3))
            fclose(fid);
            error('Floating point value read when integer was expected on line %i.',i)
        elseif any(Temp<0) | any(Temp(1:3)==0)
            fclose(fid);
            error('Unexpected negative or zero value on line %i.',i)
        else
            Record(i,idx4)=Temp;
        end
    elseif length(Temp)==6
        if ~isequal(round(Temp(1:5)),Temp(1:5))
            fclose(fid);
            error('Floating point value read when integer was expected on line %i.',i)
        elseif any(Temp<0) | any(Temp(1:3)==0)
            fclose(fid);
            error('Unexpected negative or zero value on line %i.',i)
        else
            Record(i,1:6)=Temp;
        end
    else
        fclose(fid);
        error('Invalid number of values on line %i.',i)
    end
end
fclose(fid);
Record=Record(1:i,:);
%
% Following check is only valid for WAQUA. In the case of Delft3D any
% roughness code can represent a line roughness type and can, therefore, be
% associated with a fraction (in that case length) of more than 1.
%
%invalid=Record(:,5)<950 & Record(:,6)>1;
%if any(invalid)
%   invalid=find(invalid);
%   i=invalid(1);
%   error('Invalid area fraction %f on line %i.',Record(i,6),i)
%end
Struct.Records=Record;
Struct.RoughnessIDs=unique(Record(:,5));

function OK=Local_areawrite(filename,Struct)
fid=fopen(filename,'w','n','US-ASCII');
idx4=[1:2 5:6];
for line=1:size(Struct.Records,1)
    if Struct.Records(line,3)==0
        fprintf(fid,'%i %i %i %g\n',Struct.Records(line,idx4));
    else
        fprintf(fid,'%i %i %i %i %i %g\n',Struct.Records(line,:));
    end
end
fclose(fid);
OK=1;
