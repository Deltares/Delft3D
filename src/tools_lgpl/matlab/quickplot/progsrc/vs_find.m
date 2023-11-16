function Output=vs_find(FileData,IO_eName)
%VS_FIND Locates an element in the filestructure of a NEFIS file.
%   Output=vs_find(NFStruct,'ElementName') lists the names of the groups
%   that contain the specified element using the data stored in the
%   NEFIS structure. If the name specified is a group that will be
%   mentioned also.
%
%   If the file (NFStruct) is not specified, the NEFIS that was last opened
%   by VS_USE will be used to read the data. A file structure NFStruct can
%   be obtained as output argument from the function VS_USE.
%
%   See also VS_USE, VS_DISP, VS_LET, VS_GET, VS_DIFF, VS_TYPE.

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

switch nargin
    case 0
        error('Not enough input arguments');
    case 1
        if isstruct(FileData)
            error('No string specified.');
        end
        VS=vs_use('lastread');
        if ~isstruct(VS)
            error('No NEFIS file specified.');
        end
        eName=FileData;
    case 2
        if isstruct(FileData)
            VS=FileData;
            eName=IO_eName;
        else
            error('(First) argument should be a NEFIS structure');
        end
end

gNames={VS.GrpDat(:).Name};
J=strmatch(eName,gNames,'exact');
if nargout==0
    if ~isempty(J)
        fprintf(1,'%s is the name of a data group.\n',eName);
    end
end

gNames={VS.GrpDef(:).Name};
J=strmatch(eName,gNames,'exact');
if nargout==0
    if ~isempty(J)
        fprintf(1,'%s is the definition name of a group.\n',eName);
    end
end

gNames={VS.CelDef(:).Name};
J=strmatch(eName,gNames,'exact');
if nargout==0
    if ~isempty(J)
        fprintf(1,'%s is the name of a cell.\n',eName);
    end
end

eNames={VS.ElmDef(:).Name};
J=strmatch(eName,eNames,'exact');
if isempty(J)
    if nargout==0
        fprintf(1,'No group contains an element called %s.\n',eName);
    else
        Output='';
    end
    return
end
if nargout==0
    if ~isempty(J)
        fprintf(1,'%s is the name of an element.\n',eName);
    end
end

icList=zeros(1,length(VS.CelDef));
for i=1:length(VS.CelDef)
    icList(i)=ismember(J,VS.CelDef(i).Elm);
end
icList=find(icList);

idList=zeros(1,length(VS.GrpDef));
for i=1:length(VS.GrpDef)
    idList(i)=ismember(VS.GrpDef(i).CelIndex,icList);
end
idList=find(idList);

iList=zeros(1,length(VS.GrpDat));
for i=1:length(VS.GrpDat)
    iList(i)=ismember(VS.GrpDat(i).DefIndex,idList);
end
iList=find(iList);

if nargout==0
    if isempty(iList)
        fprintf(1,'No data group contains an element called %s.\n',eName);
    else
        if length(iList)>1
            fprintf(1,'The following data groups contain an element called %s.\n',eName);
        else
            fprintf(1,'The following data group contains an element called %s.\n',eName);
        end
        for i=iList
            id=VS.GrpDat(i).DefIndex;
            idList=setdiff(idList,id);
            ic=VS.GrpDef(id).CelIndex;
            icList=setdiff(icList,ic);
            fprintf(1,'%s [def: %s, cel: %s]\n', ...
                leftstr(VS.GrpDat(i).Name,16), ...
                VS.GrpDef(id).Name,VS.CelDef(ic).Name);
        end
    end
    if ~isempty(idList)
        if length(idList)>1
            fprintf(1,'The following group definitions contain an element called %s.\n',eName);
        else
            fprintf(1,'The following group definition contains an element called %s.\n',eName);
        end
        for id=idList
            ic=VS.GrpDef(id).CelIndex;
            icList=setdiff(icList,ic);
            fprintf(1,'%s [cel: %s]\n', ...
                leftstr(VS.GrpDef(id).Name,16), ...
                VS.CelDef(ic).Name);
        end
    end
    if ~isempty(icList)
        if length(icList)>1
            fprintf(1,'The following cell definitions contain an element called %s.\n',eName);
        else
            fprintf(1,'The following cell definition contains an element called %s.\n',eName);
        end
        for ic=icList
            fprintf(1,'%s\n',leftstr(VS.CelDef(ic).Name,16));
        end
    end
else
    if isempty(iList)
        Output={};
    else
        Output={VS.GrpDat(iList).Name};
    end
end

function Str=leftstr(StrIn,Len)
Str=char(32*ones(1,Len));
Str(1:length(StrIn))=StrIn;
