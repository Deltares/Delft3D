function Output=vs_disp(varargin)
%VS_DISP Displays the filestructure of a NEFIS file.
%
%   GENERAL OVERVIEW OF FILE CONTENTS
%   =================================
%   VS_DISP(NFSfile) lists all group names, group elements, and all group
%   and element properties like the "disp stat" command of the stand alone
%   Viewer Selector program for accessing NEFIS file contents.
%
%   VS_DISP(NFSfile,[]) lists only group names, group dimensions, and
%   number of elements per group.
%
%   Output=VS_DISP(NFSfile) or Output=VS_DISP(NFSfile,[]) returns a char
%   array containing the names of the groups.
%
%   CONTENT OF SINGLE GROUP
%   =======================
%   VS_DISP(NFSfile,'GroupName') lists all group elements, and all element
%   properties like the "disp <GroupName>" command of the stand alone
%   Viewer Selector program for accessing NEFIS file contents.
%
%   Output=VS_DISP(NFSfile,'GroupName') returns a char array containing
%   the names of the elements of the group.
%
%   VS_DISP(NFSfile,'GroupName',[]) gives detailed data about the
%   specified group.
%
%   Output=VS_DISP(NFSfile,'GroupName',[]) returns detailed data about the
%   specified group.
%
%   INFORMATION OF A SINGLE ELEMENT
%   ===============================
%   VS_DISP(NFSfile,'GroupName','ElementName') gives detailed data about
%   the specified element.
%
%   Output=VS_DISP(NFSfile,'GroupName','ElementName') returns detailed
%   data about the specified element.
%
%   FOR ALL CASES
%   =============
%   NFSfile may be either the data structure of a NEFIS as obtained from
%   VS_USE or the name of a NEFIS file (in which case VS_USE will be called
%   on the fly). If the NFSfile is not specified then NEFIS that was last
%   opened by VS_USE will be used to read the data.
%
%   VS_DISP(FID,...) writes the information to the specified file instead
%   of the command window.
%
%   Example
%      F = vs_use('trim-xxx.dat','trim-xxx.def');
%      vs_disp(F,[]) % show brief listing of all groups
%      vs_disp(F,'map-series') % shows detailed listing of map-series group
%      Info=vs_disp(F,'map-series','S1') % shows detailed info of element
%                                        % S1 in map-series group.
%
%   See also VS_USE, VS_DISP, VS_LET, VS_GET, VS_DIFF, VS_FIND, VS_TYPE.

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

fid=1;
args = varargin;
if nargin>0
    if isnumeric(args{1}) & ~isempty(args{1})
        fid = args{1};
        args = args(2:end);
    end
end

VS=[];
if ~isempty(args)
    if isstruct(args{1})
        VS = args{1};
        args = args(2:end);
    elseif ischar(args{1})
        try
           VS = vs_use(args{1});
           args = args(2:end);
        catch
        end
    end
end
if isempty(VS)
    VS=vs_use('lastread');
    if ~isstruct(VS)
        error('No NEFIS file specified.');
    end
end

args(length(args)+1:2) = {''};
gName = args{1};
eName = args{2};

% Verify input arguments
if ~(ischar(gName) | isequal(gName,[])) | ~(ischar(eName) | isequal(eName,[]))
    error('Invalid group or element name specified.');
end
gName=deblank(gName);
eName=deblank(eName);

short=0;
OutRec=0;
if strcmp(gName,'')
    iList=1:length(VS.GrpDat);
elseif strcmp(eName,'')
    % gName: either empty - for a short list
    %        or the name of a group.
    if isempty(gName) | isequal(gName,'[]')
        short=1;
        iList=1:length(VS.GrpDat);
    elseif ischar(gName)
        if isempty(VS.GrpDat)
            gNames={};
        else
            gNames={VS.GrpDat(:).Name};
        end
        iList=strmatch(gName,gNames,'exact');
        if isempty(iList)
            if nargout==0
                fprintf(fid,' [****] Group does not exist.\n');
                iList=strmatch(gName,gNames);
                if ~isempty(iList)
                    fprintf(fid,'        Matching');
                    Char=':';
                    for ii=1:length(iList)
                        fprintf(fid,'%c %s',Char,VS.GrpDat(iList(ii)).Name);
                        Char=',';
                    end
                    fprintf(fid,'\n');
                end
            else
                Output=-1;
            end
            return
        end
    else
        if nargout==0
            fprintf(fid,' [****] Incorrect argument.\n');
        else
            Output=-1;
        end
        return
    end
else
    % gName: the name of a group
    % eName: empty - for information about the group
    %        or, the name of an element - for information about the element
    if ischar(gName)
        if isempty(VS.GrpDat)
            gNames={};
        else
            gNames={VS.GrpDat(:).Name};
        end
        iList=strmatch(gName,gNames,'exact');
        if isempty(iList)
            if nargout==0
                fprintf(fid,' [****] Group does not exist.\n');
                iList=strmatch(gName,gNames);
                if ~isempty(iList)
                    fprintf(fid,'        Matching');
                    Char=':';
                    for ii=1:length(iList)
                        fprintf(fid,'%c %s',Char,VS.GrpDat(iList(ii)).Name);
                        Char=',';
                    end
                    fprintf(fid,'\n');
                end
            else
                Output=-1;
            end
            return
        end
    else
        if nargout==0
            fprintf(fid,' [****] Incorrect argument.\n');
        else
            Output=-1;
        end
        return
    end
    if isempty(eName) | isequal(eName,'[]')
        OutRec=1;
    elseif ischar(eName)
        OutRec=2;
        idList=VS.GrpDat(iList).DefIndex; % allways one element
        if idList<=0
            if nargout==0
                fprintf(fid,' [****] Data group definition is missing.\n');
            else
                Output=-1;
            end
            return
        end
        icList=VS.GrpDef(idList).CelIndex; % allways one element
        if icList<=0
            if nargout==0
                fprintf(fid,' [****] Cel definition is missing.\n');
            else
                Output=-1;
            end
            return
        end
        elList=VS.CelDef(icList).Elm; % get elements of the specified group
        jList=strmatch(eName,{VS.ElmDef.Name},'exact');
        if ~isempty(jList) % check whether the element is part of the group
            jList=elList(jList==elList);
        end
        if isempty(jList)
            if nargout==0
                fprintf(fid,' [****] Element does not exist (in specified group).\n');
                jList=elList(strmatch(eName,{VS.ElmDef(elList).Name}));
                if ~isempty(jList)
                    fprintf(fid,'        Matching');
                    Char=':';
                    for jj=1:length(jList)
                        fprintf(fid,'%c %s',Char,VS.ElmDef(jList(jj)).Name);
                        Char=',';
                    end
                    fprintf(fid,'\n');
                end
            else
                Output=-1;
            end
            return
        elseif length(jList)>1
            if nargout==0
                fprintf(fid,' [****] Element exists multiple times in group; selecting first one.\n');
            end
            jList = jList(1);
        end
    else
        if nargout==0
            fprintf(fid,' [****] Incorrect argument.\n');
        else
            Output=-1;
        end
        return
    end
end

% Valid input arguments
if nargout~=0
    if OutRec==1
        % iList scalar
        Str.Name=VS.GrpDat(iList).Name;
        Str.GroupDatOffset=VS.GrpDat(iList).Offset;
        Str.DefName=VS.GrpDat(iList).DefName;
        for a=1:5
            Str.Attrib.Int(a).Name=deblank(VS.GrpDat(iList).IANames(a,:));
            Str.Attrib.Int(a).Value=VS.GrpDat(iList).IAValue(a);
        end
        for a=1:5
            Str.Attrib.Real(a).Name=deblank(VS.GrpDat(iList).RANames(a,:));
            Str.Attrib.Real(a).Value=VS.GrpDat(iList).RAValue(a);
        end
        for a=1:5
            Str.Attrib.Str(a).Name=deblank(VS.GrpDat(iList).SANames(a,:));
            Str.Attrib.Str(a).Value=deblank(VS.GrpDat(iList).SAValue(a,:));
        end
        Str.VarDim=VS.GrpDat(iList).VarDim;
        idList=VS.GrpDat(iList).DefIndex;
        if idList>0
            Str.GroupDefOffset=VS.GrpDef(idList).Offset;
            Str.CellName=VS.GrpDef(idList).CelName;
            Str.NDim=length(VS.GrpDat(iList).SizeDim);
            Str.SizeDim=VS.GrpDat(iList).SizeDim;
            Str.OrderDim=VS.GrpDat(iList).OrderDim;
            icList=VS.GrpDef(idList).CelIndex;
            if icList>0
                Str.CellDefOffset=VS.CelDef(icList).Offset;
                elList=VS.CelDef(icList).Elm;
                if any(elList==0)
                    Str.CellNByte=NaN;
                else
                    Str.CellNByte=sum([VS.ElmDef(elList).SizeElm]);
                end
                Str.CellNElm=length(elList);
            end
        end
    elseif OutRec==2
        % iList and jList scalar
        Str.GrpNname=gName;
        Str.ElmName=eName;
        Str.ElmQuantity=deblank(VS.ElmDef(jList).Quantity);
        Str.ElmUnits=deblank(VS.ElmDef(jList).Units);
        Str.ElmDescription=deblank(VS.ElmDef(jList).Description);
        Str.ElmDefOffset=VS.ElmDef(jList).Offset;
        Str.NDim=length(VS.ElmDef(jList).Size);
        Str.SizeDim=VS.ElmDef(jList).Size;
        Str.TypeVal=VS.ElmDef(jList).Type;
        Str.NByteVal=VS.ElmDef(jList).SizeVal;
        Str.NByte=VS.ElmDef(jList).SizeElm;
    else
        if isempty(gName) | isequal(gName,'[]')
            if isempty(VS.GrpDat)
                Str={};
            else
                Str={VS.GrpDat(:).Name};
            end
            % str2mat?
        else
            % iList is scalar
            idList=VS.GrpDat(iList).DefIndex;
            elList=[];
            Str={};
            if idList>0
                icList=VS.GrpDef(idList).CelIndex;
                if icList>0
                    elList=VS.CelDef(icList).Elm;
                    Str={VS.ElmDef(elList).Name};
                end
            end
            % str2mat?
        end
    end
    Output=Str;
    return
end

if OutRec==1
    fprintf(fid,'Information of group ''%s''\n',gName);
    disp(vs_disp(VS,gName,[]));
    return
elseif OutRec==2
    fprintf(fid,'Information of element ''%s'' of group ''%s''\n',eName,gName);
    disp(vs_disp(VS,gName,eName));
    return
end
for i=iList
    if short
        fprintf(fid,'%s(',leftstr(VS.GrpDat(i).Name,16));
    else
        fprintf(fid,'Group Name:%s Dimensions:(',leftstr(VS.GrpDat(i).Name,16));
    end
    fprintf(fid,'%i',VS.GrpDat(i).SizeDim(1));
    if length(VS.GrpDat(i).SizeDim)>1
        fprintf(fid,',%i',VS.GrpDat(i).SizeDim(2:end));
    end
    if short
        fprintf(fid,') -');
    else
        fprintf(fid,')\n');
    end
    Att=sum(~all(VS.GrpDat(i).IANames==32,2)) ...
        +sum(~all(VS.GrpDat(i).RANames==32,2)) ...
        +sum(~all(VS.GrpDat(i).SANames==32,2));
    if Att
        if short
            fprintf(fid,' %i attribute(s),', Att);
        else
            fprintf(fid,'  Attributes:\n');
            for j=1:5
                if any(VS.GrpDat(i).IANames(j,:)~=32)
                    fprintf(fid,'          %s=%14i\n',VS.GrpDat(i).IANames(j,:),VS.GrpDat(i).IAValue(j));
                end
            end
            for j=1:5
                if any(VS.GrpDat(i).RANames(j,:)~=32)
                    fprintf(fid,'          %s=%14.7f\n',VS.GrpDat(i).RANames(j,:),VS.GrpDat(i).RAValue(j));
                end
            end
            for j=1:5
                if any(VS.GrpDat(i).SANames(j,:)~=32)
                    fprintf(fid,'          %s=''%s''\n',VS.GrpDat(i).SANames(j,:),VS.GrpDat(i).SAValue(j,:));
                end
            end
        end
    else
        if short
            fprintf(fid,' No attributes,');
        else
            fprintf(fid,'  No attributes\n');
        end
    end
    id=VS.GrpDat(i).DefIndex;
    if id<=0
        fprintf(fid,' [****] Group definition definition is missing.\n');
    else
        ic=VS.GrpDef(id).CelIndex;
        if ic<=0
            fprintf(fid,' [****] Cel definition is missing.\n');
        else
            el=VS.CelDef(ic).Elm;
            if short
                fprintf(fid,' %i element(s).\n',length(el));
            else
                for j=el
                    if j<=0 | j>length(VS.ElmDef)
                        fprintf(fid,'    **** Element Definition Missing\n');
                    else
                        fprintf(fid,'    %s',leftstr(VS.ElmDef(j).Name,16));
                        switch VS.ElmDef(j).Type
                            case 1
                                fprintf(fid,'CHARACTE*');
                            case 2
                                fprintf(fid,'COMPLEX *');
                            case 3
                                fprintf(fid,'INTEGER *');
                            case 4
                                fprintf(fid,'LOGICAL *');
                            case 5
                                fprintf(fid,'REAL    *');
                            otherwise
                                fprintf(fid,'????    *');
                        end
                        fprintf(fid,'%3i %s %s (', ...
                            VS.ElmDef(j).SizeVal, ...
                            leftstr(VS.ElmDef(j).Quantity,16), ...
                            leftstr(VS.ElmDef(j).Units,16));
                        fprintf(fid,' %i',VS.ElmDef(j).Size);
                        fprintf(fid,' )\n        %s\n',VS.ElmDef(j).Description);
                    end
                end
            end
            if ~short
                fprintf(fid,'\n');
            end
        end
    end
end

function Str=leftstr(StrIn,Len)
Str=char(32*ones(1,Len));
Str(1:length(StrIn))=StrIn;
