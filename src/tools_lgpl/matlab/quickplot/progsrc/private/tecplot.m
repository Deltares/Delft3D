function Out=tecplot(cmd,varargin)
%TECPLOT Read/write for Tecplot files.
%
%   FileInfo=TECPLOT('write',FileName,Data)
%      Writes the matrix Data to a Tecplot file.
%   NewFileInfo=TECPLOT('write',FileName,FileInfo)
%      Writes a Tecplot file based on the information
%      in the FileInfo. FileInfo should be a structure
%      with at least a field Zone having two subfields
%      Title and Data. For example
%        FI.Zone(1).Title='B001';
%        FI.Zone(1).Data=Data1;
%        FI.Zone(2).Title='B002';
%        FI.Zone(2).Data=Data2;
%      Optional fields Title (overall title) and Variables
%      (cell array of variable names) will also be processed.

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
    if nargout>0
        Out=[];
    end
    return
end
switch cmd
    case 'open'
        Out=Local_open_file(varargin{:});
    case 'write'
        Out=Local_write_file(varargin{:});
    otherwise
        uiwait(msgbox('unknown command','modal'));
end


function FI=Local_open_file(filename)
FI.FileName=filename;
fid=fopen(filename,'r','l');
if fid<0
    error('Cannot open "%s".',filename)
end
try
    str = fread(fid,[1 8],'*char');
    if ~strcmp(str(1:5),'#!TDV')
        fclose(fid);
        error('File "%s" does not start with "#!TDV" string',filename)
    end
    version = sscanf(str(6:8),'%i');
    switch version
        case 75
            FI.Version = 9;
        case 102
            FI.Version = 10;
        case 112
            FI.Version = 11;
        otherwise
            fclose(fid);
            error('Version %i of Tecplot file "%s" not supported.',version,file)
    end
    fread(fid,1,'int32'); % 1
    fread(fid,1,'int32'); % 1
    FI.Title = freadstring(fid);
    nV = fread(fid,1,'int32');
    Var = cell(1,nV);
    for i = 1:nV
        Var{i} = freadstring(fid);
    end
    FI.Variables = Var;
    %
    fread(fid,[1 4],'uchar'); %00 80 95 43
    %
    z = 1;
    FI.Zone(z).Title = freadstring(fid);
    switch FI.Version
        case 9
             ZoneType = fread(fid,1,'int32'); % 0=BLOCK, 1=POINT, 2=FEBLOCK, 3=FEPOINT order
             FI.Zone(z).Color = fread(fid,1,'int32'); % ColorNumber
        case {10,11}
             FI.Zone(z).Color = fread(fid,1,'int32'); % ColorNumber
             ZoneType = fread(fid,1,'int32'); % 0=ORDERED, 1=FELINESEG, 2=FETRIANGULAR, 3=FEQUADRILATERAL, 4=FETETRAHEDRON, 5=FEBRICK
             BlockPnt = fread(fid,1,'int32'); % 0=BLOCK, 1=POINT
             DataLoc  = fread(fid,1,'int32'); % NODAL or CELLCENTERED data - if 1, followed by flag array of length nV: 0=NODAL, 1=CELLCENTERED
             fread(fid,1,'int32'); % ???
        case 11
            % ???
    end
    fclose(fid);
catch err
    fclose(fid);
    rethrow(err)
end

function FI=Local_write_file(filename,FileInfo)
if ~isstruct(FileInfo),
    FI.Zone.Title='';
    FI.Zone.Data=FileInfo;
else
    FI=FileInfo;
end
FI.Check='NotOK';
if isfield(FI,'FileType')
    ASCII=isequal(FI.FileType,'TecplotASCII');
else
    [p,f,e]=fileparts(filename);
    if isequal(lower(e),'.dat')
        ASCII=1;
    elseif isequal(lower(e),'.plt')
        ASCII=0;
    else
        ASCII=1;
    end
end
FI.FileName=filename;
if ~isfield(FI.Zone,'Color')
    [FI.Zone(:).Color]=deal('');
end
if ~isfield(FI.Zone,'Color')
    [FI.Zone(:).DataPacking]=deal('BLOCK');
end
if ~isfield(FI.Zone,'AuxData')
    FI.Zone(1).AuxData=[];
end
if ASCII
    FI.FileType='TecplotASCII';
    if ~isfield(FI,'Version')
        FI.Version=9;
    end
else
    FI.FileType='TecplotBINARY';
    if ~isfield(FI,'Version')
        FI.Version=10;
    end
end

%
% --- Check consistency of number of variables ...
%
nV=[];
for i=1:length(FI.Zone)
    nDi=ndims(FI.Zone(i).Data);
    if nDi>4
        error('The data array should be at most 4 dimensional.')
    end
    nVi=size(FI.Zone(i).Data,nDi);
    if ~isempty(nV)
        if nV~=nVi,
            error('Number of variables should not vary.')
        end
    else
        nV=nVi;
    end
end

%
% --- Open file ... always in PC style ...
%
fid=fopen(filename,'w','l');
if fid<0
    error('Cannot open requested output file.')
end

%
% --- Start writing file and title ...
%
if ~ASCII
    switch FI.Version
        case 9
            fwrite(fid,'#!TDV75 ','uchar');
        case 10
            fwrite(fid,'#!TDV102','uchar');
    end
end
if ASCII
    if isfield(FI,'Title') && ~isempty(FI.Title)
        fprintf(fid,'TITLE= "%s"\n',FI.Title);
    end
else
    fwrite(fid,1,'int32');
    if isfield(FI,'Title') && ~isempty(FI.Title)
        fwritestring(fid,FI.Title)
    else
        fwritestring(fid,'')
    end
end

%
% --- Use or generate variable names ...
%
for i=1:nV, Vars{i}=sprintf('V%i',i); end
if isfield(FI,'Variables')
    nVnames=min(nV,length(FI.Variables));
    Vars(1:nVnames)=FI.Variables(1:nVnames);
end
FI.Variables=Vars;
if ASCII
    fprintf(fid,'VARIABLES=');
    fprintf(fid,' "%s"',Vars{:});
    fprintf(fid,'\n');
else
    fwrite(fid,nV,'int32');
    for i=1:nV
        fwritestring(fid,Vars{i})
    end
end

Clrs={'', 'BLACK', 'RED', 'GREEN', 'BLUE', 'CYAN', 'YELLOW', 'PURPLE', 'WHITE', ...
    'CUST1', 'CUST2', 'CUST3', 'CUST4', 'CUST5', 'CUST6', 'CUST7', 'CUST8'};

%
% --- Write zones ...
%
for i=1:length(FI.Zone)
    Di=FI.Zone(i).Data;
    nDi=ndims(Di);
    szDi=size(Di);
    nIJK=prod(szDi(1:(nDi-1)));
    ijk=repmat({':'},1,nDi-1);
    %
    % --- Set colorname and colornumber
    %
    Color=FI.Zone(i).Color;
    if isequal(Color,'') || isequal(Color,-1)
        ColorNumber=-1;
    elseif ischar(Color)
        ColorNumber=strmatch(Color,Clrs,'exact')-2;
    else
        ColorNumber=Color;
    end
    ColorName=Clrs{ColorNumber+2};
    %
    % --- Start new zone ...
    %
    if ASCII
        %
        % Start ZONE ...
        %
        fprintf(fid,'ZONE');
        %
        % Size of ordered block ...
        %
        fprintf(fid,' I=%i',szDi(1));
        if nDi>=3, fprintf(fid,' J=%i',szDi(2)); end
        if nDi==4, fprintf(fid,' K=%i',szDi(3)); end
        %
        % BLOCK ordering of the data ...
        %
        switch FI.Version
            case 9
                fprintf(fid,' F=BLOCK');
            case 10
                %fprintf(fid,' ZONETYPE=ORDERED');
                fprintf(fid,' DATAPACKING=BLOCK');
        end
        %
        % Colour ...
        %
        if ColorNumber>=0
            fprintf(fid,' C=%s',ColorName);
        end
        %
        % Zone title ...
        %
        if isfield(FI.Zone,'Title') && ~isempty(FI.Zone(i).Title)
            fprintf(fid,' T="%s"\n',FI.Zone(i).Title);
        else
            fprintf(fid,'\n');
        end
    else
        %
        fwrite(fid,[0 128 149 67],'uchar'); %00 80 95 43
        %
        % Zone title ...
        %
        if isfield(FI.Zone,'Title') && ~isempty(FI.Zone(i).Title)
            fwritestring(fid,FI.Zone(i).Title);
        else
            fwritestring(fid,sprintf('ZONE %3.3i',i));
        end
        %
        switch FI.Version
            case 9
                %
                % 0=BLOCK, 1=POINT, 2=FEBLOCK, 3=FEPOINT order
                %
                fwrite(fid,0,'int32');
                %
                % Colour ...
                %
                fwrite(fid,ColorNumber,'int32');
                %
            case 10
                %
                % Colour ...
                %
                fwrite(fid,ColorNumber,'int32');
                %
                % 0=ORDERED, 1=FELINESEG, 2=FETRIANGULAR, 3=FEQUADRILATERAL, 4=FETETRAHEDRON, 5=FEBRICK
                %
                fwrite(fid,0,'int32');
                %
                % 0=BLOCK, 1=POINT
                %
                fwrite(fid,0,'int32');
                %
                % NODAL or CELLCENTERED data
                % if 1, followed by flag array of length nV: 0=NODAL, 1=CELLCENTERED
                %
                fwrite(fid,0,'int32');
                %
                % ???
                %
                fwrite(fid,0,'int32');
                %
        end
        %
        % I,J,K     or    FI.Version=9  N,E,F
        %                                  F: 1=TRIANGLE, 2=QUADRILATERAL, 3=TETRAHEDRON, 4=BRICK
        %           or    FI.Version=10 N,E and some other values ...
        %
        szWrite = szDi(1:end-1);
        if length(szWrite)<3
            szWrite(3)=1;
        end
        fwrite(fid,szWrite,'int32');
        %
        switch FI.Version
            case 10
                %
                AuxData=FI.Zone(i).AuxData;
                if ~isempty(AuxData)
                    for ad=1:size(AuxData,1)
                        %
                        % indicate auxdata entry
                        %
                        fwrite(fid,1,'int32');
                        %
                        % auxdata namestring="valuestring"
                        %
                        fwritestring(fid,AuxData{ad,1});
                        fwrite(fid,0,'int32');
                        fwritestring(fid,AuxData{ad,2});
                    end
                end
                %
                % finish auxdata block
                %
                fwrite(fid,0,'int32');
                %
        end
    end

    %
    % --- Write zone data ...
    %
    if ASCII
        nvalPerLine=20;
        Format=[repmat(' %12g',1,nvalPerLine) '\n'];
        for v=1:size(Di,nDi)
            fprintf(fid,Format,Di(ijk{:},v));
            if mod(nIJK,nvalPerLine)~=0
                fprintf(fid,'\n');
            end
        end
    end
end

%
% --- Write zone data ...
%
if ~ASCII
    %
    % 00 80 B2 43
    %
    fwrite(fid,[0 128 178 67],'uchar');
    %
    for i=1:length(FI.Zone)
        Di=FI.Zone(i).Data;
        %
        % --- Write zone data ...
        %
        % 00 80 95 43
        %
        fwrite(fid,[0 128 149 67],'uchar');
        %
        switch FI.Version
            case 9
                %
                % length of duplist followed by duplist
                %
                fwrite(fid,0,'int32');
                %fwrite(fid,duplist,'int32');
                %
        end
        %
        % 1=(SINGLE), 2=(DOUBLE), 3=(LONGINT), 4=(SHORTINT), 5=(BYTE), 6=(BIT)
        %
        fwrite(fid,ones(1,nV),'int32');
        %
        switch FI.Version
            case 10
                %
                % if 1, followed by VARSHARELIST (length=nV) of ZONE NUMBERS= value+1
                %
                fwrite(fid,0,'int32');
                %
                % if >=0, CONNECTIVITYSHAREZONE= value+1
                %
                fwrite(fid,-1,'int32');
                %
        end
        %
        % DATA in block format
        % FEDATA value data, 0, elementconnectivity
        %                    1 (in case of FECONNECT)
        %
        fwrite(fid,Di,'float32');
    end
end
FI.Check='OK';
fclose(fid);

function Str = freadstring(fid)
Loc = ftell(fid);
Str = fread(fid,[1 1024],'int32=>char');
Len = min(find(Str==0));
Str = Str(1:Len-1);
fseek(fid,Loc+4*Len,-1);

function fwritestring(fid,Str)
fwrite(fid,Str,'int32');
fwrite(fid,0,'int32');
