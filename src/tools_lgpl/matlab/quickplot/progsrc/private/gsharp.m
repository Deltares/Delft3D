function Out=gsharp(cmd,varargin)
%GSHARP Read/write GSharp files.
%
%   FileInfo=GSHARP('read',FileName)
%   Writes the structure Data to a GSharp folder file.
%
%   FileInfoOut=GSHARP('write',FileName,FileInfoIn)
%   Writes the structure Data to a GSharp folder file.

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

if nargin==0,
    if nargout>0
        Out=[];
    end
    return
end
switch cmd
    case 'read'
        Out=Local_read_file(varargin{:});
    case 'write'
        Out=Local_write_file(varargin{:});
    otherwise
        uiwait(msgbox('unknown command','modal'));
end


function FI=Local_read_file(filename)
fid=fopen(filename,'r','l');
X=fread(fid,[1 2],'int32');
if ~isequal(X,[1 0])
    error('Unexpected start of file.')
end

FI.FileType='GSharp Folder';
FI.FileName=filename;

lFolder=fread(fid,1,'int32');

X=fread(fid,[1 2],'int32');
if ~isequal(X,[-1 -1])
    error('Unexpected start of file.')
end

Str=char(fread(fid,[1 lFolder],'*uchar'));
FI.Folder=Str(1:end-1);

nVar=fread(fid,1,'int32');

for i=1:nVar
    lVarName=fread(fid,1,'int32');
    Type=fread(fid,1,'int32');
    Sz=fread(fid,[1 3],'int32');
    N=prod(Sz);
    X=fread(fid,1,'int32');
    if ~isequal(X,-1)
        fclose(fid);
        error('Error reading file.')
    end
    Str=char(fread(fid,[1 lVarName],'*uchar'));
    FI.Var(i).Name=Str(1:end-1);
    switch Type
        case 2
            nBytes=fread(fid,1,'int32');
            Data=fread(fid,N,'double');
            Data=reshape(Data,Sz);
        case 5
            Data=cell(Sz);
            for n=1:N
                lStr=fread(fid,1,'int32');
                Str=char(fread(fid,[1 lStr],'*uchar'));
                Data{n}=Str(1:end-1);
            end
        otherwise
            error('Type %i not yet implemented.',Type)
    end
    FI.Var(i).Data=Data;
end
fclose(fid);


function FI=Local_write_file(filename,FileInfo)

FolderName='MATLAB';
lFolderName=length(FolderName)+1;
if ~isstruct(FileInfo)
    FI.Data=FileInfo;
else
    FI=FileInfo;
end
Flds=fieldnames(FileInfo);
nFlds=length(Flds);

for i=1:nFlds
    Data=getfield(FI,Flds{i});
    if ndims(Data)>3
        error('Cannot write data with more than 3 dimensions to GSharp Folder file.')
    elseif isempty(Data)
        error('Cannot write empty data to GSharp Folder file.')
    elseif iscellstr(Data)
        if any(cellfun('ndims',Data(:),1)>2) | any(cellfun('size',Data(:),1)>1)
            error('Only single line strings can be written to a GSharp Folder file.')
        else
            % OK
        end
    elseif iscell(Data)
        error('Cannot write cell data (non-cellstr) to GSharp Folder file.')
    elseif isstruct(Data)
        error('Cannot write structure data to GSharp Folder file.')
    elseif isnumeric(Data)
        % OK, convert to double when necessary ...
    elseif ischar(Data)
        error('Cannot write %s data to GSharp Folder file.',class(Data))
    else
        error('Cannot write %s data to GSharp Folder file.',class(Data))
    end
end

fid=fopen(filename,'w','l');

fwrite(fid,[1 0 lFolderName -1 -1],'int32');
fwrite(fid,FolderName,'uchar'); fwrite(fid,0,'uchar');
fwrite(fid,nFlds,'int32');

for i=1:nFlds
    Name=Flds{i};
    lName=length(Name)+1;
    Data=getfield(FI,Name);
    fwrite(fid,lName,'int32');
    if iscell(Data) % strings
        fwrite(fid,5,'int32');
    else % double
        fwrite(fid,2,'int32');
    end
    Sz=size(Data); N=prod(Sz);
    if length(Sz)==2
        Sz(3)=1;
    end
    fwrite(fid,Sz,'int32');
    fwrite(fid,-1,'int32');
    fwrite(fid,Name,'uchar'); fwrite(fid,0,'uchar');
    if iscell(Data) % strings
        for n=1:N
            Str=Data{n};
            fwrite(fid,length(Str)+1,'int32');
            fwrite(fid,Str,'uchar'); fwrite(fid,0,'uchar');
        end
    else % double
        fwrite(fid,N*8,'int32');
        fwrite(fid,double(Data(:)),'float64');
    end
end

fclose(fid);
