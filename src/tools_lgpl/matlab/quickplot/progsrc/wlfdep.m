function varargout=wlfdep(cmd,varargin)
%WLFDEP Read/write Delft3D-MOR field files.
%   FIELD=WLFDEP('read',FILENAME) read the classic Delft3D-MOR field file.
%
%   WLFDEP('write',FILENAME,FIELD) read the FIELD to the specified file in
%   classic Delft3D-MOR field format.
%
%   See also: WLDEP

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
        varargout=cell(1,nargout);
    end
    return
end

switch cmd
    case 'read'
        Dep=Local_depread(varargin{:});
        varargout={Dep};
    case 'write'
        Out=Local_depwrite(varargin{:});
        if nargout>0
            varargout{1}=Out;
        end
    otherwise
        error('Unknown command: %s',var2str(cmd))
end


function DP=Local_depread(filename)
% DEPREAD reads field data from a given filename
%    DEPTH=DEPREAD('FILENAME.DEP')

DP=[];

if strcmp(filename,'?')
    [fname,fpath]=uigetfile('*.*','Select Delft3D-MOR field file');
    if ~ischar(fname)
        return
    end
    filename=[fpath,fname];
end

fid=fopen(filename,'r','n','US-ASCII');
if fid<0
    error('Cannot open %s.',filename)
end
try
    fgetl(fid);
    dim=fscanf(fid,'%i',[1 2]);
    [DP,Cnt]=fscanf(fid,'%f',dim);
    DP=DP';
    fj=fscanf(fid,'%f',1);
    yeof=feof(fid) & isempty(fj);
    fclose(fid);
catch
    fclose(fid);
    rethrow(lasterror)
end
if Cnt~=prod(dim) || ~yeof
    error('Missing data or invalid file format while reading %s',filename)
end


function OK=Local_depwrite(filename,DP)
% DEPWRITE writes field data to a given filename
%
% Usage: depwrite('filename',Matrix)

if DP(end,end)~=-999
    switch input('Negate date points? (Y/N) ','s')
        case {'Y','y'}
            DP=-DP;
        otherwise
    end
    switch input('Grid extension: 9 (-999 values)/B (boundary values) /N (Don''t extend) ','s')
        case {'9'}
            DP=[DP -999*ones(size(DP,1),1); ...
                -999*ones(1,size(DP,2)+1)];
        case {'B','b'}
            DP=[DP DP(:,end); ...
                DP(end,:) DP(end,end)];
        otherwise
    end
end

DP(isnan(DP))=-999;

fid=fopen(filename,'w','n','US-ASCII');
fprintf(fid,'%s\n',filename);
fprintf(fid,'%i %i\n',fliplr(size(DP)));
fprintf(fid,strcat('%f',repmat(' %f',1,size(DP,2)-1),'\n'),DP');
fclose(fid);
OK=1;
