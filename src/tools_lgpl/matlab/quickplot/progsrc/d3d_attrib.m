function varargout=d3d_attrib(cmd,varargin)
%D3D_ATTRIB Read/write a various Delft3D attribute files.
%   W = D3D_ATTRIB('read',FILENAME) reads the specified file and returns a
%   structure W with fields FileName and Type and other fields depending on
%   the content of the file. The Type field indicates the type of the
%   file/content. This routine can handle various attribute files and will
%   automatically detect the file type based on the file contents:
%    * Delft3D open boundary
%        Fields: Name, BndType, Forcing, MN, Alpha, Profile, AstrSta1, AstrSta2
%    * Delft3D rigid sheet
%        Fields: MNKu, CHARu, MNKv, CHARv
%    * Delft3D 3d gate
%        Fields: MNKu, MNKv
%    * Delft3D weir
%        Fields: MNu, CHARu, MNv, CHARv
%    * SIMONA/WAQUA weir
%        Fields: MNu, CHARu, MNv, CHARv
%    * Delft3D thin dam
%        Fields: MNu, MNv
%    * SIMONA/WAQUA thin dam
%        Fields: MNu, MNv
%    * Delft3D dry points
%        Fields: MN
%    * Delft3D discharge stations
%        Fields: Name, Interpolation, MNK,DischType, MNK_out
%    * Delft3D cross-sections
%        Fields: Name, MNMN
%    * Delf3D observation points
%        Fields: Name, MN
%
%   D3D_ATTRIB('write',W,FILENAME) writes the data of structure W to a file
%   with name FILENAME. The structure should specify the file type and
%   include the fields specified for that particular file type indicated
%   above.
%
%   See also: WLGRID, WLDEP.

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

if nargin<2
    error('Not enough input arguments.')
end

switch cmd
    case 'read'
        Out = Local_read_attrib(varargin{:});
        varargout = {Out};
    case 'write'
        Out = Local_write_attrib(varargin{:});
        if nargout>0
            varargout{1} = Out;
        end
    case 'convert'
        Out = Local_convert_attrib(varargin{:});
        if nargout>0
            varargout{1} = Out;
        end
    case 'supported_types'
        Out = Local_supported_types;
    otherwise
        error('Unknown command')
end

function Types = Local_supported_types
Types = {'openboundary', 'rigidsheet', '3dgate', 'weir', 'weir-waqua', ...
    'thindam', 'thindam-waqua', 'drypoint', 'cross-sections', ...
    'discharge stations', 'observation points', 'barriers'};


function Out = Local_read_attrib(filename,filetype)
if (nargin==0) || strcmp(filename,'?')
    [fname,fpath]=uigetfile('*.*','Select attribute file');
    if ~ischar(fname)
        return
    end
    filename=fullfile(fpath,fname);
end

Out.FileName=filename;
Out.Check='OK';
fid = -1;
if nargin<2 || isempty(filetype)
    types = Local_supported_types;
else
    types = {filetype};
end
fid=fopen(filename,'r','n','US-ASCII');
if fid<0
    error('Can''t open file: %s.',filename)
end
for tpC = types
    tp = tpC{1};
    Out.Type = tp;
    try
        switch tp
            case 'openboundary'
                i=0;
                while 1
                    Line=fgetl(fid);
                    if (~ischar(Line) || isempty(deblank(Line)) ) && feof(fid)
                        break
                    end
                    i=i+1;
                    Name{i,1}=deblank(Line(1:20));
                    [values,COUNT,ERRMSG,NEXTINDEX] = sscanf(Line(21:end),' %[A-Za-z] %[A-Za-z] %d %d %d %d %f',7);
                    BndType(i) = upper(char(values(1)));
                    Forcing(i) = upper(char(values(2)));
                    MN(i,1:4) = values(3:6)';
                    Alpha(i) = values(7);
                    Remainder = Line(20+NEXTINDEX:end);
                    j = 1;
                    Tokens = {};
                    while ~isempty(Remainder)
                        [tok,Remainder] = strtok(Remainder);
                        if ~isempty(tok)
                            Tokens{j} = tok;
                            j = j+1;
                        end
                    end
                    %
                    % profiles = {'uniform','logarithmic','3d-profile'};
                    switch length(Tokens)
                        case 0
                            if strcmp(Forcing(i),'A')
                                error('Astronomical stations missing')
                            end
                            Profile{i} = '';
                            AComp1{i} = '';
                            AComp2{i} = '';
                        case 1
                            if strcmp(Forcing(i),'A')
                                error('Astronomical stations missing')
                            end
                            Profile{i} = Tokens{1};
                            AComp1{i} = '';
                            AComp2{i} = '';
                        case 2
                            Profile{i} = '';
                            AComp1{i} = Tokens{1};
                            AComp2{i} = Tokens{2};
                        case 3
                            Profile{i} = Tokens{1};
                            AComp1{i} = Tokens{2};
                            AComp2{i} = Tokens{3};
                    end
                end
                Out.Name = Name;
                Out.BndType = BndType;
                Out.Forcing = Forcing;
                Out.MN = MN;
                Out.Alpha = Alpha;
                Out.Profile = Profile;
                Out.AstrSta1 = AComp1;
                Out.AstrSta2 = AComp2;
            case 'rigidsheet' % or porous plate
                [Data,N]=fscanf(fid,' %[uUvV] %i %i %i %i %i %i %f',[8 inf]);
                erryes=~feof(fid);
                if erryes || round(N/8)~=N/8
                    error('Error reading file.')
                end
                U=upper(char(Data(1,:)))=='U';
                % The following "if isempty" statements are necessary for the standalone version
                Out.MNKu=Data(2:7,U)'; if isempty(Out.MNKu), Out.MNKu=zeros(0,6); end
                Out.CHARu=Data(8,U)'; if isempty(Out.CHARu), Out.CHARu=zeros(0,1); end
                Out.MNKv=Data(2:7,~U)'; if isempty(Out.MNKv), Out.MNKv=zeros(0,6); end
                Out.CHARv=Data(8,~U)'; if isempty(Out.CHARv), Out.CHARv=zeros(0,1); end
                kmax=max(1,max(max(Out.MNKu(:,5:6))));
                kmax=max(kmax,max(max(Out.MNKv(:,5:6))));
                Out.KMax=kmax;
            case '3dgate'
                [Data,N]=fscanf(fid,' %[uUvV] %i %i %i %i %i %i',[7 inf]);
                erryes=~feof(fid);
                if erryes || round(N/7)~=N/7
                    error('Error reading file.')
                end
                U=upper(char(Data(1,:)))=='U';
                % The following "if isempty" statements are necessary for the standalone version
                Out.MNKu=Data(2:7,U)'; if isempty(Out.MNKu), Out.MNKu=zeros(0,6); end
                Out.CHARu=Data([],U)'; if isempty(Out.CHARu), Out.CHARu=zeros(0,0); end
                Out.MNKv=Data(2:7,~U)'; if isempty(Out.MNKv), Out.MNKv=zeros(0,6); end
                Out.CHARv=Data([],~U)'; if isempty(Out.CHARv), Out.CHARv=zeros(0,0); end
                kmax=max(1,max(max(Out.MNKu(:,5:6))));
                kmax=max(kmax,max(max(Out.MNKv(:,5:6))));
                Out.KMax=kmax;
            case 'weir'
                [Data,N]=fscanf(fid,' %[uUvV] %i %i %i %i %f %f %f',[8 inf]);
                erryes=~feof(fid);
                if erryes || round(N/8)~=N/8
                    error('Error reading file.')
                end
                U=upper(char(Data(1,:)))=='U';
                % The following "if isempty" statements are necessary for the standalone version
                Out.MNu=Data(2:5,U)'; if isempty(Out.MNu), Out.MNu=zeros(0,4); end
                Out.CHARu=Data(6:8,U)'; if isempty(Out.CHARu), Out.CHARu=zeros(0,3); end
                Out.MNv=Data(2:5,~U)'; if isempty(Out.MNv), Out.MNv=zeros(0,4); end
                Out.CHARv=Data(6:8,~U)'; if isempty(Out.CHARv), Out.CHARv=zeros(0,3); end
            case 'weir-waqua'
                %W    3   26    8.86    1.65    1.57    0.00    0.00    0.00 'K' ' ' 1 0
                [Data,N]=fscanf(fid,' %*[W] %i %i %f %f %f %f %f %f ''%1c'' ''%1c'' %i %i',[12 inf]);
                erryes=~feof(fid);
                if erryes || round(N/12)~=N/12
                    error('Error reading file.')
                end
                U=Data(11,:)~=0;
                V=Data(12,:)~=0;
                % The following "if isempty" statements are necessary for the standalone version
                Out.MNu=Data([1:2 1:2],U)'; if isempty(Out.MNu), Out.MNu=zeros(0,4); end
                Out.CHARu=Data([3:5 9 11],U)'; if isempty(Out.CHARu), Out.CHARu=zeros(0,5); end
                Out.MNv=Data([1:2 1:2],V)'; if isempty(Out.MNv), Out.MNv=zeros(0,4); end
                Out.CHARv=Data([6:8 10 12],V)'; if isempty(Out.CHARv), Out.CHARv=zeros(0,5); end
            case 'thindam'
                [Data,N]=fscanf(fid,' %i %i %i %i %[uUvV]',[5 inf]);
                erryes=~feof(fid);
                if erryes || round(N/5)~=N/5
                    error('Error reading file.')
                end
                U=upper(char(Data(5,:)))=='U';
                % The following "if isempty" statements are necessary for the standalone version
                Out.MNu=Data(1:4,U)'; if isempty(Out.MNu), Out.MNu=zeros(0,4); end
                Out.CHARu=Data([],U)'; if isempty(Out.CHARu), Out.CHARu=zeros(0,0); end
                Out.MNv=Data(1:4,~U)'; if isempty(Out.MNv), Out.MNv=zeros(0,4); end
                Out.CHARv=Data([],~U)'; if isempty(Out.CHARv), Out.CHARv=zeros(0,0); end
            case 'thindam-waqua'
                [Data,N]=fscanf(fid,' %1[mMnN]%*[^=]= %i %i %i',[4 inf]);
                erryes=~feof(fid);
                if erryes || round(N/4)~=N/4
                    error('Error reading file.')
                end
                U=upper(char(Data(1,:)))=='M';
                % The following "if isempty" statements are necessary for the standalone version
                Out.MNu=Data([2 3 2 4],U)'; if isempty(Out.MNu), Out.MNu=zeros(0,4); end
                Out.CHARu=Data([],U)'; if isempty(Out.CHARu), Out.CHARu=zeros(0,0); end
                Out.MNv=Data([3 2 4 2],~U)'; if isempty(Out.MNv), Out.MNv=zeros(0,4); end
                Out.CHARv=Data([],~U)'; if isempty(Out.CHARv), Out.CHARv=zeros(0,0); end
            case 'drypoint'
                [Data,N]=fscanf(fid,' %i %i %i %i',[4 inf]);
                erryes=~feof(fid);
                if erryes || round(N/4)~=N/4
                    error('Error reading file.')
                end
                if isempty(Data)
                    Data=zeros(0,4);
                else
                    Data=Data';
                end
                Out.MN=Data;
            case 'discharge stations'
                i=0;
                while 1
                    Line=fgetl(fid);
                    if (~ischar(Line) || isempty(deblank(Line)) ) && feof(fid)
                        break
                    end
                    i=i+1;
                    Name{i,1}=deblank(Line(1:20));
                    [X,n,er,ni]=sscanf(Line(21:end),' %1s %i %i %i',[1 4]);
                    Interpolation(i,1)=char(X(1));
                    MNK(i,1:3)=X(2:4);
                    MNK_out(i,1:3)=[NaN NaN NaN];
                    [X,n,er]=sscanf(Line(20+ni:end),' %1s %i %i %i',[1 4]);
                    if n==0
                        DischType{i,1}='normal discharge';
                    else
                        switch upper(char(X(1)))
                            case 'N'
                                DischType{i,1}='normal discharge';
                            case 'W'
                                DischType{i,1}='walking discharge';
                            case 'P'
                                DischType{i,1}='power station';
                                if n<4
                                    error('Missing outlet location');
                                end
                                MNK_out(i,1:3)=X(2:4);
                            case {'C','D','E','U'}
                                DischType{i,1}='culvert';
                                if n<4
                                    error('Missing outlet location');
                                end
                                MNK_out(i,1:3)=X(2:4);
                            otherwise
                                error('Unknown discharge type: %s',char(X(1)))
                        end
                    end
                end
                Out.Name=Name;
                Out.Interpolation=Interpolation;
                Out.MNK=MNK;
                Out.DischType=DischType;
                if any(~isnan(MNK_out(:)))
                    Out.MNK_out=MNK_out;
                end
            case 'cross-sections'
                i=0;
                while 1
                    Line=fgetl(fid);
                    if ~ischar(Line) || isempty(deblank(Line))
                        if feof(fid)
                            break
                        else % skip empty lines (mostly at the end of the file)
                            continue
                        end
                    end
                    i=i+1;
                    Name{i,1}=deblank(Line(1:20));
                    MNMN(i,1:4)=sscanf(Line(21:end),' %i %i %i %i',[1 4]);
                end
                Out.Name=Name;
                Out.MNMN=MNMN;
            case 'barriers'
                i=0;
                while 1
                    Line=fgetl(fid);
                    if ~ischar(Line)
                        break
                    end
                    Line=strtrim(Line);
                    if isempty(Line)
                        % skip empty lines
                        continue
                    elseif Line(1)=='*'
                        % skip comments
                        continue
                    end
                    i=i+1;
                    Name{i,1}=Line(1:20);
                    [v,n,err,j]=sscanf(Line(21:end),'%[uUvV] %i %i %i %i',[1 5]);
                    U(i)=upper(v(1))=='U';
                    MNMN(i,1:4)=v(1,2:5);
                end
                Out.Name=Name;
                Out.U   =U;
                Out.MNMN=MNMN;
            case 'observation points'
                i=0;
                while 1
                    Line=fgetl(fid);
                    if ~ischar(Line)
                        break
                    elseif isempty(deblank(Line))
                        continue
                    end
                    i=i+1;
                    Name{i,1} = deblank(Line(1:20));
                    mn = sscanf(Line(21:end),' %i %i',[1 2]);
                    if any(mn <= 0)
                        error('Invalid MN coordinates (%d,%d) for observation point "%s"',mn,Name{i,1})
                    end
                    MN(i,1:2) = mn;
                end
                Out.Name=Name;
                Out.MN=MN;
            otherwise
                error('Reading file format ''%s'' not supported.',tp)
        end
        fclose(fid);
        return
    catch err
        if length(types)==1
            fclose(fid);
            rethrow(err)
        else
            fseek(fid,0,-1);
        end
    end
end
Out.Check='NotOK';


function OK=Local_write_attrib(Out,filename)
if isstruct(filename) && ischar(Out)
    tmp = Out;
    Out = filename;
    filename = tmp;
end
if ~isfield(Out,'Type')
    Out.Type='weir';
end
fid=fopen(filename,'w','n','US-ASCII');
switch Out.Type
    case 'openboundary'
        for i = 1:length(Out.Name)
            fprintf(fid,'%-20s %c %c %5i %5i %5i %5i %15f', ...
                Out.Name{i},Out.BndType(i),Out.Forcing(i),Out.MN(i,:),Out.Alpha(i));
            if isempty(strfind('ZN',Out.Forcing)) && ~isempty(Out.Profile{i})
                fprintf(fid,' %s',Out.Profile{i});
            end
            if strcmp(Out.Forcing,'A')
                fprintf(fid,' %s %s',Out.AstrSta1{i},Out.AstrSta2{i});
            end
            fprintf(fid,'\n');
        end
    case 'rigidsheet'
        Data=[repmat(abs('U'),size(Out.MNKu,1),1) Out.MNKu Out.CHARu
            repmat(abs('V'),size(Out.MNKv,1),1) Out.MNKv Out.CHARv]';
        fprintf(fid,' %c %5i %5i %5i %5i %5i %5i %12f\n',Data);
    case '3dgate'
        Data=[repmat(abs('U'),size(Out.MNKu,1),1) Out.MNKu
            repmat(abs('V'),size(Out.MNKv,1),1) Out.MNKv]';
        fprintf(fid,' %c %5i %5i %5i %5i %5i %5i\n',Data);
    case 'weir'
        Data=[repmat(abs('U'),size(Out.MNu,1),1) Out.MNu Out.CHARu
            repmat(abs('V'),size(Out.MNv,1),1) Out.MNv Out.CHARv]';
        fprintf(fid,' %c %5i %5i %5i %5i %12f %12f %12f\n',Data);
    case 'weir-waqua'
        %W    3   26    8.86    1.65    1.57    0.00    0.00    0.00 'K' ' ' 1 0
        Nu = size(Out.MNu,1);
        Nv = size(Out.MNv,1);
        Data = [Out.MNu(:,1:2) Out.CHARu(:,1:3) zeros(Nu,3) Out.CHARu(:,4) zeros(Nu,1)+32 Out.CHARu(:,5) zeros(Nu,1)
            Out.MNv(:,1:2) zeros(Nv,3) Out.CHARv(:,1:3) zeros(Nv,1)+32 Out.CHARv(:,4) zeros(Nv,1) Out.CHARv(:,5)];
        Data = sortrows(Data);
        I = find(all(diff(Data(:,1:2))==0,2));
        Data(I+1,[6:8 10 12])=Data(I,[6:8 10 12]);
        Data(I,:)=[];
        fprintf(fid,'W %4i %4i %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f ''%1c'' ''%1c'' %1i %1i\n',Data');
    case 'thindam'
        Data=[Out.MNu repmat(abs('U'),size(Out.MNu,1),1)
            Out.MNv repmat(abs('V'),size(Out.MNv,1),1)]';
        fprintf(fid,' %5i %5i %5i %5i %c\n',Data);
    case 'thindam-waqua'
        Data=Out.MNu(:,[1 2 4])';
        fprintf(fid,'mnn : mnnl =  %5i %5i %5i\n',Data);
        Data=Out.MNv(:,[2 1 3])';
        fprintf(fid,'nmm : nmml =  %5i %5i %5i\n',Data);
    case 'drypoint'
        fprintf(fid,' %5i %5i %5i %5i %c\n',Out.MN');
    case 'discharge stations'
        for i=1:length(Out.Name)
            fprintf(fid,'%-20s %c %5i %5i %5i',Out.Name{i},Out.Interpolation(i),Out.MNK(i,:));
            switch Out.DischType{i}
                case 'normal discharge'
                    fprintf(fid,'\n');
                case 'walking discharge'
                    fprintf(fid,' W\n');
                case 'power station'
                    fprintf(fid,' P %5i %5i %5i\n',Out.MNK_out(i,:));
                otherwise
                    fclose(fid);
                    error('Don''t know how to write %s',Out.DischType{i})
            end
        end
    case 'cross-sections'
        for i=1:length(Out.Name)
            fprintf(fid,'%-20s %5i %5i %5i %5i\n',Out.Name{i},Out.MNMN(i,:));
        end
    case 'observation points'
        for i=1:length(Out.Name)
            fprintf(fid,'%-20s %5i %5i\n',Out.Name{i},Out.MN(i,:));
        end
    otherwise
        fclose(fid);
        error('Write command does not support type: %s.',Out.Type)
end
fclose(fid);
OK=1;

function Out=Local_convert_attrib(In,newtype)
if ~isfield(In,'Type')
    In.Type = 'weir';
end
if isequal(newtype,In.Type)
    Out = In;
    return
end
%
Out = In;
Out.FileName = ['Conversion of ' In.FileName];
Out.Type = newtype;
converted = 0;
switch newtype
    case 'weir'
        switch In.Type
            case 'weir-waqua'
                converted = 1;
                Nu = size(Out.MNu,1);
                Out.CHARu = [ones(Nu,1) -In.CHARu(:,1) ones(Nu,1)];
                Nv = size(Out.MNv,1);
                Out.CHARv = [ones(Nv,1) -In.CHARv(:,1) ones(Nv,1)];
        end
    case 'weir-waqua'
        switch In.Type
            case 'weir'
                converted = 1;
                %
                %expand if MNu(1:2) not equal to MNu(3:4) not yet implemented
                %(idem for MNv)
                %
                Nu = size(Out.MNu,1);
                Out.CHARu = [-In.CHARu(:,2) zeros(Nu,2) 32+zeros(Nu,1) ones(Nu,1)];
                Nv = size(Out.MNv,1);
                Out.CHARv = [-In.CHARv(:,2) zeros(Nv,2) 32+zeros(Nv,1) repmat(2,Nv,1)];
                %
                %get sill levels from depth file before and after weir not yet
                %implemented
                %
        end
    case 'thindam'
        switch In.Type
            case 'thindam-waqua'
                converted = 1;
        end
    case 'thindam-waqua'
        switch In.Type
            case 'thindam'
                converted = 1;
                %
                %expand if MNu(1) not equal to MNu(3) not yet implemented
                %(idem for MNv(2/4))
                %
        end
end
if converted
    error('Conversion from %s to %s not implemented.',In.Type,newtype)
end
