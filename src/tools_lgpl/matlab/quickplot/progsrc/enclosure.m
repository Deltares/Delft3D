function varargout=enclosure(cmd,varargin)
%ENCLOSURE Read/write enclosure files and convert enclosures.
%   ENCLOSURE provides support for enclosure operations in general, like
%   reading, writing and applying enclosures.
%
%   MN=ENCLOSURE('read',FILENAME) reads a Delft3D or WAQUA enclosure file.
%
%   ENCLOSURE('write',FILENAME,MN) writes a Delft3D enclosure file.
%
%   ENCLOSURE('write',FILENAME,MN,'waqua') writes a WAQUA enclosure file.
%
%   [X,Y] = ENCLOSURE('apply',MN,Xorg,Yorg) applies the enclosure,
%   replacing grid coordinates outside the enclosure by NaN.
%
%   MASK = ENCLOSURE('inside',MN,SZ) returns an array of size SZ containing
%   1 in the active part of the domain, and 0 outside.
%
%   MN = ENCLOSURE('extract',X,Y) extracts the enclosure indices from X and
%   Y matrices containing NaN for points outside the enclosure.
%
%   [XC,YC] = ENCLOSURE('coordinates',MN,X,Y) obtain the X,Y coordinates
%   from M,N enclosure indices. If the MN argument is skipped, the
%   enclosure indices will first be determined using the extract call
%   above.
%
%   See also WLGRID.

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
    error('Missing input arguments.')
end

switch lower(cmd)
    case 'read'
        Enc = Local_encread(varargin{:});
        varargout = {Enc};
    case 'extract'
        Enc = Local_encextract(varargin{:});
        varargout = {Enc};
    case 'thindam'
        [MNu,MNv] = Local_enc2uv(varargin{:});
        varargout = {MNu MNv};
    case 'inside'
        MN = varargin{1};
        sz = varargin{2} + 1;
        Ind0 = Local_mask_WL(MN);
        szInd0 = size(Ind0);
        nd = ndims(szInd0);
        %
        idx = cell(1,nd);
        for i = nd:-1:1
            idx{i} = 1:min(sz(i),szInd0(i));
        end
        %
        Ind = false(sz);
        Ind(idx{:}) = Ind0(idx{:});
        %
        varargout = {Ind};
    case 'apply'
        [X,Y] = Local_encapply(varargin{:});
        varargout = {X Y};
    case 'coordinates'
        XY = Local_coordinates(varargin{:});
        if nargout == 2
            varargout = {XY(:,1) XY(:,2)};
        else
            varargout = {XY};
        end
    case 'write'
        Local_encwrite(varargin{:});
    otherwise
        error('Unknown command')
end


function Enc = Local_encread(filename)
% * ENC=ENCLOSURE('read',FileName)
%   % Delft3D, Waqua
% read an enclosure file
Enc=[];
delimiters = {'(';')';',';';'};

if (nargin==0) | strcmp(filename,'?')
    [fname,fpath]=uigetfile('*.*','Select enclosure file');
    if ~ischar(fname)
        return
    end
    filename=fullfile(fpath,fname);
end

% Grid enclosure file
fid=fopen(filename,'r','n','US-ASCII');
err=[];
ln = 0;
if fid>0
    try
        Enc = NaN(1000,2); % avoid extending array for every line
        iP  = 0;
        while 1
            ln = ln+1;
            line=fgetl(fid);
            if ~ischar(line)
                break
            end
            
            %
            % TK : X returned empty on comgrid scaloost model waqua,
            %      remove the delimiters
            %
            org_line = line;
            for i_lim = 1: length(delimiters)
                line(line == delimiters{i_lim}) = ' ';
            end
            
            [X,nX,errX] = sscanf(line,'%i');
            
            if nX==0
                % empty line
            elseif nX ~= round(nX/2)*2
                error('Every enclosure line should contain a pair of numbers; check line %i:\n%s',ln,org_line)
            elseif nX ==2
                % normal line
                if size(Enc,1)<iP+1
                    Enc = [Enc;NaN(size(Enc))]; % avoid extending array for every line
                end
                Enc(iP+1,:) = X';
                iP = iP+1;
            elseif ~isempty(errX)
                % Multiple coordinate pairs on a single line are accepted
                % for WAQUA but then no other text on the line is allowed.
                % Otherwise, we would interpret a thin dam file containing
                % lines like
                %       8      4      8      2 U
                % also an enclosure file.
                error('Error reading data from line %i:\n%s',ln,org_line)
            elseif nX>2
                %
                % Multiple coordinate pairs on 1 single line ==> reshape
                %
                nX = nX/2;
                if size(Enc,1)<iP+nX
                    Enc = [Enc;NaN(size(Enc))]; % avoid extending array for every line
                end
                X = reshape(X,2,nX)';
                Enc(iP+1:iP+nX,:) = X;
                iP = IP+nX;
            end
        end
        %
        Enc = Enc(1:iP,:); % avoid extending array for every line
    catch err
    end
    fclose(fid);
    if ~isempty(err)
        rethrow(err)
    end
else
    error('Error opening file.')
end


function Ind = Local_mask_DP(MN)
Ind=repmat(logical(0),max(MN));
i=1;
sMN1=size(MN,1);
while i<=sMN1
    j=find(MN(:,1)==MN(i,1) & MN(:,2)==MN(i,2));
    j=j(j>i);

    MNseg=MN(i:j,:);
    dMN1=diff(MNseg(:,1));
    ilist=find(dMN1);
    for i=ilist'
        if dMN1(i)>0
            m=MNseg(i,1):(MNseg(i+1,1)-1);
            n=1:MNseg(i,2)-1;
        else
            m=MNseg(i+1,1):(MNseg(i,1)-1);
            n=1:MNseg(i,2)-1;
        end
        Ind(m,n)=~Ind(m,n);
        %imagesc(setnan(~Ind)); drawnow; pause
    end

    i=j+1;
end


function Ind = Local_mask_WL(MN)
Ind=repmat(logical(0),max(MN)+1);
i=1;
first=1;
sMN1=size(MN,1);
while i<=sMN1
    j=find(MN(:,1)==MN(i,1) & MN(:,2)==MN(i,2));
    j=j(j>i);

    MNseg=MN(i:j,:);
    clockw = clockwise(MNseg(:,1),MNseg(:,2));
    outer = double(xor(clockw>0,~first));
    dMN1=diff(MNseg(:,1));
    dMN2=diff(MNseg(:,2));
    ilist=find(dMN1);
    for i=ilist'
        if i==1
            dMNp=dMN2(end);
        else
            dMNp=dMN2(i-1);
        end
        if i==size(dMN1,1)
            dMNn=dMN2(1);
        else
            dMNn=dMN2(i+1);
        end
        if dMN1(i)>0
            m=(MNseg(i,1)+(dMNp>0)*outer+(1-outer)*(dMNp<0)):(MNseg(i+1,1)-(dMNn<0)*outer-(1-outer)*(dMNn>0));
            n=MNseg(i,2)+1-outer:size(Ind,2);
        else
            m=(MNseg(i+1,1)+(dMNn>0)*outer+(1-outer)*(dMNn<0)):(MNseg(i,1)-(dMNp<0)*outer-(1-outer)*(dMNp>0));
            n=MNseg(i,2)+outer:size(Ind,2);
        end
        Ind(m,n)=~Ind(m,n);
        %imagesc(Ind); drawnow
    end

    first=0;
    i=j+1;
end


function [MNu,MNv]=Local_enc2uv(MN)
% * [MNu,MNv]=ENCLOSURE('thindam',MN)
if isempty(MN)
    MNu = zeros(0,4);
    MNv = zeros(0,4);
else
    Ind=Local_mask_WL(MN);
    %imagesc(Ind)
    [m,n]=find(Ind(1:end-1,:)~=Ind(2:end,:));
    MNu=[m n m n];
    [m,n]=find(Ind(:,1:end-1)~=Ind(:,2:end));
    MNv=[m n m n];
end


function MN=Local_encextract(X,Y)
% * MN=ENCLOSURE('extract',X,Y)
%   % can be implemented using contour
%   % c=contourc((0:62)+0.5,(0:98)+0.5,[0 0;0 Act],[0.5 0.5])
%   %      ---> (i,j+.5)    ---> (i,j) (i,j+1)   ----> remove duplicates
if nargin==1
    Act=~isnan(X);
else
    Act=~isnan(X) & ~isnan(Y);
end
Z=zeros(size(Act)+2);
Z(2:end-1,2:end-1)=Act;
c=contours((0:size(Act,2)+1)+0.5,(0:size(Act,1)+1)+0.5, ...
    Z, [0.5 0.5]);
MN={};
i=1;
while i<size(c,2)
    N=c(2,i);
    mn=zeros(2,2*N);
    c(:,i+(1:N))=floor(c(:,i+(1:N)));
    j=0;
    for n=1:N
        i=i+1;
        if n>1
            if c(1,i)>c(1,i-1) & c(2,i)<c(2,i-1)
                j=j+1;
                mn(:,j)=c(:,i)+[0;1];
            elseif c(1,i)<c(1,i-1) & c(2,i)>c(2,i-1)
                j=j+1;
                mn(:,j)=c(:,i)+[1;0];
            end
        end
        j=j+1;
        mn(:,j)=c(:,i);
    end
    i=i+1;
    mn(:,j+1:end)=[];
    % simplify enclosure: delete double points
    mn=remove_double_points(mn);
    % simplify enclosure: delete points on straight lines
    dmn=diff(mn,1,2);
    mn(:,1+find(~any(diff(dmn,1,2))))=[];
    % remove points from narrow sections
    p1=2;
    dm = [1;0];
    dn = [0;1];
    while p1<size(mn,2)
        p2=p1+1;
        while p2<=size(mn,2)
            if isequal(mn(:,p1),mn(:,p2))
                dmn = mn(:,p1)-mn(:,p1-1);
                if dmn(2)==0 & dmn(1)~=0
                    if dmn(1)>0
                        mn=detangle(mn,p1,p2,dm,dn);
                    else%if dmn(1)<0
                        mn=detangle(mn,p1,p2,-dm,-dn);
                    end
                elseif dmn(1)==0 & dmn(2)~=0
                    if dmn(2)>0
                        mn=detangle(mn,p1,p2,dn,-dm);
                    else%if dmn(2)<0
                        mn=detangle(mn,p1,p2,-dn,dm);
                    end
                else
                    warning(sprintf('Enclosure problem at (M,N) = %i,%i',mn(1,p1),mn(2,p1)))
                end
                break
            end
            p2=p2+1;
        end
        p1=p1+1;
    end
    %
    MN{end+1}=mn;
end
MN=cat(2,MN{:});
% transpose and flip
MN=MN([2 1],:)';

function mn=detangle(mn,p1,p2,dm,dn)
mn0 = mn(:,p1);
if p2==size(mn,2)
    mn = [mn(:,2:p1-1) mn0-dm mn0-dn mn(:,p1+1:p2-1) mn0+dm mn0+dn mn(:,2)];
else
    mn = [mn(:,1:p1-1) mn0-dm mn0-dn mn(:,p1+1:p2-1) mn0+dm mn0+dn mn(:,p2+1:end)];
end
mn = remove_double_points(mn);

function mn=remove_double_points(mn)
% simplify enclosure: delete points on straight lines
dmn=diff(mn,1,2);
mn(:,~any(dmn))=[];


function [X,Y]=Local_encapply(MN,Xorg,Yorg)
% * [X,Y]=ENCLOSURE('apply',ENC,Xorg,Yorg)
%   % can be implemented using inpolygon
%   % in=inpolygon(XI,YI,x-0.5,y-0.5)
X = Xorg;
Y = Yorg;
if ~isempty(MN)
    Ind = Local_mask_DP(MN);
    if size(Ind,1)>size(X,1)
        Ind=Ind(1:size(X,1),:);
    else
        Ind(size(X,1),1)=0;
    end
    if size(Ind,2)>size(X,2)
        Ind=Ind(:,1:size(X,2));
    else
        Ind(1,size(X,2))=0;
    end
    Ind=Ind~=1;
    X(Ind)=NaN;
    Y(Ind)=NaN;
end


function XY=Local_coordinates(MNall,X,Y)
% * [XC,YC]=ENCLOSURE('coordinates',ENC,X,Y)
% * [XC,YC]=ENCLOSURE('coordinates',X,Y)
% obtain X,Y coordinates from M,N enclosure
if nargin==2
    Y=X;
    X=MNall;
    MNall=Local_encextract(X);
end
XY=cell(0,1);
iMN=sub2ind(size(X)+1,MNall(:,1),MNall(:,2));
%
% Break enclosure up into segments
%
s1=1;
while s1<size(MNall,1)
    s2=find(iMN==iMN(s1));
    s2=min(s2(s2>s1));
    MN=MNall(s1:s2,:);
    if s1>1
        XY{end+1,1}=[NaN NaN];
        %
        % a hole should rotate clockwise
        %
        if clockwise(MN(:,1),MN(:,2))<0
            MN=flipud(MN);
        end
    else
        %
        % the outer boundary should rotate anti-clockwise
        %
        if clockwise(MN(:,1),MN(:,2))>0
            MN=flipud(MN);
        end
    end
    s1=s2+1;
    %
    % Expand to single grid cell per step
    %
    dMN=diff(MN,1,1);
    ndMN=max(abs(dMN),[],2);
    mn=zeros(sum(ndMN)+1,2);
    mn(1,:)=MN(1,:);
    i0=1;
    for i=1:size(dMN,1)
        mn(i0+(1:ndMN(i)),:)=MN(repmat(i,1,ndMN(i)),:)+(1:ndMN(i))'*sign(dMN(i,:));
        i0=i0+ndMN(i);
    end
    %
    % Need to shift from surrounding water level points to corner points
    %
    % rnm(dmn*[1;3]+5) from mn         to mn
    %                  ^  3  2  1      ^  7  6  5
    %                  N  4 mn  8      N  8 mn  4
    %                     5  6  7         1  2  3
    %                         M->             M->
    %
    dmn=diff(mn,1,1);
    dmn=dmn([end 1:end 1],:);
    rnm=[1 2 3 8 0 4 7 6 5]';
    d=rnm(dmn*[1;3]+5);
    x=1e-10;
    %
    %   from:
    %   TR T  TL L  BL B  BR R  to:
    NP=[2  2  x  x  x  x  1  1  % BL
        1  1  2  2  x  x  x  1  % B
        1  1  2  2  x  x  x  x  % BR
        x  1  1  1  2  2  x  x  % R
        x  x  1  1  2  2  x  x  % TR
        x  x  x  1  1  1  2  2  % T
        x  x  x  x  1  1  2  2  % TL
        2  2  x  x  x  1  1  1  % L
        ];
    n_mn1=sum(NP(sub2ind([8 8],d(2:end),d(1:end-1))));
    mn1 = zeros(n_mn1,2);
    j=1;
    for i=1:size(d,1)-1
        switch 10*d(i)+d(i+1)
            case 0
                %--------------------------
                % For each 'from' direction: cycle through 'to' directions
                % from most right (biggest internal) to most left (smallest
                % internal). The first internal point is not recorded as it
                % is recorded as the last point of the previous segment.
                %--------------------------
            case 18
                % o  v  /
                % <  +
                % i*    i*
                mn1(j,:)=mn(i,:)-[0 1];
                j=j+1;
                mn1(j,:)=mn(i,:)-[1 1];
            case 11
                % o  v  /
                % <  +
                % /*    i*
                mn1(j,:)=mn(i,:)-[0 1];
                j=j+1;
                mn1(j,:)=mn(i,:)-[1 1];
            case 12
                % o  v  /
                %    +
                % o  v  i*
                mn1(j,:)=mn(i,:)-[0 1];
            case 13
                % o  v  /
                %    +
                % o  v  \*
                mn1(j,:)=mn(i,:)-[0 1];
                %--------------------------
            case 28
                % o  v  i
                % <  +
                % i*    i*
                mn1(j,:)=mn(i,:)-[0 1];
                j=j+1;
                mn1(j,:)=mn(i,:)-[1 1];
            case 21
                % o  v  i
                % <  +
                % /*    i*
                mn1(j,:)=mn(i,:)-[0 1];
                j=j+1;
                mn1(j,:)=mn(i,:)-[1 1];
            case 22
                % o  v  i
                %    +
                % o  v  i*
                mn1(j,:)=mn(i,:)-[0 1];
            case 23
                % o  v  i
                %    +
                % o     \*
                mn1(j,:)=mn(i,:)-[0 1];
            case 24
                % o  v  i*
                %    +  >
                % o     o
                mn1(j,:)=mn(i,:);
                %--------------------------
            case 32
                % \     i*
                % >  +
                % o  v  i*
                mn1(j,:)=mn(i,:);
                j=j+1;
                mn1(j,:)=mn(i,:)-[0 1];
            case 33
                % \     i*
                % >  +
                % o  v  \*
                mn1(j,:)=mn(i,:);
                j=j+1;
                mn1(j,:)=mn(i,:)-[0 1];
            case 34
                % \     i*
                % >  +  >
                % o     o
                mn1(j,:)=mn(i,:);
            case 35
                % \    /*
                % >  +  >
                % o     o
                mn1(j,:)=mn(i,:);
                %--------------------------
            case 42
                % i     i*
                % >  +
                % o  v  i*
                mn1(j,:)=mn(i,:);
                j=j+1;
                mn1(j,:)=mn(i,:)-[0 1];
            case 43
                % i     i*
                % >  +
                % o  v  \*
                mn1(j,:)=mn(i,:);
                j=j+1;
                mn1(j,:)=mn(i,:)-[0 1];
            case 44
                % i     i*
                % >  +  >
                % o     o
                mn1(j,:)=mn(i,:);
            case 45
                % i     /*
                % >  +  >
                % o     o
                mn1(j,:)=mn(i,:);
            case 46
                % i* ^  o
                % >  +
                % o     o
                mn1(j,:)=mn(i,:)-[1 0];
                %--------------------------
            case 54
                % i*    i*
                %    +  >
                % /  ^  o
                mn1(j,:)=mn(i,:)-[1 0];
                j=j+1;
                mn1(j,:)=mn(i,:);
            case 55
                % i*    /*
                %    +  >
                % /  ^  o
                mn1(j,:)=mn(i,:)-[1 0];
                j=j+1;
                mn1(j,:)=mn(i,:);
            case 56
                % i* ^  o
                %    +
                % /  ^  o
                mn1(j,:)=mn(i,:)-[1 0];
            case 57
                % \* ^  o
                %    +
                % /  ^  o
                mn1(j,:)=mn(i,:)-[1 0];
                %--------------------------
            case 64
                % i*    i*
                %    +  >
                % i  ^  o
                mn1(j,:)=mn(i,:)-[1 0];
                j=j+1;
                mn1(j,:)=mn(i,:);
            case 65
                % i*    /*
                %    +
                % i  ^  o
                mn1(j,:)=mn(i,:)-[1 0];
                j=j+1;
                mn1(j,:)=mn(i,:);
            case 66
                % i* ^  o
                %    +
                % i  ^  o
                mn1(j,:)=mn(i,:)-[1 0];
            case 67
                % \* ^  o
                %    +
                % i  ^  o
                mn1(j,:)=mn(i,:)-[1 0];
            case 68
                % o     o
                % <  +
                % i* ^  o
                mn1(j,:)=mn(i,:)-[1 1];
                %--------------------------
            case 76
                % i* ^  o
                %    +  <
                % i*    \
                mn1(j,:)=mn(i,:)-[1 1];
                j=j+1;
                mn1(j,:)=mn(i,:)-[1 0];
            case 77
                % \* ^  o
                %    +  <
                % i*    \
                mn1(j,:)=mn(i,:)-[1 1];
                j=j+1;
                mn1(j,:)=mn(i,:)-[1 0];
            case 78
                % o     o
                % <  +  <
                % i*    \
                mn1(j,:)=mn(i,:)-[1 1];
            case 71
                % o     o
                % <  +  <
                % /*    \
                mn1(j,:)=mn(i,:)-[1 1];
                %--------------------------
            case 86
                % i* ^  o
                %    +  <
                % i*    i
                mn1(j,:)=mn(i,:)-[1 1];
                j=j+1;
                mn1(j,:)=mn(i,:)-[1 0];
            case 87
                % \* ^  o
                %    +  <
                % i*    i
                mn1(j,:)=mn(i,:)-[1 1];
                j=j+1;
                mn1(j,:)=mn(i,:)-[1 0];
            case 88
                % o     o
                % <  +  <
                % i*    i
                mn1(j,:)=mn(i,:)-[1 1];
            case 81
                % o     o
                % <  +  <
                % /*    i
                mn1(j,:)=mn(i,:)-[1 1];
            case 82
                % o     o
                %    +  <
                % o  v  i*
                mn1(j,:)=mn(i,:)-[0 1];
                %--------------------------
            case {15, 26, 37, 48, 51, 62, 73, 84}
                % enclosure would turn back on itself
                error('Shouldn''t come here: enclosure turns back on itself.')
            otherwise
                % enclosure with dangling grid point
                error('Shouldn''t come here: enclosure with dangling grid point.')
        end
        j=j+1;
    end
    %
    % Remove double points
    %
    mn1(all(diff(mn1,1,1)==0,2),:)=[];
    %
    % Convert enclosure to X,Y coordinates
    %
    ind=sub2ind(size(X),mn1(:,1),mn1(:,2));
    XC=X(ind); YC=Y(ind);
    XY{end+1,1}=[XC(:) YC(:)];
end
XY=cat(1,XY{:});


function Local_encwrite(filename,MN,waqopt)
% * ENCLOSURE('write',FileName,MN)
%   % ...,'waqua') for waqua file format

if size(MN,1)>2
    MN=transpose(MN);
end

fid=fopen(filename,'w','n','US-ASCII');
if fid<0
    error('Error opening output file.')
end
if nargin>2 % waqua format
    fprintf(fid,'e=\n');
end
fprintf(fid,'%5i%5i\n',MN);
fclose(fid);

OK=1;
