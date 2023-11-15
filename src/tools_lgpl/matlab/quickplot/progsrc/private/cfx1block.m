function Data=cfx1block(dmp,minfo,var,varargin)
%CFX1BLOCK  Merge CFX 4 multiblock data into one block
%     MInfo=cfx1block(DMP)
%     Data =cfx1block(DMP,MInfo,Variable,DmType,i,PH,T)
%                                ^ see cfx command for explanation of the
%                                arguments.

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

if nargin==1
    % dmp=cfx('open','u:\ow\Q2933\4-drempel8\37.5cm\m01.dmp');
    cb.Block=1;
    cb.Idx={1:dmp.Block(1).I-2 1:dmp.Block(1).J-2 1:dmp.Block(1).K-2};
    cb.Orientation=[1 2 3];
    cb.Size=[dmp.Block(1).I dmp.Block(1).J dmp.Block(1).K]-2;
    cb.ReorderedSize=cb.Size;
    cb.CPoint=[1 1 1];
    if isfield(dmp,'Glue')
        gp=1:length(dmp.Glue);
    else
        gp=[];
    end
    mini=1; minj=1; mink=1; maxi=dmp.Block(1).I-2; maxj=dmp.Block(1).J-2; maxk=dmp.Block(1).K-2;
    Blk=1;
    noerror=1;
    DirStr='IJK';
    while ~isempty(gp) & noerror
        doneglueing=zeros(1,length(gp));
        for j=1:length(gp)
            g=gp(j);
            %
            p1=dmp.Glue(g).Patch1;
            p2=dmp.Glue(g).Patch2;
            DirChange=dmp.Glue(g).DirChange;
            DirChange(DirChange>3) = -(DirChange(DirChange>3)-3);
            %
            b1=dmp.Patch(p1).Block;
            b2=dmp.Patch(p2).Block;
            %
            newglue=1;
            if ismember(b1,Blk)
                if ismember(b2,Blk)
                    % check glue
                    newglue=0;
                end
            elseif ismember(b2,Blk)
                if ismember(b1,Blk)
                    % check glue
                    newglue=0;
                end
                p_ = p2; p2 = p1; p1 = p_;
                b_ = b2; b2 = b1; b1 = b_;
                [dummy,aDirChange]=sort(abs(DirChange));
                iDC=abs(DirChange(DirChange<0));
                aDirChange(iDC)=-aDirChange(iDC);
                DirChange=aDirChange;
            end
            if dmp.Patch(p1).IJK~=dmp.Patch(p2).IJK
                noerror=0;
                break
            end
            %
            if newglue
                % glue
                ib1=find(Blk==b1);
                %
                cb(end+1).Block=b2;
                Blk(end+1)=b2;
                if ~isequal(DirChange,[1 2 3]) % transform using Glue.DirChange
                    cb(end).Orientation = DirChange(abs(cb(ib1).Orientation)) ...
                        .* sign(cb(ib1).Orientation);
                else
                    cb(end).Orientation = cb(ib1).Orientation;
                end
                cb(end).Size=[dmp.Block(b2).I dmp.Block(b2).J dmp.Block(b2).K]-2;
                cb(end).ReorderedSize=cb(end).Size(abs(cb(end).Orientation));
                cb(end).CPoint=cb(ib1).CPoint;
                cb(end).Idx=cb(ib1).Idx;
                %
                % Have to do something with the glue patch offsets ...
                %
                Patch_b1=[dmp.Patch(p1).Imin dmp.Patch(p1).Jmin dmp.Patch(p1).Kmin;dmp.Patch(p1).Imax dmp.Patch(p1).Jmax dmp.Patch(p1).Kmax];
                Patch_b1=Patch_b1(:,abs(cb(ib1).Orientation));
                Patch_b2=[dmp.Patch(p2).Imin dmp.Patch(p2).Jmin dmp.Patch(p2).Kmin;dmp.Patch(p2).Imax dmp.Patch(p2).Jmax dmp.Patch(p2).Kmax];
                Patch_b2=Patch_b2(:,abs(cb(end).Orientation));
                %
                maxside = 0;
                drc1=dmp.Patch(p1).Direc;
                if drc1>3
                    drc1 = drc1-3;
                    maxside = 1;
                end
                drc1_reordered = find(abs(cb(ib1).Orientation)==drc1);
                %
                drc2=dmp.Patch(p2).Direc;
                if drc2>3, drc2=drc2-3; end
                I=getfield(dmp.Block(b2),DirStr(drc2));
                %drc2_reordered = abs(cb(end).Orientation)==drc2;
                %
                di=drc1_reordered;
                if maxside
                    cb(end).Idx{di}=max(cb(end).Idx{di})+(1:I-2);
                    cb(end).CPoint(di)=cb(end).CPoint(di)+cb(ib1).ReorderedSize(di);
                else
                    cb(end).Idx{di}=min(cb(end).Idx{di})+(-I+2:-1);
                    cb(end).CPoint(di)=cb(end).CPoint(di)-I+2;
                end
                doneglueing(j)=1;
            end
        end
        if ~any(doneglueing)
            break
        else
            gp=gp(~doneglueing);
        end
    end
    if ~noerror
        return
    end
    for b=2:length(cb)
        mini=min(mini,min(cb(b).Idx{1}));
        minj=min(minj,min(cb(b).Idx{2}));
        mink=min(mink,min(cb(b).Idx{3}));
        maxi=max(maxi,max(cb(b).Idx{1}));
        maxj=max(maxj,max(cb(b).Idx{2}));
        maxk=max(maxk,max(cb(b).Idx{3}));
    end
    for b=1:length(cb)
        cb(b).Idx{1}=cb(b).Idx{1}-mini+1;
        cb(b).Idx{2}=cb(b).Idx{2}-minj+1;
        cb(b).Idx{3}=cb(b).Idx{3}-mink+1;
        cb(b).CPoint(1)=cb(b).CPoint(1)-mini+1;
        cb(b).CPoint(2)=cb(b).CPoint(2)-minj+1;
        cb(b).CPoint(3)=cb(b).CPoint(3)-mink+1;
    end
    maxi=maxi-mini+1;
    maxj=maxj-minj+1;
    maxk=maxk-mink+1;
    Data.Blocks=cb;
    Data.IJK=[maxi maxj maxk];
elseif nargin>2
    X=cfx('read',dmp,var,varargin{:});
    cb=minfo.Blocks;
    c={':',':',':'};
    if isequal(prod(size(X{1})),dmp.Block(1).IJK)
        %
        % Values
        %
        Data = repmat(NaN,minfo.IJK);
        for i = 1:length(cb),
            Tmp = X{cb(i).Block}(2:end-1,2:end-1,2:end-1);
            if ~isequal(abs(cb(i).Orientation),[1 2 3])
                Tmp = permute(Tmp,abs(cb(i).Orientation));
            end
            for d = find(cb(i).Orientation<0);
                c_d = c; c_d{d} = size(Tmp,d):-1:1;
                Tmp = Tmp(c_d{:});
            end
            Data(cb(i).Idx{:}) = Tmp;
        end
    else
        %
        % Coordinates
        %
        Data = repmat(NaN,minfo.IJK+1);
        for i = 1:length(cb),
            ii = cb(i).Idx{1}; ii(end+1) = ii(end)+1;
            jj = cb(i).Idx{2}; jj(end+1) = jj(end)+1;
            kk = cb(i).Idx{3}; kk(end+1) = kk(end)+1;
            Tmp = X{cb(i).Block}(2:end-1,2:end-1,2:end-1);
            if ~isequal(abs(cb(i).Orientation),[1 2 3])
                Tmp = permute(Tmp,abs(cb(i).Orientation));
            end
            for d = find(cb(i).Orientation<0);
                c_d = c; c_d{d} = size(Tmp,d):-1:1;
                Tmp = Tmp(c_d{:});
            end
            Data(ii,jj,kk) = Tmp;
        end
    end
else
    error('Invalid number of arguments.');
end
