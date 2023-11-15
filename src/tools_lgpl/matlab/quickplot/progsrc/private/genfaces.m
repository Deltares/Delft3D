function hNew=genfaces(hOld,Ops,Parent,Val,X,Y,Z)
%GENFACES Generic plot routine for patches plot.

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

special_edges = 0;
if nargin==4
    if isempty(hOld) || ~ishandle(hOld)
        error('Invalid handle')
    else
        hNew=hOld;
        set(hNew,'facevertexcdata',Val(:))
    end
else
    %
    if nargin>6
        %
        % ---> X, Y and Z
        %
        faces=reshape(1:numel(Z),size(Z));
        if isequal(size(Z),size(Val)+1)
            faces=faces(1:end-1,1:end-1);
            faces=faces(:);
            xv=[X(:) Y(:) Z(:)];
            fv=[faces faces+1 faces+size(X,1)+1 faces+size(X,1)];
        else
            faces=faces(:,1:end-1);
            faces=faces(:);
            X0=X(1:end-1,:);
            Y0=Y(1:end-1,:);
            X1=X(2:end,:);
            Y1=Y(2:end,:);
            xv=[X0(:) Y0(:) Z(:); X1(:) Y1(:) Z(:)];
            fv=[faces faces+numel(X0) faces+size(X0,1)+numel(X0) faces+size(X0,1)];
        end
    elseif ndims(X)==4
        %
        % ---> X=XYZ and Y=TRI
        %
        if length(Val) == size(Y,1)
            % one data value provided per triangular patch
            xv = squeeze(X(1,:,1,:));
            fv = Y;
        else
            % assume one data value per node
            special_edges = 1;
            ncoord = size(X,4);
            nnodes = size(X,2);
            nfaces = size(Y,1);
            nedges = size(Y,2);
            xv = zeros(nnodes+(nedges+1)*nfaces,ncoord);
            xv(1:nnodes,:) = squeeze(X(1,:,1,:));
            for i = 1:ncoord
                x = squeeze(X(1,:,1,i));
                xv(nnodes+1:nnodes+nfaces,i) = mean(x(Y),2);
                for e = 1:nedges
                    xv(nnodes+e*nfaces+1:nnodes+(e+1)*nfaces,i) = mean(x(Y(:,[e mod(e,nedges)+1])),2);
                end
            end
            fv = zeros(nedges*nfaces,4);
            for e = 1:nedges
                em = e-1;
                if em==0
                    em = nedges;
                end
                fv(nfaces*(e-1)+1:nfaces*e,:) = [Y(:,e) transpose(nnodes+e*nfaces+1:nnodes+(e+1)*nfaces) transpose(nnodes+1:nnodes+nfaces) transpose(nnodes+em*nfaces+1:nnodes+(em+1)*nfaces)];
            end
            Val = Val(Y);
            Val = Val(:);
        end
    else
        %
        % ---> X and Y
        %
        nX = size(X,1);
        nZ = size(Y,2);
        if isequal(size(Val),[nX-1 nZ-1]) && isequal(size(Y),[nX nZ])
            % Val at centre, X and Y defined at corners
            faces=reshape(1:numel(Y),size(Y));
            faces=faces(1:end-1,1:end-1);
            faces=faces(:);
            xv=[X(:) Y(:)];
            fv=[faces faces+1 faces+size(X,1)+1 faces+size(X,1)];
        elseif isequal(size(Val),[nX-1 nZ-1]) && isequal(size(Y),[nX-1 nZ])
            faces=reshape(1:numel(Y),size(Y));
            faces=faces(:,1:end-1);
            faces=faces(:);
            X0=X(1:end-1,:);
            X1=X(2:end,:);
            xv=[X0(:) Y(:); X1(:) Y(:)];
            fv=[faces faces+numel(X0) faces+size(X0,1)+numel(X0) faces+size(X0,1)];
        elseif isequal(size(Val),[nX-1 nZ]) && isequal(size(Y),[nX-1 nZ])
            Y = (Y(:,[1:end end]) + Y(:,[1 1:end]))/2;
            X(:,end+1) = X(:,end);
            faces=reshape(1:numel(Y),size(Y));
            faces=faces(:,1:end-1);
            faces=faces(:);
            X0=X(1:end-1,:);
            X1=X(2:end,:);
            xv=[X0(:) Y(:); X1(:) Y(:)];
            fv=[faces faces+numel(X0) faces+size(X0,1)+numel(X0) faces+size(X0,1)];
        end
    end
    cv=Val(:);
    %
    % set face values of patches with undefined corners to NaN
    %
    for d = 1:size(xv,2)
        coord = xv(:,d)'; % force coord to be a row vector just in case there is just one face, i.e. fv is a row vector
        if ~isa(cv,'double')
            cv = double(cv);
        end
        cv(any(isnan(coord(fv)),2))=NaN;
    end
    %
    if isfield(Ops,'Thresholds') && ~strcmp(Ops.Thresholds,'none')
        Thresholds = Ops.Thresholds;
    else
        Thresholds = [];
    end
    %
    if any(~ishandle(hOld)) || ~isempty(Thresholds)
        delete(hOld(ishandle(hOld)))
        hOld = [];
    end
    %
    if isempty(hOld)
        if isempty(Ops.colourmap) && ~strcmp(Ops.presentationtype,'grid')
            fv=fv(~isnan(cv),:);
            hNew=patch('vertices',xv, ...
                'faces',fv, ...
                'parent',Parent, ...
                'edgecolor','none', ...
                'facecolor',Ops.colour);
        elseif isempty(Thresholds)
            hNew=patch('vertices',xv, ...
                'faces',fv, ...
                'facevertexcdata',cv, ...
                'parent',Parent, ...
                'edgecolor','none', ...
                'facecolor','flat');
        else
            nThresholds = length(Thresholds);
            Thresholds(end+1) = inf;
            hNew=zeros(1,nThresholds);
            for i = 1:nThresholds
                iclass = cv>=Thresholds(i) & cv<Thresholds(i+1);
                if any(iclass)
                    facecolor = 'flat';
                else
                    facecolor = 'none';
                end
                hNew(i)=patch('vertices',xv, ...
                    'faces',fv(iclass,:), ...
                    'facevertexcdata',0*cv(iclass)+i, ...
                    'parent',Parent, ...
                    'edgecolor','none', ...
                    'facecolor',facecolor);
            end
        end
        if strcmp(Ops.presentationtype,'patches with lines') || strcmp(Ops.presentationtype,'grid')
            if special_edges
                ln = fv(:,[2 3 3])';
                %
                ln2 = zeros(nedges*nfaces,2);
                for e = 1:nedges
                    ln2(nfaces*(e-1)+1:nfaces*e,:) = Y(:,[e mod(e,nedges)+1]);
                end
                ln2 = sortrows(sort(ln2,2));
                % remove duplicate, i.e. inner, edges
                d = any(diff([-ln2(1,:);ln2]),2);
                d(d(2:end)==0)=0;
                ln2 = ln2(d,[1 2 2])';
                %
                ln = [ln(:);ln2(:)];
                x = xv(ln,1);
                y = xv(ln,2);
                x(3:3:end) = NaN;
                y(3:3:end) = NaN;
                hNew(end+1) = line(x,y,'color',Ops.colour,'parent',Parent);
            else
                set(hNew,'edgecolor',Ops.colour)
            end
        end
    else
        hNew=hOld;
        if isempty(Ops.colourmap) && ~strcmp(Ops.presentationtype,'grid')
            fv=fv(~isnan(cv) & cv~=0,:);
            set(hNew,'vertices',xv, ...
                'faces',fv)
        else
            set(hNew,'vertices',xv, ...
                'faces',fv, ...
                'facevertexcdata',cv)
        end
    end
end
