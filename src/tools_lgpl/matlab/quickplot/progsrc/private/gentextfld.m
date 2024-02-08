function hNew=gentextfld(hOld,Ops,Parent,Val,X,Y,Z)
%GENTEXTFLD Generic plot routine for a text field.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2024 Stichting Deltares.                                     
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

delete(hOld);
zcoord=nargin>6;
if zcoord
    blank=isnan(X(:))|isnan(Y(:))|isnan(Z(:));
    Z=Z(~blank); Z=Z(:);
else
    blank=isnan(X(:))|isnan(Y(:));
end
X=X(~blank); X=X(:);
Y=Y(~blank); Y=Y(:);
Val=Val(~blank); Val=Val(:);
%
if isempty(X)
    X=NaN;
    Y=NaN;
    if zcoord
        Z=NaN;
    end
end
%
% axes limits don't react to text objects, add a line to influence the axes
if zcoord
    hLine = line([min(X) max(X)],[min(Y) max(Y)],[min(Z) max(Z)],'linestyle','none','marker','none','parent',Parent);
else
    hLine = line([min(X) max(X)],[min(Y) max(Y)],'linestyle','none','marker','none','parent',Parent);
end
if isfield(Ops,'thinningmode') && strcmp(Ops.thinningmode,'dynamic')
    xDummy = zeros(Ops.thinningcount,1);
else
    xDummy = zeros(length(X),1);
end
% generate text objects
hText = text(xDummy,xDummy,'','parent',Parent,'clipping','on',Ops.FontParams{:});
hNew = cat(2,hLine,hText');
% fill text objects
if zcoord
    args = {hNew,[X,Y,Z],Val,Ops};
else
    args = {hNew,[X,Y],Val,Ops};
end
setappdata(hLine,'axesRefresh',@gentextfld_update)
setappdata(hLine,'axesRefreshArguments',args)
gentextfld_update(args{:})


function gentextfld_update(hNew,XYZ,Val,Ops)
if iscellstr(Val)
    convert = 0;
elseif ischar(Val)
    convert = 1;
elseif isfield(Ops,'numformat')
    if iscell(Val)
        convert = 2;
    else
        convert = 3;
    end
else
    if iscell(Val)
        convert = 4;
    else
        convert = 5;
    end
end

N = length(Val);
if isfield(Ops,'thinningmode') && strcmp(Ops.thinningmode,'dynamic')
    K = Ops.thinningcount;
    
    % first clip to xlim/ylim
    ax = get(hNew(1),'parent');
    xlim = get(ax,'xlim');
    ylim = get(ax,'ylim');
    I = XYZ(:,1) > xlim(1) & XYZ(:,1) < xlim(2) & XYZ(:,2) > ylim(1) & XYZ(:,2) < ylim(2);
    XYZ = XYZ(I,:);
    Val = Val(I);
    N = length(Val);
    
    % if numeric values then we can do some smart stuff such as make sure
    % to include the minimum and maximum values.
    if isnumeric(Val)
        [Val,sorted] = sort(Val);
        XYZ = XYZ(sorted,:);
        
        if N < K
            % keep all
        else
            if N > 50000
                % for performance reasons start with a random subset
                I = sort(randperm(N,50000));
                % but always include the minimum and maximum values
                I(1) = 1;
                I(end) = N;
                XYZ = XYZ(I,:);
                Val = Val(I);
                N = 50000;
            end
            
            f = @(i) (XYZ(:,1)-XYZ(i,1)).^2 + (XYZ(:,2)-XYZ(i,2)).^2;
            I = zeros(1,K);
            I(1) = 1; % minimum value
            dist = f(1);
            I(2) = N; % maximum value
            dist = min(dist, f(N));
            for i = 3:K
                [~,k] = max(dist);
                I(i) = k;
                dist = min(dist, f(k));
            end
            I = sort(I);
            
            XYZ = XYZ(I,:);
            Val = Val(I);
        end
    end
    N = length(Val);
end

ncrd = size(XYZ,2);
for i = 1:N
    switch convert
        case 0
            Str = Val{i};
        case 1 % char
            Str = Val(i);
        case 2
            Str = sprintf(Ops.numformat,Val{i});
        case 3
            Str = sprintf(Ops.numformat,Val(i));
        case 4
            Str = var2str(Val{i});
        case 5
            Str = var2str(Val(i));
    end
    Str = protectstring(Str);

    % we need to preserve the Z coordinate in a 2D plot ...
    xyz  = get(hNew(1+i),'position');
    xyz(1:ncrd) = XYZ(i,:);

    set(hNew(1+i),'position',xyz,'string',Str,'visible','on')
    % possible future extension ... highlight the min/max values somehow ... by colour, fontsize, weight, box ... ,'color','k')
end
set(hNew(1+N+1:end),'visible','off')
% set(hNew(1+[1,N]),'color','r') % min/max
