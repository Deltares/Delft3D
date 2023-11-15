function Out=balanceplot(Time,X,varargin)
%BALANCEPLOT Create a balance plot.

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

options.type='bar';
[options,err]=procargs(varargin, ...
    {'Name','HasDefault','Default'}, ...
    {'type',1,'bar'
    'color',1,'none'});
[options,err]=procargs(varargin, ...
    {'Name' ,'HasDefault','Default','List'}, ...
    {'parent',0,[],[]
    'type' ,1,'bar',{'area','bar'}
    'color',1,'none',[]});
if ~isempty(err)
    error(err)
end
hNew=[];
hLine=[];
Time=Time(:);
if length(Time)~=size(X,1)
    error('Length of time vector does not match number of data values per column!')
end
%
MaxPos = zeros(size(Time));
MaxNeg = zeros(size(Time));
%
Index  = 2:size(X,1);
IndexT = [Index-1;Index;Index;Index-1];
t = Time(IndexT);
tt = t;
%
switch options.type
    case 'bar'
        Index2 = [Index-1;Index];
        Index2 = Index2(:);
        t2 = Time(Index2);
        Index2 = [Index;Index];
        Index2 = Index2(:);
end
%
for i=1:size(X,2)
    xx = X(:,i);
    x0 = MaxPos;
    x0(xx<0) = MaxNeg(xx<0);
    switch options.type
        case 'area'
            x = [x0(Index-1) x0(Index) x0(Index)+xx(Index) x0(Index-1)+xx(Index-1)]';
            tt = t;
            signchange = find(x(3,:).*x(4,:)<=0);
            if length(signchange)>0
                tt = [t t(:,signchange)];
                x0 = x(:,signchange);
                %
                x1 = x0;
                x1(1,:) = MaxPos(signchange);
                x1(4,:) = MaxPos(signchange);
                x1(1,x1(3,:)<0) = MaxNeg(signchange(x1(3,:)<0));
                x1(4,x1(3,:)<0) = MaxNeg(signchange(x1(3,:)<0));
                %
                x2 = x0;
                x2(2,:) = MaxPos(signchange+1);
                x2(3,:) = MaxPos(signchange+1);
                x2(2,x2(4,:)<0) = MaxNeg(signchange(x2(4,:)<0)+1);
                x2(3,x2(4,:)<0) = MaxNeg(signchange(x2(4,:)<0)+1);
                %
                x(:,signchange) = x1;
                x = [x x2];
            end
        case 'bar'
            x = [x0 x0 x0+xx x0+xx]';
            x = x(:,Index);
    end
    %
    % Two approaches:
    %   (1) Colour edge with same colour as face -> less tiny gaps visible
    %       during rotation, but the lines are not visible -> the objects
    %       should be reordered such that the lines will be plotted on top.
    %hNew(end+1,1)=patch(tt,x,repmat(i,size(x)),'edgecolor','flat','parent',options.parent);
    %
    %   (2) Do not draw the edges -> more tiny gaps visible during rotation,
    %       but the lines are visible
    hNew(end+1,1)=patch(tt,x,i,'edgecolor','none','parent',options.parent);
    %
    MaxNeg=MaxNeg+min(xx,0);
    MaxPos=MaxPos+max(xx,0);
    if ~isequal(options.color,'none')
        switch options.type
            case 'area'
                hLine(end+1,1)=line(Time,MaxPos,'color',options.color,'parent',options.parent);
                hLine(end+1,1)=line(Time,MaxNeg,'color',options.color,'parent',options.parent);
            case 'bar'
                hLine(end+1,1)=line(t2,MaxPos(Index2),'color',options.color,'parent',options.parent);
                hLine(end+1,1)=line(t2,MaxNeg(Index2),'color',options.color,'parent',options.parent);
        end
    end
end
hNew(end+1,1)=line(Time,Time*0,'color','k');
hNew = [hNew;hLine];
if nargout>0
    Out=flipud(hNew);
end
