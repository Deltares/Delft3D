function [hNew,Thresholds,Param]=qp_plot_seg(hNew,Parent,Param,data,Ops,Props)
%QP_PLOT_SEG Plot function of QuickPlot for 1D line segment data sets.

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

T_=1; ST_=2; M_=3; N_=4; K_=5;

FirstFrame=Param.FirstFrame;
Quant=Param.Quant;
Units=Param.Units;
if ~isempty(Units)
    PName=sprintf('%s (%s)',Quant,Units);
else
    PName=Quant;
end
TStr=Param.TStr;
Selected=Param.Selected;
multiple=Param.multiple;
NVal=Param.NVal;

DimFlag=Props.DimFlag;
Thresholds=[];

if strcmp(Ops.presentationtype,'values')
    NVal = 4;
end
switch NVal
    case 0
        if strcmp(Ops.presentationtype,'edges') || (multiple(M_) && ~strcmp(Ops.presentationtype,'markers')) % network
            if ishandle(hNew)
                set(hNew,'vertices',data.XY,'faces',data.SEG(:,[1 2 2]))
            else
                hNew=patch('vertices',data.XY,'faces',data.SEG(:,[1 2 2]), ...
                    'parent',Parent, ...
                    'edgecolor',Ops.colour, ...
                    'linewidth',Ops.linewidth, ...
                    'linestyle',Ops.linestyle, ...
                    'marker',Ops.marker, ...
                    'markersize',Ops.markersize, ...
                    'markeredgecolor',Ops.markercolour, ...
                    'markerfacecolor',Ops.markerfillcolour);
            end
        else % point
            if strcmp(data.ValLocation,'EDGE')
                % compute edge centers
                data.XY = sum(reshape(data.XY(data.SEG,:),[size(data.SEG,1) 2 2]),2)/2;
            end
            if ishandle(hNew)
                set(hNew,'xdata',data.XY(:,1),'ydata',data.XY(:,2))
            else
                hNew=line(data.XY(:,1),data.XY(:,2), ...
                    'parent',Parent, ...
                    Ops.LineParams{:});
            end
        end
    case {1,5,6}
        switch Ops.presentationtype
            case 'markers'
                if strcmp(data.ValLocation,'EDGE')
                    % compute edge centers
                    data.XY = reshape(sum(reshape(data.XY(data.SEG,:),[size(data.SEG,1) 2 2]),2)/2,[size(data.SEG,1) 2]);
                end
                if ishandle(hNew)
                    set(hNew,'vertices',data.XY,'faces',(1:size(data.XY,1))', ...
                        'facevertexcdata',data.Val(:))
                else
                    hNew=patch('vertices',data.XY,'faces',(1:size(data.XY,1))', ...
                        'facevertexcdata',data.Val(:), ...
                        'parent',Parent, ...
                        'marker',Ops.marker, ...
                        'markersize',Ops.markersize, ...
                        'markeredgecolor',Ops.markercolour, ...
                        'markerfacecolor',Ops.markerfillcolour, ...
                        'linestyle','none', ...
                        'edgecolor','flat', ...
                        'facecolor','none');
                end
            case 'edges'
                XY=data.XY(data.SEG(:),:);
                SEG=data.SEG; SEG(:)=1:2*size(SEG,1);
                Val=cat(1,data.Val(:),data.Val(:));
                if ishandle(hNew)
                    set(hNew,'vertices',XY,'faces',SEG, ...
                        'facevertexcdata',Val)
                else
                    hNew=patch('vertices',XY, ...
                        'faces',SEG, ...
                        'facevertexcdata',Val, ...
                        'parent',Parent, ...
                        'edgecolor','flat', ...
                        'linewidth',Ops.linewidth, ...
                        'linestyle',Ops.linestyle, ...
                        'marker',Ops.marker, ...
                        'markersize',Ops.markersize, ...
                        'markeredgecolor',Ops.markercolour, ...
                        'markerfacecolor',Ops.markerfillcolour);
                end
            case 'continuous shades'
                if ishandle(hNew)
                    if isfield(data,'SEG') && ~isempty(data.SEG)
                        set(hNew,'vertices',data.XY,'faces',data.SEG, ...
                            'facevertexcdata',data.Val(:))
                    else
                        set(hNew,'vertices',data.XY,'faces',(1:size(data.XY,1))', ...
                            'facevertexcdata',data.Val(:))
                    end
                else
                    if isfield(data,'SEG') && ~isempty(data.SEG) && isfield(Ops,'linestyle')
                        hNew=patch('vertices',data.XY,'faces',data.SEG, ...
                            'facevertexcdata',data.Val(:), ...
                            'parent',Parent, ...
                            'edgecolor','interp', ...
                            'linewidth',Ops.linewidth, ...
                            'linestyle',Ops.linestyle, ...
                            'marker',Ops.marker, ...
                            'markersize',Ops.markersize, ...
                            'markeredgecolor',Ops.markercolour, ...
                            'markerfacecolor',Ops.markerfillcolour);
                    else
                        hNew=patch('vertices',data.XY,'faces',(1:size(data.XY,1))', ...
                            'facevertexcdata',data.Val(:), ...
                            'parent',Parent, ...
                            'marker',Ops.marker, ...
                            'markersize',Ops.markersize, ...
                            'markeredgecolor',Ops.markercolour, ...
                            'markerfacecolor',Ops.markerfillcolour, ...
                            'linestyle','none', ...
                            'edgecolor','flat', ...
                            'facecolor','none');
                    end
                end
            otherwise
                if multiple(T_)
                    if ishandle(hNew)
                        set(hNew,'xdata',data.Time(:),'ydata',data.Val(:));
                    else
                        hNew=line(data.Time(:),data.Val(:,1), ...
                            'parent',Parent, ...
                            Ops.LineParams{:});
                    end
                end
        end
    case {2,3}
        if multiple(M_) % network
        else % point
        end
    case 4
        if strcmp(data.ValLocation,'EDGE')
            % compute edge centers
            data.XY = sum(reshape(data.XY(data.SEG,:),[size(data.SEG,1) 2 2]),2)/2;
        end
        if isnumeric(data.Val)
            Remove = isnan(data.Val);
            if any(Remove)
                data.Val(Remove) = [];
                data.XY(Remove,:) = [];
            end
        end
        hNew=gentextfld(hNew,Ops,Parent,data.Val,data.XY(:,1),data.XY(:,2));
end
if strcmp(Ops.colourbar,'none')
    qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
else
    qp_title(Parent,{TStr},'quantity',Quant,'unit',Units,'time',TStr)
end


