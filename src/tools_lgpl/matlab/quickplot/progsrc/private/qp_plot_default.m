function [hNew,Thresholds,Param,Parent]=qp_plot_default(hNew,Parent,Param,data,Ops,Props)
%QP_PLOT_DEFAULT Plot function of QuickPlot for structured data sets.

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
quivopt=Param.quivopt;
stats=Param.stats;
stn=Param.stn;
s=Param.s;
compat7=Param.compat7;

DimFlag=Props.DimFlag;
Thresholds=Ops.Thresholds;
axestype=Ops.basicaxestype;

switch NVal
    
    case {0,0.5}
        switch axestype
            case {'X-Y','Lon-Lat','X-Y-Val','Lon-Lat-Val'}
                
                if isfield(data,'TRI')
                    if FirstFrame
                        if isempty(data.TRI)
                            data.TRI = 1:size(data.XYZ,2);
                            if length(data.TRI)<3
                                data.TRI(length(data.TRI)+1:3) = 1;
                            end
                            Ops.linestyle = 'none';
                        end
                        hNew=patch('vertices',reshape(data.XYZ(1,:,1,:),[size(data.XYZ,2) size(data.XYZ,4)]),'faces',data.TRI, ...
                            'facecolor','none','edgecolor',Ops.colour, ...
                            'linewidth',Ops.linewidth, ...
                            'linestyle',Ops.linestyle, ...
                            'marker',Ops.marker, ...
                            'markersize',Ops.markersize, ...
                            'markeredgecolor',Ops.markercolour, ...
                            'markerfacecolor',Ops.markerfillcolour, ...
                            'parent',Parent);
                    else
                        set(hNew,'vertices',data.XYZ,'faces',data.TRI);
                    end
                elseif isfield(data,'XDam')
                    if ~FirstFrame
                        delete(hNew)
                    end
                    if strcmp(Ops.presentationtype,'edge m')
                        data.XDamVal(:) = NaN;
                    elseif strcmp(Ops.presentationtype,'edge n')
                        data.YDamVal(:) = NaN;
                    end
                    if strcmp(Ops.presentationtype,'values')
                        xx = (data.X(:,1:end-1) + data.X(:,2:end))/2;
                        yy = (data.Y(:,1:end-1) + data.Y(:,2:end))/2;
                        vv = data.XDamVal(:,2:end);
                        hNew = qp_scalarfield(Parent,[],Ops.presentationtype,'QUAD',xx,yy,[],vv,Ops);
                        %
                        xx = (data.X(1:end-1,:) + data.X(2:end,:))/2;
                        yy = (data.Y(1:end-1,:) + data.Y(2:end,:))/2;
                        vv = data.YDamVal(2:end,:);
                        hNew2 = qp_scalarfield(Parent,[],Ops.presentationtype,'QUAD',xx,yy,[],vv,Ops);
                        %
                        hNew = cat(2,hNew,hNew2);
                    elseif isfield(data,'XDamVal') && Ops.colourdams
                        hNew=thindam(data.X,data.Y,data.XDam,data.YDam,'color',data.XDamVal,data.YDamVal,'parent',Parent);
                        set(hNew,'linewidth',Ops.linewidth, ...
                            'linestyle',Ops.linestyle, ...
                            'edgecolor','flat', ...
                            'marker',Ops.marker, ...
                            'markersize',Ops.markersize, ...
                            'markeredgecolor',Ops.markercolour, ...
                            'markerfacecolor',Ops.markerfillcolour);
                    else
                        hNew=thindam(data.X,data.Y,data.XDam,data.YDam,'parent',Parent);
                        set(hNew,Ops.LineParams{:});
                    end
                elseif isfield(data,'X') && sum(size(data.X)>1)>=2
                    if ndims(data.X)>2
                        data.X = data.X(:,:,1);
                        data.Y = data.Y(:,:,1);
                    end
                    if FirstFrame
                        hNew=surface(data.X,data.Y,zeros(size(data.X)), ...
                            'cdata',[], ...
                            'parent',Parent, ...
                            'edgecolor',Ops.colour, ...
                            'linewidth',Ops.linewidth, ...
                            'linestyle',Ops.linestyle, ...
                            'marker',Ops.marker, ...
                            'markersize',Ops.markersize, ...
                            'markeredgecolor',Ops.markercolour, ...
                            'markerfacecolor',Ops.markerfillcolour, ...
                            'facecolor','none');
                    else
                        set(hNew,'xdata',data.X,'ydata',data.Y);
                    end
                else
                    if isfield(data,'X')
                        X = data.X;
                        Y = data.Y;
                    elseif isfield(data,'XY')
                        X = data.XY(:,1);
                        Y = data.XY(:,2);
                    end
                    if FirstFrame
                        hNew=line(X,Y, ...
                            'parent',Parent, ...
                            Ops.LineParams{:});
                    else
                        set(hNew,'xdata',X,'ydata',Y);
                    end
                end
                if strcmp(Ops.colourbar,'none')
                    qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
                else
                    qp_title(Parent,{TStr},'quantity',Quant,'unit',Units,'time',TStr)
                end
            case {'X-Val'}
                if 1
                    s = data.X;
                    nX = length(s);
                    x = [s s NaN(nX,1)]';
                    y = repmat([get(Parent,'ylim')';NaN],1,nX);
                    x = x(:);
                    y = y(:);
                else
                    x = data.X;
                    y = zeros(size(data.X));
                end
                if FirstFrame
                    hNew=line(x,y, ...
                        'parent',Parent, ...
                        Ops.LineParams{:});
                else
                    set(hNew,'xdata',x);
                end
                qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
            case {'X-Z'}
                % dummy values
                if ~isequal(size(data.Z),size(data.X))
                    Ops.presentationtype = 'grid';
                    data.Val = repmat(NaN,size(data.Z)-[0 1]);
                else
                    Ops.presentationtype = 'old grid';
                    data.Val = repmat(NaN,size(data.Z));
                end
                hNew = plotslice(hNew,Parent,data,Ops,Props,Thresholds);
                qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
            case {'Val-Z'}
                z=squeeze(data.Z);
                if FirstFrame
                    hNew=line(zeros(size(z)),z, ...
                        'parent',Parent, ...
                        Ops.LineParams{:});
                else
                    set(hNew,'ydata',z);
                end
                qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
            case {'Time-Z'}
                z=squeeze(data.Z);
                t=repmat(data.Time,1,size(z,2));
                if FirstFrame
                    hNew=surface(t,z,zeros(size(z)), ...
                            'cdata',[], ...
                            'parent',Parent, ...
                            'edgecolor',Ops.colour, ...
                            'linewidth',Ops.linewidth, ...
                            'linestyle',Ops.linestyle, ...
                            'marker',Ops.marker, ...
                            'markersize',Ops.markersize, ...
                            'markeredgecolor',Ops.markercolour, ...
                            'markerfacecolor',Ops.markerfillcolour, ...
                            'facecolor','none');
                else
                    set(hNew,'xdata',t,'ydata',z);
                end
                qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
            case 'unknown?'
                if FirstFrame
                    hNew=line(data.X,data.Y, ...
                        'parent',Parent, ...
                        Ops.LineParams{:});
                    if isfield(data,'Z')
                        if length(hNew)==1
                            set(hNew,'zdata',data.Z);
                        else
                            for i=1:size(data.Z,2)
                                set(hNew(i),'zdata',data.Z(:,i));
                            end
                        end
                        %set(get(Parent,'zlabel'),'string','elevation (m) \rightarrow')
                    end
                else
                    if isfield(data,'Z')
                        set(hNew,'xdata',data.X,'ydata',data.Y,'zdata',data.Z);
                    else
                        set(hNew,'xdata',data.X,'ydata',data.Y);
                    end
                end
                if ~isempty(stn)
                    Str={PName,stn};
                else
                    Str=PName;
                end
                qp_title(Parent,Str,'quantity',Quant,'unit',Units)
            otherwise
                hNew=gentext(hNew,Ops,Parent,'Plot not defined');
        end
        
    case {1,5,6}
        switch axestype
            case {'X-Y','Lon-Lat','X-Y-Val','X-Y-Z','Lon-Lat-Val','Lon-Lat-Z'}
                if isfield(data,'TRI')
                    set(Parent,'NextPlot','add');
                    switch Ops.presentationtype
                        case {'values','markers'}
                            if isfield(data,'Z') && 0
                                hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'QUAD',data.X,data.Y,data.Z,data.Val,Ops);
                            else
                                hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'QUAD',data.X,data.Y,[],data.Val,Ops);
                            end
                        otherwise
                            hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'TRI',data.TRI,data.XYZ,data.Val,Ops);
                    end
                    if strcmp(Ops.colourbar,'none')
                        qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
                    else
                        qp_title(Parent,{TStr},'quantity',Quant,'unit',Units,'time',TStr)
                    end
                else
                    data = qp_dimsqueeze(data,Ops.axestype,multiple,DimFlag,Props);
                    if strcmp(Ops.presentationtype,'labels') && isfield(data,'Classes')
                        if isfield(data,'ClassVal')
                            [~, data.Val] = ismember(data.Val, data.ClassVal);
                            miss = data.Val == 0;
                        else
                            miss = isnan(data.Val);
                        end
                        data.Val(miss) = 1;
                        data.Val = data.Classes(data.Val);
                        data.Val(miss) = {''};
                    end
                    if isfield(data,'Z') && 0
                        hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'QUAD',data.X,data.Y,data.Z,data.Val,Ops);
                    elseif isfield(data,'X')
                        hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'QUAD',data.X,data.Y,[],data.Val,Ops);
                    else
                        hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'QUAD',data.XY(:,1),data.XY(:,2),[],data.Val,Ops);
                    end
                    if isempty(Selected{K_})
                        str=PName;
                        lyr={};
                    else
                        lyr = qp_layer(Selected{K_});
                        str = sprintf('%s in %s',PName,lyr);
                        lyr = {lyr};
                    end
                    %
                    if strcmp(Ops.colourbar,'none')
                        tit = {str};
                    else
                        tit = lyr;
                    end
                    if ~isempty(stn)
                        tit{end+1}=stn;
                    end
                    if ~isempty(TStr)
                        tit{end+1}=TStr;
                    end
                    if length(tit)>2
                        tit{1}=[tit{1} ' at ' tit{2}];
                        tit(2)=[];
                    end
                    qp_title(Parent,tit,'quantity',Quant,'unit',Units)
                end
                
            case {'Distance-Val','X-Val','X-Z','X-Time','Time-X','Time-Z','Time-Val'}
                [hNew,Param,Parent] = qp_plot_xzt(hNew,Parent,Param,data,Ops,Props,PName,TStr,stn,Quant,Units);
               
            case 'Val-Z'
                
                if length(data.Time)>1 % Time-Z
                    c2 = squeeze(data.Z);
                    c1 = repmat(data.Time,1,size(c2,2));
                    v = squeeze(data.Val);
                    set(Parent,'NextPlot','add');
                    switch Ops.presentationtype
                        case 'values'
                            I=~isnan(data.Val);
                            hNew=gentextfld(hNew,Ops,Parent,v,c1,c2);
                            
                        case 'continuous shades'
                            hNew=gensurface(hNew,Ops,Parent,v,c1,c2,v);
                            
                        case 'markers'
                            hNew=genmarkers(hNew,Ops,Parent,v,c1,c2);
                            
                        case {'contour lines','coloured contour lines','contour patches','contour patches with lines'}
                            if isequal(size(c1),size(v)+1)
                                [c1,c2,v]=face2surf(c1,c2,v);
                            end
                            v(isnan(c1) | isnan(c2))=NaN;
                            ms=max(c1(:));
                            mz=max(c2(:));
                            c1(isnan(c1))=ms;
                            c2(isnan(c2))=mz;
                            hNew=gencontour(hNew,Ops,Parent,c1,c2,v,Thresholds);
                            
                    end
                    if FirstFrame
                        set(Parent,'view',[0 90],'layer','top');
                    end
                    qp_title(Parent,PName,'quantity',Quant,'unit',Units)
                else
                    if FirstFrame
                        hNew=line(squeeze(data.Val),squeeze(data.Z), ...
                            'parent',Parent, ...
                            Ops.LineParams{:});
                    elseif ishandle(hNew)
                        set(hNew,'xdata',squeeze(data.Val), ...
                            'ydata',squeeze(data.Z));
                    else
                        return
                    end
                    if ~isempty(stn)
                        Str={stn,TStr};
                    else
                        Str={TStr};
                    end
                    qp_title(Parent,Str,'quantity',Quant,'unit',Units,'time',TStr)
                end

            otherwise % Text
                strval = sprintf(Ops.numformat,data.Val);
                hNew=gentext(hNew,Ops,Parent,['Val=',strval]);
                
        end
        
    case {2,3}
        
        switch axestype
            case {'Time-Val','Time-Z'}
                if multiple(T_)
                    
                    if ~isempty(hNew)
                        delete(hNew)
                    end
                    
                    if length(Parent)==1
                        relaxpos = [ ...
                            0         0         0.2754    1.0000
                            0.3623    0.5814    0.6377    0.4186
                            0.3623    0         0.6377    0.4186];
                        Parent = getlinkedparents(Parent,relaxpos);
                    end
                    
                    Qc1 = [Quant ' comp.1'];
                    Qc2 = [Quant ' comp.2'];
                    ax=Parent(1);
                    if isfield(data,'YComp')
                        Y=data.YComp;
                    else
                        Y=data.ZComp;
                    end
                    hNew(1)=line(data.XComp,Y,'parent',ax);
                    setaxesprops(ax,'Val-Val',{Qc1 Qc2},{Units Units});
                    set(ax,'dataAspectRatio',[1 1 1], ...
                        'plotboxAspectRatio',[1 1 1e30])
                    
                    ax=Parent(2);
                    hNew(2)=line(data.Time,data.XComp,'parent',ax);
                    setaxesprops(ax,'Time-Val',{'' Qc1},{'' Units});
                    if ~isempty(stn)
                        qp_title(ax,stn)
                    end
                    
                    ax=Parent(3);
                    hNew(3)=line(data.Time,Y,'parent',ax);
                    setaxesprops(ax,'Time-Val',{'' Qc2},{'' Units});
                    set(hNew,Ops.LineParams{:});
                end
            case 'X-Z'
                n = ndims(data.XComp);
                s = min(data.X,[],n);
                [~,s,z] = resize2data(data.XComp,s,data.Z,Ops);
                if strcmp(Ops.plotcoordinate,'reverse path distance')
                    xsign=-1;
                else
                    xsign=1;
                end
                %
                % get right component to plot: select the component in the plane
                % to be plotted.
                %
                if isfield(data,'dX_tangential')
                    % arbcross
                    if length(data.dX_tangential) == length(s)
                        ex=data.dX_tangential;
                        ey=data.dY_tangential;
                    else
                        ex=data.dX_tangential([1:end end]);
                        ey=data.dY_tangential([1:end end]);
                        ex(ex~=ex([1 1:end-1]))=NaN;
                        ey(isnan(ex))=NaN;
                    end
                    %
                    repSize = [1 1];
                    repSize(n) = size(data.XComp,n);
                    %
                    ex = repmat(ex,repSize);
                    ey = repmat(ey,repSize);
                    planecomp=ex.*data.XComp + ey.*data.YComp;
                elseif multiple(M_)
                    planecomp=data.XComp;
                else % multiple(N_)
                    planecomp=data.YComp;
                end
                planecomp=xsign.*planecomp;
                planecomp((planecomp==0) & (data.ZComp==0))=NaN;
                
                hold on
                delete(hNew);
                if any(~isnan(data.XComp(:)))
                    
                    switch Ops.verticalscalingmode
                        case 'manual'
                            ScaleFacZ=Ops.verticalscalefactor;
                            set(gca,'dataaspectratio',[1 1/ScaleFacZ 1]);
                        case 'automatic'
                            if FirstFrame
                                c1=max(max(z(:))-min(z(:)),1e-6);
                                c2=max(s(:))-min(s(:));
                                ScaleFacZ=c2/c1/10;
                                set(gca,'dataaspectratio',[1 1/ScaleFacZ 1]);
                            else
                                da=get(gca,'dataaspectratio');
                                ScaleFacZ=da(1)/da(2);
                            end
                        otherwise % unrestricted, same as automatic per timestep without actually setting dataaspectratio
                            
                            c1=max(max(z(:))-min(z(:)),1e-6);
                            c2=max(s(:))-min(s(:));
                            ScaleFacZ=c2/c1/10;
                            if ScaleFacZ==0
                                ScaleFacZ = 1;
                            end
                    end
                    if ScaleFacZ==1
                        hNew=qp_vector(Parent,Ops.vectorstyle,s,z,[],planecomp,data.ZComp,[],quivopt{:});
                    else
                        
                        %       ----------
                        %        When the following lines are used, the lengths and the directions of
                        %        the vectors can be compared. The standard implementation allows for
                        %        the comparison of the individual components and the direction. The
                        %        standard implementation is furthermore consistent with the inter-
                        %        pretation of the vector as a particle displacement in a given period.
                        %
                        %        mag1=sqrt(planecomp.^2+data.ZComp.^2);
                        %        mag2=sqrt(planecomp.^2+(ScaleFacZ*data.ZComp).^2); mag2(mag2==0)=1;
                        %        mfac=mag1./mag2;
                        %        hNew=qp_vector(Parent,Ops.vectorstyle,s,ScaleFacZ*Zvector,[],mfac.*planecomp,ScaleFacZ*mfac.*data.ZComp,[],quivopt{:});
                        %       ----------
                        hNew=qp_vector(Parent,Ops.vectorstyle,s,ScaleFacZ*z,[],planecomp,ScaleFacZ*data.ZComp,[],quivopt{:});
                        for i=1:length(hNew)
                            set(hNew(i),'ydata',get(hNew(i),'ydata')/ScaleFacZ)
                        end
                        
                    end
                    if ~isempty(Ops.vectorcolour)
                        hNew=colquiver(hNew,data.Val);
                    else                        
                        set(hNew,'color',Ops.colour)
                    end
                    set(hNew,'linewidth',Ops.linewidth)
                else
                    hNew=line(1,1,'xdata',[],'ydata',[]);
                end
                set(gca,'layer','top')
                %ylabel('elevation (m) \rightarrow')
                if strcmp(Ops.colourbar,'none')
                    qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
                else
                    qp_title(Parent,{TStr},'quantity',Quant,'unit',Units,'time',TStr)
                end
                
            case {'X-Y','Lon-Lat','X-Y-Val','Lon-Lat-Val'}
                data.XComp((data.XComp==0) & (data.YComp==0))=NaN;
                I=~isnan(data.XComp(:));
                %
                minx=min(data.X(:));
                maxx=max(data.X(:));
                miny=min(data.Y(:));
                maxy=max(data.Y(:));
                %
                hold on
                delete(hNew);
                if any(I)
                    %
                    data.X=data.X(I);
                    data.Y=data.Y(I);
                    data.XComp=data.XComp(I);
                    data.YComp=data.YComp(I);
                    if isfield(data,'Z')
                        data.Z=data.Z(I);
                    end
                    if isfield(data,'ZComp')
                        data.ZComp=data.ZComp(I);
                    end
                    if isfield(data,'Val')
                        data.Val=data.Val(I);
                    end
                    %
                    if isfield(data,'ZComp')
                        hNew=qp_vector(Parent,Ops.vectorstyle,data.X,data.Y,data.Z,data.XComp,data.YComp,data.ZComp,quivopt{:});
                    else
                        hNew=qp_vector(Parent,Ops.vectorstyle,data.X,data.Y,[],data.XComp,data.YComp,[],quivopt{:});
                    end
                    
                    if ~isempty(Ops.vectorcolour)
                        if ~strcmp(Ops.Thresholds,'none')
                            vc = zeros(size(data.Val));
                            for i=1:length(Thresholds)
                                vc(data.Val>=Thresholds(i))=i;
                            end
                            data.Val=vc;
                            set(Parent,'clim',[1 length(Thresholds)]);
                        end
                        hNew=colquiver(hNew,data.Val);
                    else
                        set(hNew,'color',Ops.colour)
                    end
                    set(hNew,'linewidth',Ops.linewidth)
                    hNew(end+1)=line([minx maxx],[miny maxy],'linestyle','none','marker','none');
                else
                    hNew=line(1,1,'xdata',[],'ydata',[]);
                end
                if isempty(Selected{K_})
                    str=PName;
                    lyr={};
                else
                    lyr = qp_layer(Selected{K_});
                    str = sprintf('%s in %s',PName,lyr);
                    lyr = {lyr};
                end
                if strcmp(Ops.colourbar,'none')
                    qp_title(Parent,{str,TStr},'quantity',Quant,'unit',Units,'time',TStr)
                else
                    qp_title(Parent,[{TStr} lyr],'quantity',Quant,'unit',Units,'time',TStr)
                end
                
            case 'Val-Z'
                
                if ~isempty(hNew)
                    delete(hNew)
                end
                
                if length(Parent)==1
                    relaxpos = [ ...
                        0         0         0.4318    1.0000
                        0.5682    0         0.4318    1.0000];
                    Parent = getlinkedparents(Parent,relaxpos);
                end
                
                if isfield(data,'ZUnits') && ~isempty(data.ZUnits)
                    ZUnits = data.ZUnits;
                else
                    ZUnits = '';
                end
                
                if isfield(data,'Z')
                    Z = data.Z;
                elseif isfield(data,'XYZ')
                    Z = data.XYZ(:,:,:,3);
                end
                Qc1 = [Quant ' comp.1'];
                Qc2 = [Quant ' comp.2'];
                ax=Parent(1);
                hNew(1)=line(squeeze(data.XComp),squeeze(Z),'parent',ax);
                setaxesprops(ax,'Val-Z',{Qc1 'elevation'},{Units ZUnits});
                if ~isempty(stn)
                    qp_title(ax,stn,'quantity',Qc1,'unit',Units,'time',TStr)
                end
                
                ax=Parent(2);
                if isfield(data,'YComp')
                    hNew(2)=line(squeeze(data.YComp),squeeze(Z),'parent',ax);
                else
                    hNew(2)=line(squeeze(data.ZComp),squeeze(Z),'parent',ax);
                end
                setaxesprops(ax,'Val-Z',{Qc2 'elevation'},{Units ZUnits});
                qp_title(ax,TStr,'quantity',Qc2,'unit',Units,'time',TStr)
                
                set(hNew,Ops.LineParams{:});
                
            otherwise
                strxcomp = 'n/a';
                strycomp = 'n/a';
                strzcomp = 'n/a';
                if isfield(data,'XComp')
                    strxcomp = sprintf(Ops.numformat,data.XComp);
                end
                if isfield(data,'YComp')
                    strycomp = sprintf(Ops.numformat,data.YComp);
                end
                if isfield(data,'ZComp')
                    strzcomp = sprintf(Ops.numformat,data.ZComp);
                end
                if NVal==2
                    if isfield(data,'YComp')
                        strval=['[' strxcomp ' ' strycomp ']'];
                    else
                        strval=['[' strxcomp ' ' strzcomp ']'];
                    end
                else
                    strval=['[' strxcomp ' ' strycomp ' ' strzcomp ']'];
                end
                if isfield(Ops,'axestype') && isequal(strtok(Ops.axestype),'Time-Val')
                    ylim = get(Parent,'ylim');
                    yval = min(ylim(2),max(ylim(1),inf)); % XComp, YComp, ZComp, Magnitude?
                    if isempty(hNew)
                        hNew=line(data.Time*[1 1],ylim,'parent',Parent,'color',Ops.colour);
                        hNew(2)=text('position',[data.Time yval 0],'string',strval,'parent',Parent,Ops.FontParams{:});
                    else
                        set(hNew(1),'xdata',data.Time*[1 1],'ydata',ylim);
                        set(hNew(2),'position',[data.Time yval 0],'string',strval);
                    end
                else
                    hNew=gentext(hNew,Ops,Parent,['Val=',strval]);
                end
                
        end
    case {4}
        switch axestype
            case 'Text'
                hNew=gentext(hNew,Ops,Parent,data.Val);
            otherwise
                switch Ops.presentationtype
                    case {'markers'}
                        hNew=genmarkers(hNew,Ops,Parent,[],data.X,data.Y);
                    case {'labels'}
                        hNew=gentextfld(hNew,Ops,Parent,data.Val,data.X,data.Y);
                end
        end
end

function Parent = getlinkedparents(Parent,relaxpos)
if isappdata(Parent,'linkedaxes')
    Parent = getappdata(Parent,'linkedaxes');
else
    fg = get(Parent,'parent');
    ps = get(Parent,'position');
    pu = get(Parent,'units');
    tg = get(Parent,'tag');
    delete(Parent);
    Parent = qp_createaxes(fg,'relative',ps,pu,relaxpos);
    for i = 1:length(Parent)
        set(Parent(i),'tag',sprintf('%s [%i]',tg,i))
        setappdata(Parent(i),'linkedaxes',Parent)
    end
end
