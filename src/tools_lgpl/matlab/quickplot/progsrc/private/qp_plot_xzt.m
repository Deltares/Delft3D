function [hNew,Param,Parent] = qp_plot_xzt(hNew,Parent,Param,data,Ops,Props,PName,TStr,stn,Quant,Units)
% axestype = 'Distance-Val','X-Val','X-Z','X-Time','Time-X','Time-Z','Time-Val'
T_=1; ST_=2; M_=3; N_=4; K_=5;

FirstFrame = Param.FirstFrame;
if ~isfield(Ops,'plotcoordinate')
    Ops.plotcoordinate = 'time';
    data.X = data.Time;
end
if ~isempty(strfind(Ops.basicaxestype,'Z')) && isfield(data,'Z') && Param.multiple(K_)
    hNew = plotslice(hNew,Parent,data,Ops,Props,Ops.Thresholds);
    if FirstFrame
        set(Parent,'view',[0 90],'layer','top');
        %set(get(Parent,'ylabel'),'string','elevation (m) \rightarrow')
    end
    if strcmp(Ops.colourbar,'none')
        tit = {PName};
    else
        tit = {};
    end
    if ~isempty(TStr)
        tit{end+1} = TStr;
    end
    if ~isempty(stn)
        tit{end+1}=stn;
    end
    qp_title(Parent,tit,'quantity',Quant,'unit',Units,'time',TStr)
elseif strcmp(Ops.plotcoordinate,'time')
    if Param.multiple(T_)
        if FirstFrame
            hNew=line(data.Time,data.Val, ...
                'parent',Parent, ...
                Ops.LineParams{:});
            if Props.DimFlag(T_)~=5
                tick(Parent,'x','autodate')
            end
        else
            set(hNew,'xdata',data.Time,'ydata',data.Val);
        end
        if ~isempty(stn)
            Str=stn;
        else
            Str='';
        end
        qp_title(Parent,Str,'quantity',Quant,'unit',Units,'time',TStr)
    else
        strval=sprintf(Ops.numformat,data.Val);
        if isfield(Ops,'axestype') && ...
                (isequal(strtok(Ops.axestype),'Time-Val') || ...
                isequal(strtok(Ops.axestype),'Time-Z'))
            ylim = get(Parent,'ylim');
            yval = min(ylim(2),max(ylim(1),data.Val));
            if isempty(hNew)
                hNew(2)=line(data.Time*[1 1],ylim,'parent',Parent,'color',Ops.colour);
                hNew(1)=text('position',[data.Time yval 0],'string',strval,'parent',Parent,Ops.FontParams{:});
            else
                i1 = strmatch('text',get(hNew,'type')); % 1 or 2
                i2 = 3-i1; % consequently, 2 or 1
                set(hNew(i2),'xdata',data.Time*[1 1],'ydata',ylim);
                set(hNew(i1),'position',[data.Time yval 0],'string',strval);
            end
        else
            unit = '';
            if ~isempty(Ops.units)
                unit = [' ' Ops.units];
            end
            hNew=gentext(hNew,Ops,Parent,['Val = ',strval,unit]);
        end
    end
else % distance-Val, X-Val, X-Time, Time-X
    %Ops.plotcoordinate='(x,y)';
    plotTime = ~isempty(strfind(Ops.basicaxestype,'Time'));
    if plotTime && isfield(data,'Time') && length(data.Time)>1
        mask = all(isnan(data.Val(:,:)),1);
    else
        mask = isnan(data.Val);
    end
    if size(data.Val,3)==1 && size(data.X,3)>1
        if size(data.X,3)==2
            data.X = mean(data.X,3);
            data.Y = mean(data.Y,3);
            data.Z = mean(data.Z,3);
        end
    end
    switch Ops.plotcoordinate
        case '(x,y)'
            data.X(mask)=NaN;
            data.Y(mask)=NaN;
            x=data.X;
            y=data.Y;
            z=data.Val;
        otherwise
            x=data.X;
            y=data.Val;
            z=zeros(size(x));
    end
    if plotTime && isfield(data,'Time') && length(data.Time)>1
        nx = numel(x);
        nt = numel(data.Time);
        if strcmp(Ops.axestype,'X-Time')
            c1 = repmat(reshape(x, [1 nx]), [nt 1]);
            c2 = repmat(reshape(data.Time, [nt 1]), [1 nx]);
            v = squeeze(data.Val);
        else
            c1 = repmat(reshape(data.Time, [nt 1]), [1 nx]);
            c2 = repmat(reshape(x, [1 nx]), [nt 1]);
            v = squeeze(data.Val);
        end
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
                hNew=gencontour(hNew,Ops,Parent,c1,c2,v,Ops.Thresholds);
                
        end
        if FirstFrame
            set(Parent,'view',[0 90],'layer','top');
        end
        if strcmp(Ops.colourbar,'none')
            qp_title(Parent,PName,'quantity',Quant,'unit',Units)
        else
            qp_title(Parent,'','quantity',Quant,'unit',Units)
        end
    else
        if strcmp(Ops.facecolour,'none')
            if length(y)==length(x)-1
                if isfield(Ops,'presentationtype') && strcmp(Ops.presentationtype,'linear')
                    x = (x(1:end-1)+x(2:end))/2;
                    if length(z)>length(y)
                        z = (z(1:end-1)+z(2:end))/2;
                    end
                else % stepwise
                    x = x(ceil(1:.5:length(x)-0.5));
                    y = y(ceil(.5:.5:length(y)));
                    z = z(ceil(1:.5:length(z)-0.5));
                end
            end
            if FirstFrame
                hNew=line(x,y,z, ...
                    'parent',Parent, ...
                    Ops.LineParams{:});
                set(Parent,'layer','top')
            elseif ishandle(hNew)
                set(hNew,'xdata',x, ...
                    'ydata',y, ...
                    'zdata',z);
            else
                return
            end
        else
            if ~FirstFrame
                delete(hNew)
            end
            vNaN=isnan(y);
            if any(vNaN)
                bs=findseries(~vNaN);
            else
                bs=[1 length(vNaN)];
            end
            for i=1:size(bs,1)
                if x(bs(i,1))==x(bs(i,2)) && ...
                        y(bs(i,1))==y(bs(i,2))
                    % this patch should not influence color scaling.
                    % however, the default "1" cdata will do so
                    % we cannot set the cdata to [] immediately
                    % so, we change it after having set all color options
                    hNew(i)=patch(x(bs(i,1):bs(i,2)), ...
                        y(bs(i,1):bs(i,2)), ...
                        1, ...
                        'edgecolor',Ops.colour, ...
                        'facecolor',Ops.facecolour, ...
                        'linestyle',Ops.linestyle, ...
                        'linewidth',Ops.linewidth, ...
                        'marker',Ops.marker, ...
                        'markersize',Ops.markersize, ...
                        'markeredgecolor',Ops.markercolour, ...
                        'markerfacecolor',Ops.markerfillcolour, ...
                        'cdata',[], ...
                        'parent',Parent);
                else
                    hNew(i)=line(x(bs(i,1):bs(i,2)), ...
                        y(bs(i,1):bs(i,2)), ...
                        'parent',Parent, ...
                        Ops.LineParams{:});
                end
            end
            set(Parent,'layer','top')
        end
        tit = {};
        if ~isempty(stn)
            tit{end+1}=stn;
        end
        if ~isempty(TStr)
            tit{end+1}=TStr;
        end
        qp_title(Parent,tit,'quantity',Quant,'unit',Units)
    end
end
