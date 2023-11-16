function Iout=qp_iconedit(cmd)
%QP_ICONEDIT Icon editor
%   Interactive icon editor
%
%   QP_ICONEDIT

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

if nargin>0 & ischar(cmd)
    UD=get(gcf,'userdata');
    switch cmd
        case 'selcolor'
            Ncolors = size(UD.Icon.cmap,1);
            %
            cp = get(gca,'currentpoint');
            cp = max(0,round(cp(1,1:2))-[1 0]);
            mlcb = UD.MaxLengthColorBar;
            cp(1) = min(cp(1),floor((Ncolors+1)/mlcb));
            cp = sum(cp.*[UD.MaxLengthColorBar 1]);
            %
            cp = min(cp,Ncolors+1);
            if strcmp(get(gcf,'selectiontype'),'alt')
                if cp>Ncolors
                    NewColor = uisetcolor;
                    if isequal(NewColor,0)
                        return
                    end
                    UD.Icon.cmap(cp,:)=NewColor;
                else
                    UD.Icon.cmap(cp,:) = uisetcolor(UD.Icon.cmap(cp,:));
                end
            elseif cp>Ncolors
                return
            end
            UD.CurrentColor = cp;
            %
            update_interface(UD)
        case 'stopchangepoint'
            set(gcf,'windowbuttonmotionfcn','')
        case 'changepoint'
            cp=get(gca,'currentpoint');
            pnt=round(cp(1,1:2));
            if pnt(2)>=1 & pnt(2)<=size(UD.Icon.idx,1) & pnt(1)>=1 & pnt(1)<=size(UD.Icon.idx,2)
                UD.Icon.idx(pnt(2),pnt(1))=UD.CurrentColor;
                %
                update_interface(UD)
                %
                set(gcf,'windowbuttonmotionfcn','qp_iconedit changepoint')
            end
        case 'open'
            resetPath=pwd;
            cd(UD.Path);
            [FN,PN] = uigetfile(...
                {'*.bmp;*.cur;*.ico;*.gif;*.jpg;*.pbm;*.pcx;*.pgm;*.png;*.pnm;*.ppm;*.ras;*.tif;*.xwd', 'Bitmap Files'}, 'Select File');
            cd(resetPath);
            if ischar(FN)
                UD.Path=PN;
                FN = [PN FN];
                [PN2, FN2, EX2] = fileparts(FN);
                if isequal(lower(EX2),'.mat')
                    Icons = load(FN);
                    selectedIcon = ui_type('windowtitle','Select Icon',fieldnames(Icons));
                    if ~isempty(selectedIcon)
                        UD.Icon = getfield(Icons,selectedIcon);
                    end
                else
                    [UD.Icon.idx,UD.Icon.cmap,AlphaMask] = imread(FN);
                    if isempty(UD.Icon.cmap)
                        [UD.Icon.idx,UD.Icon.cmap]=rgb2idx(UD.Icon.idx);
                    end
                    if isa(UD.Icon.idx,'uint8')
                        UD.Icon.idx=double(UD.Icon.idx)+1;
                    end
                end
                update_interface(UD)
            end
        case 'saveas'
            resetPath=pwd;
            cd(UD.Path);
            [FN,PN] = uiputfile(...
                {'*.png;*.bmp', 'PNG/BMP Files'}, 'Specify Export File Name');
            cd(resetPath);
            if ischar(FN)
                FN = [PN FN];
                UD.Path=PN;
                [PN2, FN2, EX2] = fileparts(FN);
                if isempty(EX2)
                    FN=[FN '.png'];
                end
                if isequal(lower(EX2),'.mat')
                    if isstandalone
                        errordlg('Saving to MAT file not yet implemented.','Error','modal')
                    else
                        Icons = load(FN);
                        iconNames = fieldnames(Icons);
                        NewIcon={};
                        def='';
                        while isempty(NewIcon)
                            NewIcon=inputdlg('Enter name of icon in file','Icon Name',1,{def});
                            if isempty(NewIcon)
                                break
                            end
                            NewIcon=lower(NewIcon{1});
                            if strmatch(NewIcon,iconNames,'exact')
                                Replace=questdlg({sprintf('Icon named "%s" already exists.',NewIcon),'Do you want to replace it?'}, ...
                                    'Icon Name Conflict', ...
                                    'Yes','No','No');
                                if isequal(Replace,'No')
                                    def=NewIcon;
                                    NewIcon={};
                                end
                            end
                            if ~iscell(NewIcon)
                                lasterr('')
                                try
                                    % Icons = setfield(Icons,NewIcon,UD.Icon);
                                    eval([NewIcon '=UD.Icon;'])
                                    ops={};
                                    if matlabversionnumber>=7
                                        ops={'-v6'};
                                    end
                                    save(FN,NewIcon,'-append')
                                catch
                                    uiwait(errordlg(lasterr,'Error','modal'))
                                    def=NewIcon;
                                    NewIcon={};
                                end
                            end
                        end
                    end
                else
                    imwrite(UD.Icon.idx,UD.Icon.cmap,FN);
                end
            end
        case 'resize'
            set(UD.SmallPrevAxes, ...
                'units','normalized', ...
                'position',[0.9 0.8 0.05 0.1], ...
                'units','pixels')
            %
            update_interface(UD)
        case 'colormappack'
            ClrUsed=unique(UD.Icon.idx);
            AllClrs=1:size(UD.Icon.cmap,1);
            TranslateTable=cumsum(ismember(AllClrs,ClrUsed));
            UD.Icon.idx  = TranslateTable(UD.Icon.idx);
            UD.Icon.cmap = UD.Icon.cmap(ClrUsed,:);
            %
            update_interface(UD)
        case 'close'
            set(gcf,'visible','off')
        otherwise
            errordlg(sprintf('Unknown command: %s',cmd),'Error','modal')
    end
    set(gcf,'userdata',UD)
else
    if nargin>0
        UD.Icon=cmd;
    else
        UD.Icon.idx=ones(16,16);
        UD.Icon.cmap=[1 1 1];
    end
    Fig = figure('doublebuffer','on', ...
        'menubar','none', ...
        'numbertitle','off', ...
        'name','Icon Editor v1.00', ...
        'resizefcn','qp_iconedit resize');
    FileMenu = uimenu('parent',Fig,'label','&File');
    uimenu('parent',FileMenu,'label','&Open ...','callback','qp_iconedit open')
    uimenu('parent',FileMenu,'label','Save &As ...','callback','qp_iconedit saveas')
    ClrMenu = uimenu('parent',Fig,'label','Color &Map');
    uimenu('parent',ClrMenu,'label','Reduce to &Active Colors Only','callback','qp_iconedit colormappack')

    UD.Path=pwd;
    UD.CurrentColor=1;
    UD.MaxLengthColorBar=20;

    Icon_RGB = idx2rgb(UD.Icon.idx,UD.Icon.cmap);
    Ncolors = size(UD.Icon.cmap,1);
    Cmap_RGB = idx2rgb((1:Ncolors)',UD.Icon.cmap);
    Color = idx2rgb(UD.CurrentColor,UD.Icon.cmap);

    axes('position',[0.05 0.1 0.7 0.8])
    UD.Bitmap=imagesc(Icon_RGB);
    set(UD.Bitmap,'buttondownfcn','qp_iconedit changepoint')
    set(gcf,'windowbuttonupfcn','qp_iconedit stopchangepoint')
    set(gca,'da',[1 1 1],'xtick',[],'ytick',[])
    title('Enlarged Bitmap','fontsize',8)

    axes('position',[0.8 0.1 0.15 0.6])
    UD.Colorbar=imagesc(Cmap_RGB);
    UD.Hatch = line( ...
        [1.5 0.5 1.5 0.5 NaN 1.0 1.5 NaN 0.5 1.0], ...
        Ncolors+[0.5 0.5 1.5 1.5 NaN 0.5 1.0 NaN 1.0 1.5], ...
        'color','k', ...
        'userdata',[Ncolors 1]);
    set([UD.Colorbar UD.Hatch gca],'buttondownfcn','qp_iconedit selcolor')
    set(gca,'da',[1 1 1],'xtick',[],'ytick',[],'ylim',[0.5 Ncolors+1.5])
    title('Color Map','fontsize',8)

    axes('position',[0.8 0.8 0.05 0.05])
    UD.C=imagesc(Color);
    set(gca,'da',[1 1 1],'xtick',[],'ytick',[])
    title({'Current','Color'},'fontsize',8)

    UD.Tool(1) = uipushtool('cdata',Icon_RGB,'enable','on','tooltip','Enabled state');
    UD.Tool(2) = uipushtool('cdata',Icon_RGB,'enable','off','separator','on','tooltip','Disabled state');
    uipushtool('enable','off','separator','on');

    UD.SmallPrevAxes = axes('position',[0 0 1 1],'units','pixels');
    UD.Bitmap2=imagesc(Icon_RGB);
    set(gca,'da',[1 1 1],'xtick',[],'ytick',[])
    title({'Normal Size','Bitmap'},'fontsize',8)

    set(gcf,'userdata',UD,'closerequestfcn','qp_iconedit close')
    waitfor(gcf,'visible')
    UD=get(gcf,'userdata');
    delete(gcf)
    Iout=UD.Icon;
end


function update_interface(UD)
Ncolors = size(UD.Icon.cmap,1);
Icon_RGB = idx2rgb(UD.Icon.idx,UD.Icon.cmap);
mlcb = UD.MaxLengthColorBar;
if Ncolors<mlcb
    clridx=(1:Ncolors+1)';
    cbw=1;
else
    cbw=ceil((Ncolors+1)/mlcb);
    clridx=repmat(Ncolors+1,mlcb,cbw);
    clridx(1:Ncolors)=1:Ncolors;
end
Cmap_RGB = idx2rgb(clridx,[UD.Icon.cmap;get(gcf,'color')]);
Color = idx2rgb(UD.CurrentColor,UD.Icon.cmap);
set(UD.Bitmap,'cdata',Icon_RGB)
Ax=get(UD.Bitmap,'parent');
set(Ax, ...
    'xlim',[0.5 size(UD.Icon.idx,2)+0.5], ...
    'ylim',[0.5 size(UD.Icon.idx,1)+0.5])
set(get(Ax,'title'),'string',sprintf('Enlarged Bitmap (%ix%i)',size(UD.Icon.idx)))
if all(size(UD.Icon.idx)<=[32 32])
    set(UD.Tool,'cdata',Icon_RGB,'visible','on')
else
    set(UD.Tool,'visible','off')
end
set(UD.Colorbar,'cdata',Cmap_RGB)
set(get(UD.Colorbar,'parent'), ...
    'position',[0.8 0.1 0.15-max(0,2-cbw)*0.05 0.6], ...
    'xlim',[0.5 cbw+0.5], ...
    'ylim',[0.5 min(mlcb-1,Ncolors)+1.5])
set(UD.C,'cdata',Color)
Noldcolors=get(UD.Hatch,'userdata');
oldHx=Noldcolors(2);
oldHy=Noldcolors(1);
newHx=cbw;
newHy=Ncolors-(cbw-1)*mlcb;
set(UD.Hatch, ...
    'xdata',get(UD.Hatch,'xdata')+newHx-oldHx, ...
    'ydata',get(UD.Hatch,'ydata')+newHy-oldHy, ...
    'userdata',[newHy newHx])
p = get(UD.SmallPrevAxes,'position');
p(3:4)=fliplr(size(UD.Icon.idx))+1;
set(UD.SmallPrevAxes,'position',p, ...
    'xlim',[-0.5 size(UD.Icon.idx,2)+0.5], ...
    'ylim',[-0.5 size(UD.Icon.idx,1)+0.5])
set(UD.Bitmap2,'cdata',Icon_RGB)
