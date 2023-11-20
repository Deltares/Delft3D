function ui_inspectstruct(cmd,Title)
%UI_INSPECTSTRUCT Inspect a structure.
%
%   UI_INSPECTSTRUCT(Structure,Title)

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

% valid input argument?
if nargin==0
    error('One argument expected.')
else
    if isa(cmd,'org.apache.xerces.dom.DeferredDocumentImpl')
        tmp.XML = cmd;
        cmd = tmp;
    elseif ~ischar(cmd) && ~isstruct(cmd)
        try
           cmd = struct(cmd);
        catch
           error('Structure expected as argument.')
        end
    end
    StructName='';
    if ~isstandalone
        StructName=inputname(1);
    end
    if isempty(StructName) % function result
        StructName='[root]';
    end
end

fig=gcbf;
if ~ischar(cmd)
    %
    % Initialize and create interface window
    %
    % Define basic characteristics
    %
    Fig_Width=800;%500
    Fig_Height=500;%300
    XX=xx_constants;

    Units=get(0,'units');
    set(0,'units','pixels');
    ss = qp_getscreen;
    set(0,'units',Units);
    swidth = ss(3);
    sheight = ss(4);
    left = ss(1)+(swidth-Fig_Width)/2;
    bottom = ss(2)+(sheight-Fig_Height)/2;
    rect = [left bottom Fig_Width Fig_Height];
    
    %
    % Create figure in background
    %
    if nargin<2
        Title='Structure tree';
    end
    fig=figure('visible','off', ...
        'windowstyle','normal', ...
        'menu','none', ...
        'resizefcn','ui_inspectstruct resize', ...
        'integerhandle','off', ...
        'numbertitle','off', ...
        'handlevisibility','off', ...
        'color',XX.Inactive, ...
        'units','pixels', ...
        'position',rect, ...
        'name',Title);
    
    %
    % Create array index slider
    %
    H.ITxt=uicontrol('style','text', ...
        'units','pixels', ...
        'string','Index', ...
        'horizontalalignment','left', ...
        'backgroundcolor',XX.Inactive, ...
        'parent',fig);
    
    H.Index=uicontrol('style','edit', ...
        'units','pixels', ...
        'string','1', ...
        'backgroundcolor',XX.Active, ...
        'horizontalalignment','right', ...
        'parent',fig, ...
        'callback','ui_inspectstruct index');
    
    H.ISld=uicontrol('style','slider', ...
        'units','pixels', ...
        'min',1, ...
        'value',1, ...
        'max',2, ...
        'userdata',[1 2 1], ...
        'parent',fig, ...
        'callback','ui_inspectstruct slider');
    
    %
    % Create close button
    %
    H.Done=uicontrol('style','pushbutton', ...
        'units','pixels', ...
        'string','Close', ...
        'parent',fig, ...
        'callback','ui_inspectstruct done');
    
    %
    % Create tree area
    %
    H.Subs=uicontrol('style','listbox', ...
        'units','pixels', ...
        'string','', ...
        'backgroundcolor',XX.Active, ...
        'horizontalalignment','left', ...
        'parent',fig, ...
        'fontname','Courier', ...
        'keypressfcn',@browse_tree, ...
        'callback',@browse_tree);
    
    %
    % Create fields area
    %
    H.Fields=uicontrol('style','listbox', ...
        'units','pixels', ...
        'string','', ...
        'max',2, ...
        'backgroundcolor',XX.Inactive, ...
        'horizontalalignment','left', ...
        'parent',fig, ...
        'enable','off', ...
        'fontname','Courier', ...
        'callback','ui_inspectstruct down');
    
    %
    % Store state and show figure
    %
    UD.Struct=cmd;
    UD.StructName=StructName;
    UD.Index=[];
    UD.H=H;
    set(fig,'userdata',UD)
    set(fig,'visible','on')
    if isempty(UD.Struct)
        Str='  ';
    else
        Str='+ ';
    end
    Rec={[] StructName 0 Str};
    set(H.Subs,'string',{[Str StructName]},'value',1,'userdata',Rec)
    update(cmd,H)
%     if isempty(Struct)
%         LocalTree(Struct,StructName,UD.Index,0,0,H,fig)
%     else
%         LocalTree(Struct,StructName,UD.Index,1,1,H,fig)
%     end
    return
end

UD=get(fig,'userdata');
H=UD.H;
SliderInfo=get(H.ISld,'userdata');
IndexVal=SliderInfo(1);
IndexMax=SliderInfo(2);
Index=UD.Index;

switch cmd
    case 'done'
        if ishandle(fig)
            delete(fig);
            return
        end
    case {'index','slider'}
        if strcmp(cmd,'index')
            i=str2double(get(H.Index,'string'));
            if isnan(i)
                % IndexVal=IndexVal;
            elseif i<1
                IndexVal=1;
            elseif i>IndexMax
                IndexVal=IndexMax;
            else
                IndexVal=i;
            end
        else
            dI=get(H.ISld,'value')-IndexVal;
            IndexVal=IndexVal+sign(dI)*ceil(abs(dI));
        end
        set(H.Index,'string',num2str(IndexVal));
        SliderInfo(1)=IndexVal;
        set(H.ISld,'value',IndexVal,'userdata',SliderInfo);
        update(UD.Struct,H)
    case 'resize'
        pos=get(fig,'position');
        Fig_Width=pos(3);
        Fig_Height=pos(4);
        XX=xx_constants;
        
        Total_Width=Fig_Width-3*XX.Margin;
        Subs_Width=min(300,floor(Total_Width/3));
        Field_Width=Total_Width-Subs_Width;
        List_Height=max(1,Fig_Height-3*XX.Margin-XX.But.Height);
        Txt_Width=floor(min(40,Subs_Width/3));
        Val_Width=floor(min(60,(Subs_Width-Txt_Width)/2));
        Scr_Width=Subs_Width-Txt_Width-Val_Width;
        %
        rect = [XX.Margin XX.Margin Txt_Width XX.Txt.Height];
        set(H.ITxt,'position',rect)
        %
        rect(1) = rect(1)+rect(3);
        rect(3) = Val_Width;
        rect(4) = XX.But.Height;
        set(H.Index,'position',rect)
        %
        rect(1) = rect(1)+rect(3);
        rect(3) = Scr_Width;
        rect(4) = XX.But.Height;
        set(H.ISld,'position',rect)
        %
        rect(1) = rect(1)+rect(3)+XX.Margin;
        rect(3) = Field_Width;
        set(H.Done,'position',rect)
        %
        rect(1) = XX.Margin;
        rect(2) = rect(2)+rect(4)+XX.Margin;
        rect(3) = Subs_Width;
        rect(4) = List_Height;
        set(H.Subs,'position',rect)
        %
        rect(1) = rect(1)+rect(3)+XX.Margin;
        rect(3) = Field_Width;
        set(H.Fields,'position',rect)
end
UD.Index=Index;
set(fig,'userdata',UD);


function [Str,Rec,nadded]=expand(Struct,Str,Rec,i,IndexVal)
if isempty(Rec{i,1})
    SubStruct=Struct;
else
    SubStruct=subsref(Struct,Rec{i,1});
end
nflds = 0;
switch class(SubStruct)
    case 'struct'
        Str{i} = sprintf('%s- %s(%i)',repmat('  ',1,Rec{i,3}),Rec{i,2},1);
        Rec{i,1}(end+1).type='()';
        Rec{i,1}(end).subs={IndexVal};
        Rec{i,4} = '- ';
        %
        SubStruct=subsref(Struct,Rec{i,1});
        flds = fieldnames(SubStruct);
        nflds = length(flds);
        nfllw = i+1:length(Str);
        Str(nflds+nfllw)=Str(nfllw);
        Rec(nflds+nfllw,:)=Rec(nfllw,:);
        %
        for j=1:nflds
            Rec{i+j,1} = Rec{i,1};
            Rec{i+j,1}(end+1).type='.';
            Rec{i+j,1}(end).subs=flds{j};
            Rec{i+j,2} = flds{j};
            Rec{i+j,3} = Rec{i,3}+1;
            Rec{i+j,4} = '+ ';
            Str{i+j} = sprintf('%s  %s',repmat('  ',1,Rec{i+j,3}),Rec{i+j,2});
        end
    case {'org.apache.xerces.dom.DeferredDocumentImpl[]'
            'org.apache.xerces.dom.DeferredElementImpl[]'
            'org.apache.xerces.dom.DeferredDocumentImpl'
            'org.apache.xerces.dom.DeferredElementImpl'}
        cls = class(SubStruct);
        if strcmp(cls(end-1:end),'[]')
            Str{i} = sprintf('%s- %s',repmat('  ',1,Rec{i,3}),Rec{i,2});
            Rec{i,1}(end+1).type='()';
            Rec{i,1}(end).subs={IndexVal};
            Rec{i,4} = '- ';
        else
            Str{i} = sprintf('%s* %s',repmat('  ',1,Rec{i,3}),Rec{i,2});
            Rec{i,4} = '* ';
        end
        %
        SubStruct=subsref(Struct,Rec{i,1});
        nflds = SubStruct.getLength;
        nfllw = i+1:length(Str);
        Str(nflds+nfllw)=Str(nfllw);
        Rec(nflds+nfllw,:)=Rec(nfllw,:);
        %
        for j=1:nflds
            Rec{i+j,1} = Rec{i,1};
            Rec{i+j,1}(end+1).type='.';
            Rec{i+j,1}(end).subs='item';
            Rec{i+j,1}(end+1).type='()';
            Rec{i+j,1}(end).subs={j-1};
            Rec{i+j,2} = char(SubStruct.item(j-1).getNodeName);
            Rec{i+j,3} = Rec{i,3}+1;
            Rec{i+j,4} = '+ ';
            Str{i+j} = sprintf('%s  %s',repmat('  ',1,Rec{i+j,3}),Rec{i+j,2});
        end
end
nadded = nflds;


function [Str,Rec,nremoved]=collapse(Str,Rec,i)
Str{i} = sprintf('%s+ %s',repmat('  ',1,Rec{i,3}),Rec{i,2});
if strcmp(Rec{i,4},'- ')
    Rec{i,1}(end)=[];
end
Rec{i,4} = '+ ';
%
remove=false(size(Str));
for j=i+1:length(remove)
    if Rec{j,3}>=Rec{i,3}+1
        remove(j)=true;
    else
        break
    end
end
Str(remove)=[];
Rec(remove,:)=[];
nremoved=sum(remove);

        
function update(Struct,H)
XX=xx_constants;
i=get(H.Subs,'value');
Str=get(H.Subs,'string');
Rec=get(H.Subs,'userdata');
islid=i;
nlevel=length(Rec{i,1});
while islid>0
    if strcmp(strtok(Str{islid}),'-') && length(Rec{islid,1})<=nlevel
        break
    else
        islid=islid-1;
    end
end
%
if islid==0
    set(H.ITxt,'enable','off');
    set(H.Index,'string','','enable','off','backgroundcolor',XX.Inactive);
    set(H.ISld,'enable','off');
else
    indact='on';
    indclr=XX.Active;
    %
    Subscript=Rec{islid,1};
    if length(Subscript)<=1
        SubStruct=Struct;
    else
        SubStruct=subsref(Struct,Subscript(1:end-1));
    end
    nVal=numel(SubStruct);
    %
    if nVal<=1
        indact='off';
        indclr=XX.Inactive;
    end
    %
    SliderInfo=get(H.ISld,'userdata');
    if SliderInfo(3)==islid
        IndexVal=min(SliderInfo(1),nVal);
        if nVal>0
            IndexVal=max(IndexVal,1);
        end
    else
        IndexVal=Subscript(end).subs{1};
    end
    indexChanged = IndexVal~=Subscript(end).subs{1};
    %
    SliderInfo(1)=IndexVal;
    SliderInfo(2)=nVal;
    SliderInfo(3)=islid;
    set(H.ITxt,'enable','on');
    set(H.Index,'string',num2str(IndexVal),'enable',indact,'backgroundcolor',indclr);
    set(H.ISld,'enable',indact,'value',max(1,IndexVal),'max',max(2,nVal),'sliderstep',[min(0.1,1/max(1,nVal)) min(1,10/max(1,nVal))], ...
        'userdata',SliderInfo);
    %
    j=length(Subscript);
    if indexChanged
        switch class(SubStruct)
            case {'org.apache.xerces.dom.DeferredDocumentImpl[]'
                    'org.apache.xerces.dom.DeferredElementImpl[]'}
                % the different array elements can have different items
                % we might even support org.apache.xerces.dom.ParentNode[]
                % in which case the array elements may be of different
                % type!
                %
                % the solution is to collapse and expand again
                [Str,Rec,nremoved]=collapse(Str,Rec,islid);
                [Str,Rec,nadded]=expand(Struct,Str,Rec,islid,IndexVal);
                if i>islid && i<=islid+nremoved && i>islid+nadded
                    i = islid;
                end
        end
    end
    %
    k=islid;
    while k<=length(Str)
        if length(Rec{k,1})<j
            break
        end
        Rec{k,1}(j).subs={IndexVal};
        if k==islid
            Str{k}=sprintf('%s- %s(%i)',repmat('  ',1,Rec{k,3}),Rec{k,2},IndexVal);
        else
            if (strcmp(Rec{k,4},'- ') || strcmp(Rec{k,4},'* ')) && indexChanged
                % if expanded then collapse
                [Str,Rec,nremoved]=collapse(Str,Rec,k);
                if i>k
                    i=i-nremoved;
                end
            end
            SubStruct=subsref(Struct,Rec{k,1});
            exp = '  ';
            index = '';
            switch class(SubStruct)
                case 'struct'
                    exp=Rec{k,4};
                    if isequal(Rec{k,1}(end).type,'()')
                        index=sprintf('(%i)',Rec{k,1}(end).subs{1});
                    end
                case {'org.apache.xerces.dom.DeferredDocumentImpl'
                        'org.apache.xerces.dom.DeferredElementImpl'}
                    if SubStruct.getLength<1
                        % no subtags
                    elseif SubStruct.getLength==1 && strcmp(char(SubStruct.item(0).getNodeName),'#text')
                        % only #text
                    else
                        exp=Rec{k,4};
                    end
                case {'org.apache.xerces.dom.DeferredDocumentImpl[]'
                        'org.apache.xerces.dom.DeferredElementImpl[]'}
                    exp=Rec{k,4};
            end
            Str{k}=sprintf('%s%s%s%s',repmat('  ',1,Rec{k,3}),exp,Rec{k,2},index);
        end
        k=k+1;
    end
    set(H.Subs,'string',Str,'userdata',Rec,'value',i)
end
%
Subscript=Rec{i,1};
if isempty(Subscript)
    SubStruct=Struct;
else
    SubStruct=subsref(Struct,Subscript);
end
Str2=var2str(SubStruct);
if isempty(Str2)
    set(H.Fields,'string','','callback','','enable','off','backgroundcolor',XX.Inactive,'value',1);
else
    curline = get(H.Fields,'value');
    if iscell(Str2)
        nlines = length(Str2);
    else
        nlines = size(Str2,1);
    end
    if any(curline>nlines)
        curline = 1;
    end
    set(H.Fields,'string',Str2,'enable','on','backgroundcolor',XX.Active,'value',curline);
end


function browse_tree(h,event)
if isstruct(event) && isfield(event,'Key')
    key = event.Key;
else
    % mouse
    key = get(get(h,'Parent'),'selectiontype');
end
UD=get(get(h,'parent'),'userdata');
H=UD.H;
j=get(h,'value');
Str=get(h,'string');
Rec=get(h,'userdata');
j=min(j,length(Str));
open = 0;
close = 0;
scroll = 0;
switch strtok(Str{j})
    case '+'
        open   = strcmp(key,'open') | strcmp(key,'rightarrow');
    case {'-','*'}
        scroll = strcmp(get(H.ISld,'enable'),'on')& (strcmp(key,'leftarrow') | strcmp(key,'rightarrow'));
        close  = strcmp(key,'open') | strcmp(key,'leftarrow');
    otherwise
        %scroll = strcmp(key,'leftarrow') | strcmp(key,'rightarrow');
end
if scroll
    % scroll
    if strcmp(get(H.ISld,'enable'),'on')
        SliderInfo=get(H.ISld,'userdata');
        IndexVal = SliderInfo(1);
        IndexMax = SliderInfo(2);
        if IndexVal==1 && strcmp(key,'leftarrow')
            % close
            [Str,Rec]=collapse(Str,Rec,j);
            set(H.Subs,'string',Str,'userdata',Rec);
        elseif strcmp(key,'leftarrow')
            IndexVal = IndexVal-1;
            SliderInfo(1)=IndexVal;
            set(H.ISld,'value',IndexVal,'userdata',SliderInfo);
        else %if strcmp(key,'rightarrow')
            IndexVal = min(IndexVal+1,IndexMax);
            SliderInfo(1)=IndexVal;
            set(H.ISld,'value',IndexVal,'userdata',SliderInfo);
        end
    end
elseif close
    % close
    [Str,Rec]=collapse(Str,Rec,j);
    set(H.Subs,'string',Str,'userdata',Rec);
elseif open
    % open
    [Str,Rec]=expand(UD.Struct,Str,Rec,j,1);
    set(H.Subs,'string',Str,'userdata',Rec);
end
update(UD.Struct,H)

