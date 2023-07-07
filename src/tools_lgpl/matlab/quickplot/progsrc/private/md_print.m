function [Settings, fileNameArg] = md_print(varargin)
%MD_PRINT Send a figure to a printer.
%   MD_PRINT(FigureHandles, Settings, FileName)
%   Opens the user interface for the specified figures (in the
%   specified order). If no argument is specified, the command works
%   on the current figure (if it exists).
%
%   [Settings, FigureHandles] = MD_PRINT('getsettings', FigureHandles)
%   Returns the (last) selected output mode and the settings. To use
%   the UI for selection only (i.e. no actual printing/saving of a
%   figure), use an empty list of figures: Settings=MD_PRINT([])
%
%   MD_PRINT(FigureHandles, Settings, FileName)
%   Prints the specified figures using the specified settings.
%   The filename is optional for output to file (PS/EPS/TIF/JPG/EMF/PNG).
%
%   See also PRINT.

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

%  Painters   'Printer Name'         Platforms    COLOR    MultiPage
%  COLOR=0 Never
%  COLOR=1 User Specified
%  COLOR=2 Always
%  COLOR=3 Always - White Background & Black Axes inactive

% Called by print -dsetup:
%#function orient

persistent PL

if isempty(PL)
    % W: supported when running within MATLAB on Windows
    % WD: supported in deployed mode on Windows
    % L: supported when running within MATLAB on Linux
    % LD: supported in deployed mode on Linux
    W  = 1;
    WD = 2;
    L  = 4;
    LD = 8;
    win = W+WD;
    lnx = L+LD;
    all = win+lnx;
    %
    PL={1  'PDF file'                    all        1     1  0
        1  'Multi page PDF file'         all        1     1  1
        1  'PS file'                     all        1     0  0
        1  'EPS file'                    all        1     0  0
        0  'TIF file'                    all        2     0  0
        0  'BMP file'                    all        2     0  0
        0  'PNG file'                    all        2     0  0
        0  'JPG file'                    all        2     0  0
        1  'EMF file'                    win        2     0  0
        -1 'MATLAB fig file'             all        3     0  0
        0  'Bitmap to clipboard'         win        2     0  0
        1  'Metafile to clipboard'       win        2     0  0
        1  'Windows printer'             W          2     0  0
        1  'default Windows printer'     WD         2     0  0
        1  'other Windows printer'       WD         2     0  0
        -1 'QUICKPLOT session file'      all        3     1  0};
    if isdeployed
        if ispc
            code = WD;
        else
            code = LD;
        end
    else
        if ispc
            code = W;
        else
            code = L;
        end
    end
    Remove = ~bitget(cat(1,PL{:,3}),log2(code)+1);
    PL(Remove,:)=[];
end

getSettings = 0;
keepOpen = 0;
printObj.PrtID = -1;
printObj.NextNr = 1;
printObj.GhostScript = qp_settings('ghostscript','');
fileNameArg = '';
if nargin == 0
    shh=get(0,'showhiddenhandles');
    set(0,'showhiddenhandles','on');
    figlist = get(0,'currentfigure');
    Local_ui_args = {get(0,'children') figlist};
    set(0,'showhiddenhandles',shh);
else
    for i = 1:nargin
        if ischar(varargin{i})
            switch lower(varargin{i})
                case 'getsettings'
                    getSettings = 1;
                case 'keepopen'
                    keepOpen = 1;
                    if nargout == 0
                        error('Output argument required when using input argument ''keepopen''.')
                    end
                otherwise
                    fileNameArg = varargin{i};
            end
        elseif isstruct(varargin{i})
            printObj = varargin{i};
        else
            figlist = varargin{i};
        end
    end
    Local_ui_args = {};
end

selfiglist = intersect(figlist, allchild(0));
if length(selfiglist) < length(figlist)
    fprintf('Non-figure handles removed from print list.\n');
    figlist = selfiglist;
end

if ischar(printObj.PrtID)
    printObj.PrtID = ustrcmpi(printObj.PrtID, PL(:,2));
elseif printObj.PrtID >= 0
    error('Please replace PrtID number by printer name.')
end

if isfield(printObj, 'SelectFrom')
    Local_ui_args = {printObj.SelectFrom figlist};
elseif isempty(Local_ui_args) && ~isempty(figlist)
    Local_ui_args = {figlist figlist};
end

Ex = [];
is_fig = false(size(figlist));
for i = 1:length(figlist)
    if ishandle(figlist(i))
        if strcmp(get(figlist(i),'type'),'figure')
            is_fig(i) = true;
        end
    end
end
figlist = figlist(is_fig);

if ~isempty(figlist) && printObj.PrtID < 0
    figure(figlist(1));
end
if getSettings || printObj.PrtID < 0
    [printObj, figlistnew] = print_dialog(PL, length(figlist)>1, printObj, Local_ui_args{:});
    if printObj.PrtID > 0 && ~isempty(Local_ui_args)
        figlist = figlistnew;
    end
end
if getSettings
    %----- return ----
    if printObj.PrtID > 0
        printObj.PrtID = PL{printObj.PrtID,2};
        Settings = printObj;
    else
        Settings = [];
    end
    fileNameArg = figlist;
    return
end

printObj.Append = false;
if printObj.PrtID == 0
    figlist = [];
else
    printObj.Name = PL{printObj.PrtID, 2};
    %
    HG2 = ~ismember('zbuffer',set(0,'defaultfigurerenderer'));
    switch PL{printObj.PrtID,1}
        case -1 % painters/zbuffer irrelevant
            printObj.Method=1;
        case 0 % cannot be combined with painters, e.g. bitmap
            if printObj.Method==1 || printObj.Method==2
                printObj.Method = 2;
                if HG2 % HG2 mode does not support ZBuffer printing
                    printObj.Method = 3;
                end
            end
        otherwise
            if printObj.Method==2 && HG2 % HG2 mode does not support ZBuffer printing
                printObj.Method = 3;
            end
    end
    %
    switch printObj.Method
        case 1 % Painters
            if matlabversionnumber >= 9.11
                printObj.PrtMth = {'-vector'};
            else
                printObj.PrtMth = {'-painters'};
            end
        case 2 % Zbuffer
            printObj.PrtMth = {'-zbuffer', sprintf('-r%i',printObj.DPI)};
        case 3 % OpenGL
            if matlabversionnumber >= 9.11
                printObj.PrtMth = {'-image', sprintf('-r%i',printObj.DPI)};
            else
                printObj.PrtMth = {'-opengl', sprintf('-r%i',printObj.DPI)};
            end
        otherwise
            printObj.PrtMth = {};
    end
    
    switch printObj.Name
        case 'TIF file'
            printObj.ext='tif';
            printObj.dvr='-dtiff';
        case 'BMP file'
            printObj.ext='bmp';
            printObj.dvr='-dbmp';
        case 'PNG file'
            printObj.ext='png';
            printObj.dvr='-dpng';
        case 'JPG file'
            printObj.ext='jpg';
            printObj.dvr='-djpeg';
        case 'PDF file'
            printObj.ext='pdf';
            printObj.dvr='-dpdf';
        case 'Multi page PDF file'
            printObj.ext='pdf';
            printObj.dvr='-dps';
            if printObj.Color
                printObj.dvr='-dpsc';
            end
            printObj.Append = true;
        case 'EPS file'
            printObj.ext='eps';
            printObj.dvr='-deps';
            if printObj.Color
                printObj.dvr='-depsc';
            end
        case 'PS file'
            printObj.ext='ps';
            printObj.dvr='-dps';
            if printObj.Color
                printObj.dvr='-dpsc';
            end
        case 'EMF file'
            printObj.ext='emf';
            printObj.dvr='-dmeta';
        case 'MATLAB fig file'
            printObj.ext = 'fig';
            printObj.dvr='';
        case 'QUICKPLOT session file'
            printObj.ext = 'qpses';
            printObj.dvr = '';
            printObj.Append = true;
        otherwise
            printObj.ext='';
            printObj.dvr='';
    end
end

i = 0;
while i < length(figlist)
    if ~isfield(printObj,'Name')
        break
    elseif ~isfield(printObj,'FileName') || isempty(printObj.FileName)
        printObj.FileName = fileNameArg;
    end
    i=i+1;
    fig = figlist(i);
    if ~printObj.Append || i == 1
        printObj = printInitialize(printObj,length(figlist));
        fileNameArg = printObj.FileName;
    end
    if ~ischar(printObj.FileName)
        break
    end

    hvis = get(fig, 'handlevisibility');
    set(fig, 'handlevisibility', 'on');
    
    try
        printObj = printAdd(printObj, fig);
        if ~printObj.Append
            printObj = printFinalize(printObj);
        end
    catch Ex
        %rethrow the error after resetting a few things ...
        % clear the figlist to end the loop ASAP
        figlist = [];
    end
    if ~printObj.Append
        printObj.FilePath = fileparts(printObj.FileName);
        printObj.FileName = '';
    end
    set(fig,'handlevisibility',hvis);
end
try
    if printObj.Append && ~keepOpen
        printObj = printFinalize(printObj);
    end
catch Ex2
    if isempty(Ex)
        rethrow(Ex2)
    end
end
if ~isempty(Ex)
    rethrow(Ex)
end

if nargout>0
    %----- return ----
    if printObj.PrtID > 0
        printObj.PrtID = PL{printObj.PrtID,2};
    end
    Settings = printObj;
end


function printObj = printInitialize(printObj,nPrints)
if isempty(printObj.FileName) && ~isempty(printObj.ext)
    defFile = ['default.' printObj.ext];
    if isfield(printObj,'FilePath')
        defFile = fullfile(printObj.FilePath, defFile);
    end
    [fn,pn] = uiputfile(defFile,'Specify file name');
    printObj.FileName = [pn,fn];
    if ~strcmp(printObj.Name,'Multi page PDF file') && nPrints > 1
        [printObj.BaseName,printObj.NextNr,printObj.FrmtNr] = DetectBaseAndNumber(printObj.FileName);
    end
elseif ~isempty(printObj.ext) && ~isfield(printObj,'PDFName')
    [f,p,e] = fileparts(printObj.FileName);
    if ~strcmp(e,['.' printObj.ext])
        printObj.FileName = [printObj.FileName '.' printObj.ext];
    end
end
if ~isfield(printObj,'NextNr')
    printObj.NextNr = 1;
end
if ischar(printObj.FileName)
    switch printObj.Name
        case 'Multi page PDF file'
            if ~isfield(printObj,'PDFName')
                printObj.PDFName = printObj.FileName;
                printObj.FileName = [tempname '.ps'];
            end
    end
end


function [base,nr,FrmtNr] = DetectBaseAndNumber(fn)
[p,f] = fileparts(fn);
ndig = 0;
while length(f)>ndig && ismember(f(end-ndig), '0123456789')
    ndig = ndig+1;
end
if ndig == 0
    ndig = 3;
    nr = 0;
else
    nr = sscanf(f(end-ndig+1:end),'%d');
    f = f(1:end-ndig);
end
base = fullfile(p,f);
if ndig>0
    ndigstr = num2str(ndig);
    FrmtNr = strcat('%',ndigstr,'.',ndigstr,'i');
else
    FrmtNr = '';
end

    
function printObj = printAdd(printObj, fig)
if ~ischar(printObj.FileName)
    return
end
if isnumeric(fig)
    FigHandle = sprintf('-f%20.16f',fig);
else
    FigHandle = fig;
end
switch printObj.Name
    case {'TIF file','BMP file','PNG file','JPG file','EPS file','PS file','EMF file','PDF file','Multi page PDF file'}
        ih=get(fig,'inverthardcopy');
        if isfield(printObj,'InvertHardcopy') && ~printObj.InvertHardcopy
            set(fig,'inverthardcopy','off');
        else
            printObj.InvertHardcopy=1;
            set(fig,'inverthardcopy','on');
        end
        %
        if strcmp(printObj.Name,'Multi page PDF file') && printObj.NextNr > 1
            append = {'-append'};
        else
            append = {};
        end
        %
        normtext = findall(fig,'fontunits','normalized');
        if ~isempty(normtext)
            set(normtext,'fontunits','points')
            drawnow % needed to get output file nicely formatted
        end
        
        %
        % make sure that the paper type is consistent with previous paper
        % type settings
        %
        if isfield(printObj,'Paper')
            pu  = get(fig,'paperunits');
            pt = get(fig,'papertype');
            po = get(fig,'paperorientation');
            ps = get(fig,'papersize');
            set(fig,'paperunits','inches')
            set(fig,'papersize',printObj.Paper.Size)
            set(fig,'paperorientation',printObj.Paper.Orientation)
            if ~strcmp(printObj.Paper.Type,'<custom>')
                set(fig,'papertype',printObj.Paper.Type)
            end
            set(fig,'paperunits',pu)
        end
        %
        % make sure that the printed figure fits onto the page
        %
        pp_adjusted = false;
        pu  = get(fig,'paperunits');
        ppm = get(fig,'paperpositionmode');
        pp  = get(fig,'paperposition');
        set(fig,'paperunits','normalized')
        ppn = get(fig,'paperposition');
        if any(ppn(1:2) < 0) || any(ppn(1:2)+ppn(3:4) > 1)
            % first attempt to get the whole figure always on the page. To
            % keep the aspect ratio we may have to set the paperposition a
            % bit more carefully ... and we may want to switch between
            % portrait and landscape mode ...
            set(fig,'paperposition',[ 0 0 1 1 ])
            pp_adjusted = true;
        end
        % fig
        if isfield(printObj,'BaseName')
            figname = [printObj.BaseName sprintf(printObj.FrmtNr, printObj.NextNr) '.' printObj.ext];
        else
            figname = printObj.FileName;
        end
        %
        % do the actual print
        %
        print(figname, FigHandle, printObj.dvr, printObj.PrtMth{:}, append{:});
        %
        % reset the paper position
        %
        set(fig,'paperunits',pu)
        if pp_adjusted
            if strcmp(ppm,'manual')
                set(fig,'paperposition',pp)
            else
                set(fig,'paperpositionmode','auto')
            end
        end
        %
        % reset with the paper type
        %
        if isfield(printObj,'Paper')
            set(fig,'papersize',ps)
            set(fig,'paperorientation',po)
            if ~strcmp(pt,'<custom>')
                set(fig,'papertype',pt)
            end
        end
        %
        % reset other properties
        %
        if ~isempty(normtext)
            set(normtext,'fontunits','normalized')
        end
        set(fig,'inverthardcopy',ih);
        %
        if strcmp(printObj.Name,'Multi page PDF file')
            switch printObj.PageLabels
                case 1
                    % no page label
                case 2
                    figname = sprintf('page %i', printObj.NextNr);
                    add_bookmark(printObj.FileName, figname, printObj.NextNr>1)
                case 3
                    figname = listnames(fig, 'showtype', 0, 'showhandle', 0, 'showtag',0);
                    add_bookmark(printObj.FileName, figname{1}, printObj.NextNr>1)
            end
            printObj.Paper = getPaperType(fig);
        end
    case 'MATLAB fig file'
        hgsave(fig,printObj.FileName);
    case 'QUICKPLOT session file'
        if isfield(printObj,'SES')
            appendto_SES = {printObj.SES};
        else
            appendto_SES = {};
        end
        printObj.SES = qp_session('extract', fig, appendto_SES{:});
    case {'Bitmap to clipboard', 'Metafile to clipboard', 'Windows printer', 'default Windows printer', 'other Windows printer'}
        ccd=cd;
        cd(tempdir);
        ih=get(fig,'inverthardcopy');
        if isfield(printObj,'InvertHardcopy') && ~printObj.InvertHardcopy
            set(fig,'inverthardcopy','off');
        else
            printObj.InvertHardcopy=1;
            set(fig,'inverthardcopy','on');
        end
        switch printObj.Name
            case 'other Windows printer'
                dvr='-dwin';
                if printObj.Color
                    dvr='-dwinc';
                end
                if isdeployed
                    deployprint(fig)
                else
                    print(FigHandle, dvr, printObj.PrtMth{:});
                end
            case 'default Windows printer'
                if exist('printdlg','file')
                    printdlg(fig)
                else
                end
            case 'Windows printer'
                dvr='-dwin';
                if printObj.Color
                    dvr='-dwinc';
                end
                print(FigHandle,dvr,printObj.PrtMth{:});
            case 'Bitmap to clipboard'
                set(fig,'inverthardcopy','off');
                print(FigHandle, '-dbitmap');
            case 'Metafile to clipboard'
                print(FigHandle, printObj.PrtMth{:}, '-dmeta');
        end
        set(fig,'inverthardcopy',ih);
        cd(ccd)
end
printObj.NextNr = printObj.NextNr + 1;


function printObj = printFinalize(printObj)
if ~isfield(printObj,'FileName') || ~ischar(printObj.FileName)
    return
end
switch printObj.Name
    case 'Multi page PDF file'
        if isfield(printObj,'Paper') && exist(printObj.FileName,'file')
            ps2pdf('psfile',printObj.FileName, ...
                'pdffile',printObj.PDFName, ...
                'gscommand','c:\Program Files\gs\gs10.00.0\bin\gswin64c.exe', ...
                'gspapersize',paperType2gs(printObj.Paper), ...
                'deletepsfile',1)
        end
        printObj.FileName = printObj.PDFName;
        printObj = rmfield(printObj, 'PDFName');
    case 'QUICKPLOT session file'
        SER = qp_session('serialize',printObj.SES);
        SER = qp_session('make_expandables',SER,{'filename','domain'});
        qp_session('save',SER,printObj.FileName)
end
printObj.FileName = '';


function paper =  getPaperType(fig)
paper.Type = get(fig,'papertype');
paper.Orientation = get(fig,'paperorientation');
pu = get(fig,'paperunits');
set(fig, 'paperunits', 'inches');
paper.Size = get(fig, 'papersize');
set(fig, 'paperunits', pu)


function papertypeGS =  paperType2gs(paper)
papertypeGS = paper.Type;
switch papertypeGS
    case 'usletter'
        papertypeGS = 'letter';
    case 'uslegal'
        papertypeGS = 'legal';
    case {'A1', 'A2', 'A3', 'A4', 'A5'}
        papertypeGS = lower(papertypeGS);
    case {'arch-A', 'arch-B', 'arch-C', 'arch-D', 'arch-E'}
        papertypeGS(5) = [];
    case 'tabloid'
        papertypeGS = '11x17';
    otherwise % custom, A-E, B0-B5
        papertypeGS = paper.Size;
end


function md_print_callback(obj,event,arg)
UD=get(gcbf,'userdata');
UD{end+1}=arg;
set(gcbf,'userdata',UD);


function [Settings,FigID] = print_dialog(PL,CanApplyAll,Settings,SelectFrom,FigID)
persistent PrtID Method DPI Clr InvertHardcopy PageLabels
if ~exist(qp_settings('ghostscript',''),'file')
    PL(strcmp(PL(:,2),'Multi page PDF file'),:)=[];
end
if isempty(PrtID)
    PrtID=1;
    Method=2;
    DPI=150;
    InvertHardcopy=1;
    Clr=1;
    PageLabels=1;
end
if nargin>2 && isstruct(Settings)
    if isfield(Settings,'PrtID')
        PrtID=Settings.PrtID;
        if PrtID<=0
            PrtID=1;
        end
    end
    if isfield(Settings,'Method')
        Method=Settings.Method;
    end
    if isfield(Settings,'DPI')
        DPI=Settings.DPI;
    end
    if isfield(Settings,'InvertHardcopy')
        InvertHardcopy=Settings.InvertHardcopy;
    end
    if isfield(Settings,'Clr')
        Clr=Settings.Clr;
    end
    if isfield(Settings,'PageLabels')
        PageLabels=Settings.PageLabels;
    end
end

Reselect = 0;
if nargin>3 && ~isempty(SelectFrom)
    Reselect = 1;
end
if nargin<5
    FigID = [];
end
XX=xx_constants;

PrintLabel=300;
TextLabel=50;
FigListHeight=200;

ZBufWidth=80;
ResWidth=50;

TextShift = [0 XX.Txt.Height-XX.But.Height 0 0];
Fig_Width=TextLabel+PrintLabel+3*XX.Margin;
Fig_Height=6*XX.Margin+9*XX.But.Height+XX.Txt.Height+ ...
    (XX.Margin+FigListHeight)*Reselect;

ss = qp_getscreen;
swidth = ss(3);
sheight = ss(4);
left = ss(1)+(swidth-Fig_Width)/2;
bottom = ss(2)+(sheight-Fig_Height)/2;
rect = [left bottom Fig_Width Fig_Height];

fig=qp_uifigure('Print/Export','','md_print',rect);
set(fig,'closerequestfcn','closereq')
GUI.Figure = fig;

rect = [XX.Margin XX.Margin (Fig_Width-3*XX.Margin)/2 XX.But.Height];
GUI.Cancel = uicontrol('style','pushbutton', ...
    'position',rect, ...
    'string','Cancel', ...
    'parent',fig, ...
    'callback',{@md_print_callback 'cancel'});

rect(1) = (Fig_Width+XX.Margin)/2;
GUI.OK=uicontrol('style','pushbutton', ...
    'position',rect, ...
    'string','OK', ...
    'parent',fig, ...
    'callback',{@md_print_callback 'OK'});

rect(1) = XX.Margin;

if PL{PrtID,6}
    enab = 'on';
    clr  = XX.Clr.White;
else
    enab = 'off';
    clr  = XX.Clr.LightGray;
end
rect(1) = XX.Margin;
rect(2) = rect(2)+rect(4)+XX.Margin;
rect(3) = 1.2*TextLabel;
rect(4) = XX.Txt.Height-2;
GUI.PLabelTxt=uicontrol('style','text', ...
    'position',rect, ...
    'parent',fig, ...
    'string','Page Labels', ...
    'horizontalalignment','left', ...
    'backgroundcolor',XX.Clr.LightGray, ...
    'enable',enab);
rect(1) = rect(1)+rect(3)+XX.Margin;
rect(3) = Fig_Width-rect(1)-XX.Margin;
rect(4) = XX.But.Height;
GUI.PLabels=uicontrol('style','popupmenu', ...
    'position',rect, ...
    'parent',fig, ...
    'value',PageLabels, ...
    'string',{'No Labels','Page Numbers','Figure Names'}, ...
    'backgroundcolor',clr, ...
    'enable',enab, ...
    'callback',{@md_print_callback 'PageLabels'});

rect(1) = XX.Margin;
rect(2) = rect(2)+rect(4);
rect(3) = Fig_Width-2*XX.Margin;
rect(4) = XX.But.Height;
GUI.InvHard=uicontrol('style','checkbox', ...
    'position',rect, ...
    'parent',fig, ...
    'value',InvertHardcopy==1, ...
    'string','White Background and Black Axes', ...
    'backgroundcolor',XX.Clr.LightGray, ...
    'enable','on', ...
    'callback',{@md_print_callback 'InvertHardcopy'});

rect(2) = rect(2)+rect(4);
rect(3) = Fig_Width-2*XX.Margin;
rect(4) = XX.But.Height;
GUI.Color=uicontrol('style','checkbox', ...
    'position',rect, ...
    'parent',fig, ...
    'string','Print as Colour', ...
    'horizontalalignment','left', ...
    'value',Clr, ...
    'enable','on', ...
    'callback',{@md_print_callback 'Color'});

rect(1) = XX.Margin;
rect(2) = rect(2)+rect(4)+XX.Margin;
rect(3) = ZBufWidth;
rect(4) = XX.But.Height;
GUI.OpenGL=uicontrol('style','radiobutton', ...
    'position',rect, ...
    'parent',fig, ...
    'string','OpenGL', ...
    'value',Method==3, ...
    'backgroundcolor',XX.Clr.LightGray, ...
    'enable','on', ...
    'callback',{@md_print_callback 'OpenGL'});

rect(1) = rect(1)+rect(3)+XX.Margin;
rect(3) = ResWidth;
GUI.Resol=uicontrol('style','edit', ...
    'position',rect, ...
    'parent',fig, ...
    'string',num2str(DPI), ...
    'horizontalalignment','right', ...
    'backgroundcolor',XX.Clr.LightGray, ...
    'enable','off', ...
    'callback',{@md_print_callback 'DPI'});

rect(1) = rect(1)+rect(3)+XX.Margin;
rect(3) = Fig_Width-(4*XX.Margin+ZBufWidth+ResWidth);
rect(4) = XX.Txt.Height;
GUI.ResolTxt = uicontrol('style','text', ...
    'position',rect+TextShift, ...
    'parent',fig, ...
    'string','DPI', ...
    'horizontalalignment','left', ...
    'backgroundcolor',XX.Clr.LightGray, ...
    'enable','off');
if Method==2 || Method==3
    set(GUI.Resol   ,'enable','on','backgroundcolor',XX.Clr.White)
    set(GUI.ResolTxt,'enable','on')
end

rect(4) = XX.But.Height;

rect(1) = XX.Margin;
rect(2) = rect(2)+rect(4);
rect(3) = Fig_Width-2*XX.Margin;
rect(4) = XX.But.Height;
GUI.ZBuf=uicontrol('style','radiobutton', ...
    'position',rect, ...
    'parent',fig, ...
    'value',Method==2, ...
    'string','ZBuffer', ...
    'backgroundcolor',XX.Clr.LightGray, ...
    'enable','on', ...
    'callback',{@md_print_callback 'Zbuffer'});

rect(1) = XX.Margin;
rect(2) = rect(2)+rect(4);
rect(3) = Fig_Width-2*XX.Margin;
rect(4) = XX.But.Height;
GUI.Painter=uicontrol('style','radiobutton', ...
    'position',rect, ...
    'parent',fig, ...
    'value',Method==1, ...
    'string','Painters (864 DPI)', ...
    'backgroundcolor',XX.Clr.LightGray, ...
    'enable','on', ...
    'callback',{@md_print_callback 'Painters'});

rect(1) = XX.Margin;
rect(2) = rect(2)+rect(4);
rect(3) = (Fig_Width-3*XX.Margin)/2;
rect(4) = XX.Txt.Height;
GUI.PrintMeth = uicontrol('style','text', ...
    'position',rect+TextShift, ...
    'parent',fig, ...
    'string','Printing Method', ...
    'horizontalalignment','left', ...
    'backgroundcolor',XX.Clr.LightGray, ...
    'enable','on');

rect(1) = 2*XX.Margin+TextLabel;
rect(2) = rect(2)+rect(4)+XX.Margin;
rect(3) = PrintLabel/2;
rect(4) = XX.But.Height;
GUI.Opt=uicontrol('style','pushbutton', ...
    'position',rect, ...
    'parent',fig, ...
    'string','Options...', ...
    'horizontalalignment','left', ...
    'backgroundcolor',XX.Clr.LightGray, ...
    'enable','off', ...
    'callback',{@md_print_callback 'Options'});

rect(1) = XX.Margin;
rect(2) = rect(2)+rect(4)+XX.Margin;
rect(3) = TextLabel;
rect(4) = XX.Txt.Height;
GUI.PrinterTxt = uicontrol('style','text', ...
    'position',rect+TextShift, ...
    'parent',fig, ...
    'string','Printer', ...
    'horizontalalignment','left', ...
    'backgroundcolor',XX.Clr.LightGray, ...
    'enable','on');

rect(1) = 2*XX.Margin+TextLabel;
rect(3) = PrintLabel;
rect(4) = XX.But.Height;
GUI.Printer=uicontrol('style','popupmenu', ...
    'position',rect, ...
    'parent',fig, ...
    'string',PL(:,2), ...
    'value',PrtID, ...
    'horizontalalignment','left', ...
    'backgroundcolor',XX.Clr.White, ...
    'enable','on', ....
    'callback',{@md_print_callback 'Printer'});

if Reselect
    rect(1) = 2*XX.Margin+TextLabel;
    rect(2) = rect(2)+rect(4)+XX.Margin;
    rect(3) = PrintLabel;
    rect(4) = FigListHeight;
    AllFigID = SelectFrom;
    AllFigNames = listnames(AllFigID,'showType','no','showHandle','no','showTag','no');
    [AllFigNames,Order] = sort(AllFigNames);
    AllFigID = AllFigID(Order);
    FigIndex = find(ismember(AllFigID,FigID));
    GUI.FigLst=uicontrol('style','listbox', ...
        'position',rect, ...
        'parent',fig, ...
        'string',AllFigNames, ...
        'value',FigIndex, ...
        'max',2, ...
        'horizontalalignment','left', ...
        'backgroundcolor',XX.Clr.White, ...
        'enable','on', ....
        'callback',{@md_print_callback 'Figure'});

    rect(1) = XX.Margin;
    rect(2) = rect(2)+rect(4)-XX.Txt.Height;
    rect(3) = TextLabel;
    rect(4) = XX.Txt.Height;
    GUI.FigTxt = uicontrol('style','text', ...
        'position',rect+TextShift, ...
        'parent',fig, ...
        'string','Figure(s)', ...
        'horizontalalignment','left', ...
        'backgroundcolor',XX.Clr.LightGray, ...
        'enable','on');
end

set(fig,'visible','on','color',XX.Clr.LightGray);
set(fig,'userdata',{'Printer'});

gui_quit=0;               % Becomes one if the interface has to quit.
stack=[];                 % Contains the stack of commands; read from 'userdata' field of the figure
Cancel=0;

while ~gui_quit

    %%*************************************************************************************************
    %%%% UPDATE SCREEN BEFORE WAITING FOR COMMAND
    %%*************************************************************************************************

    drawnow;

    %%*************************************************************************************************
    %%%% WAIT UNTIL A COMMAND IS ON THE STACK IN THE USERDATA FIELD OF THE FIGURE
    %%*************************************************************************************************

    if ishandle(fig)
        UD=get(fig,'userdata');
        if isempty(UD)
            waitfor(fig,'userdata');
        end
    end

    %%*************************************************************************************************
    %%%% SET POINTER TO WATCH WHILE PROCESSING COMMANDS ON STACK
    %%%% FIRST CHECK WHETHER FIGURE STILL EXISTS
    %%*************************************************************************************************

    if ishandle(fig)
        stack=get(fig,'userdata');
        set(fig,'userdata',{});
    else
        Cancel=1;
        gui_quit=1;
    end

    %%*************************************************************************************************
    %%%% START OF WHILE COMMANDS ON STACK LOOP
    %%*************************************************************************************************

    while ~isempty(stack)
        cmd=stack{1};
        stack=stack(2:size(stack,1),:);
        switch cmd
            case 'cancel'
                Cancel=1;
                gui_quit=1;
            case 'OK'
                gui_quit=1;
            case {'Figure','Printer'}
                if strcmp(cmd,'Figure')
                    FigIndex=get(GUI.FigLst,'value');
                    FigID=AllFigID(FigIndex);
                    if isempty(FigID)
                        set(GUI.OK,'enable','off')
                    else
                        set(GUI.OK,'enable','on')
                    end
                end
                PrtID=get(GUI.Printer,'value');
                if strcmp(PL{PrtID,2},'Windows printer')
                    set(GUI.Opt,'enable','on');
                else
                    set(GUI.Opt,'enable','off');
                end
                Method = update_renderer(PL{PrtID,1},GUI,Method,DPI);
                switch PL{PrtID,4}
                    case 0 % NEVER
                        set(GUI.Color,'enable','off','value',0);
                        set(GUI.InvHard,'enable','on','value',InvertHardcopy==1);
                        Clr=0;
                    case 1 % USER SPECIFIED (DEFAULT ON)
                        set(GUI.Color,'enable','on','value',1);
                        set(GUI.InvHard,'enable','on','value',InvertHardcopy==1);
                        Clr=1;
                    case 2 % ALWAYS
                        set(GUI.Color,'enable','off','value',1);
                        set(GUI.InvHard,'enable','on','value',InvertHardcopy==1);
                        Clr=1;
                    case 3 % ALWAYS - No White Background & Black Axes
                        set(GUI.Color,'enable','off','value',1);
                        set(GUI.InvHard,'enable','off','value',0);
                        Clr=1;
                end
                if PL{PrtID,6}==0
                    set(GUI.PLabelTxt,'enable','off')
                    set(GUI.PLabels,'enable','off','backgroundcolor',XX.Clr.LightGray)
                else
                    set(GUI.PLabelTxt,'enable','on')
                    set(GUI.PLabels,'enable','on','backgroundcolor',XX.Clr.White)
                end
            case 'PageLabels'
                PageLabels = get(GUI.PLabels,'value');
            case 'Options'
                % call windows printer setup dialog ...
                print -dsetup
                % bring md_print dialog back to front ...
                figure(fig)

            case 'Painters'
                Method = update_renderer(PL{PrtID,1},GUI,1,DPI);
            case 'Zbuffer'
                Method = update_renderer(PL{PrtID,1},GUI,2,DPI);
            case 'OpenGL'
                Method = update_renderer(PL{PrtID,1},GUI,3,DPI);
            case 'DPI'
                X=eval(get(GUI.Resol,'string'),'NaN');
                if isnumeric(X) && isequal(size(X),[1 1]) && (round(X)==X)
                    if X<50
                        DPI=50;
                    elseif X>2400
                        DPI=2400;
                    else
                        DPI=X;
                    end
                end
                set(GUI.Resol,'string',num2str(DPI));

            case 'Color'
                Clr=get(GUI.Color,'value');
            case 'InvertHardcopy'
                InvertHardcopy=get(GUI.InvHard,'value');
        end
    end
end
if ishandle(fig)
    delete(fig);
end

Settings.PrtID=PrtID;
Settings.Method=Method;
Settings.DPI=DPI;
Settings.Color=Clr;
Settings.InvertHardcopy=InvertHardcopy;
Settings.PageLabels=PageLabels;

if Cancel
    Settings.PrtID=0;
end

function add_bookmark(fname,bookmark_text,append)
% Adds a bookmark to the temporary EPS file after %%EndPageSetup
% Derived from BSD licensed export_fig by Oliver Woodford from MATLAB Central
% Based on original idea by Petr Nechaev

% Read in the file
fh = fopen(fname, 'r');
if fh == -1
    error('File %s not found.', fname)
end
try
    fstrm = fread(fh, '*char')';
catch Ex
    fclose(fh);
    rethrow(Ex)
end
fclose(fh);

if nargin<3 || append
    % Include standard pdfmark prolog to maximize compatibility
    fstrm = strrep(fstrm, '%%BeginProlog', sprintf('%%%%BeginProlog\n/pdfmark where {pop} {userdict /pdfmark /cleartomark load put} ifelse'));
end
pages = findstr(fstrm, '%%EndPageSetup');
lastpage = pages(end);
% Add page bookmark
fstrm = [fstrm(1:lastpage-1) strrep(fstrm(lastpage:end), '%%EndPageSetup', sprintf('%%%%EndPageSetup\n[ /Title (%s) /OUT pdfmark',bookmark_text))];

% Write out the updated file
fh = fopen(fname, 'w');
if fh == -1
    error('Unable to open %s for writing.', fname)
end
try
    fwrite(fh, fstrm, 'char*1');
catch Ex
    fclose(fh);
    rethrow(Ex)
end
fclose(fh);

function Method = update_renderer(Type,GUI,Method,DPI)
XX=xx_constants;
switch Type
    case -1 % painters/zbuffer irrelevant
        set(GUI.Painter  ,'value',1,'enable','off')
        set(GUI.ZBuf     ,'value',0,'enable','off')
        set(GUI.OpenGL   ,'value',0,'enable','off')
        set(GUI.PrintMeth,'enable','off')
        Method=1;
    case 0 % cannot be combined with painters, e.g. bitmap
        set(GUI.Painter  ,'value',0,'enable','off')
        set(GUI.ZBuf     ,'value',0,'enable','on')
        set(GUI.OpenGL   ,'value',0,'enable','on')
        set(GUI.PrintMeth,'enable','on')
        if Method==1 || Method==2
            Method = 2;
            if ~isnumeric(GUI.ZBuf) % HG2 mode does not support ZBuffer printing
                Method = 3;
                set(GUI.ZBuf,'enable','off')
                set(GUI.OpenGL,'value',1)
            else
                set(GUI.ZBuf,'value',1)
            end
        else
            set(GUI.OpenGL,'value',1)
        end
    otherwise
        set(GUI.Painter,'value',0,'enable','on')
        set(GUI.ZBuf   ,'value',0,'enable','on')
        set(GUI.OpenGL ,'value',0,'enable','on')
        set(GUI.PrintMeth,'enable','on')
        if ~isnumeric(GUI.ZBuf) % HG2 mode does not support ZBuffer printing
            set(GUI.ZBuf,'enable','off')
            if Method==2
                Method = 3;
            end
        end
        switch Method
            case 1
                set(GUI.Painter,'value',1)
            case 2
                set(GUI.ZBuf   ,'value',1)
            case 3
                set(GUI.OpenGL ,'value',1)
        end
end
if Method==1
    set(GUI.Resol,'backgroundcolor',XX.Clr.LightGray,'enable','off')
    set(GUI.ResolTxt,'enable','off')
else
    set(GUI.Resol,'backgroundcolor',XX.Clr.White,'string',num2str(DPI),'enable','on')
    set(GUI.ResolTxt,'enable','on')
end
