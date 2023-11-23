function ifig=qp_showabout(qpversion,style,qpdate)
%QP_SHOWABOUT Show QuickPlot about window.

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

ifig=findall(0,'tag','AboutQP');
if ~isempty(ifig)
    figure(ifig);
    return
end
ssz=qp_getscreen;
pos(3)=300;
pos(4)=150;
pos(1)=ssz(1)+ssz(3)/2-pos(3)/2;
pos(2)=ssz(2)+ssz(4)/2-pos(4)/2;
%
switch style
    case 'matlab'
        name = 'About MATLAB';
    otherwise
        name = 'About Delft3D-QUICKPLOT';
end
ifig=qp_uifigure(name,'closereq','AboutQP',pos,'');
set(ifig,'visible','on')

vers = qpversion;
if vers(1)=='v'
    vers = ['Version ' vers(2:end)];
end
switch style
    case 'matlab'
        uicontrol('Parent',ifig, ...
            'Callback','closereq', ...
            'Position',[0 100 300 40], ...
            'Style','text', ...
            'horizontalalignment','center', ...
            'String',['Deltares Delft3D-QUICKPLOT' char(10) vers]);
       
        uicontrol('Parent',ifig, ...
            'Callback','closereq', ...
            'Position',[0 70 300 20], ...
            'Style','text', ...
            'horizontalalignment','center', ...
            'String',['Created using MATLAB ' version]);

        try
           % this does not yet work in R13 compiler of June 2002
           v  = ver;
           im = strmatch('MATLAB',{v.Name},'exact');
           dm = datevec(v(im(1)).Date);
        catch
           dm = [2002 6 20 0 0 0];
        end
        uicontrol('Parent',ifig, ...
            'Callback','closereq', ...
            'Position',[0 40 300 20], ...
            'Style','text', ...
            'horizontalalignment','center', ...
            'String',sprintf('MATLAB. (c) 1984 - %i The MathWorks, Inc.',dm(1)));
        
    otherwise
        uicontrol('Parent',ifig, ...
            'Callback','closereq', ...
            'Position',[0 100 300 20], ...
            'Style','text', ...
            'horizontalalignment','center', ...
            'String','Deltares');
        
        uicontrol('Parent',ifig, ...
            'Callback','closereq', ...
            'Position',[0 70 300 20], ...
            'Style','text', ...
            'horizontalalignment','center', ...
            'String','Delft3D-QUICKPLOT');
        
        if strcmp(qpdate,'<CREATIONDATE>')
            voffset = 20;
            str = {vers,['running in MATLAB ' version]};
        else
            voffset = 20;
            str = {vers,qpdate};
        end
        uicontrol('Parent',ifig, ...
            'Callback','closereq', ...
            'Position',[0 voffset 300 60-voffset], ...
            'Style','text', ...
            'horizontalalignment','center', ...
            'String',str);
end

uicontrol('Parent',ifig, ...
    'Callback','closereq', ...
    'Position',[250 10 40 20], ...
    'String','OK');

drawnow;
set(ifig,'windowstyle','modal')
