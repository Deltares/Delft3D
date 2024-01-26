function qp_updaterecentfiles(mfig)
%QP_UPDATERECENTFILES Update list of recently opened data files in QP main dialog.

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

RecentFiles = cell(9,1);
for i = 1:9
    RecentFiles{i} = qp_settings(sprintf('File%2.2i',i),'');
end

close = findall(mfig,'type','uimenu','tag','close');
filemenu = get(close,'parent');

h = zeros(9,1);
for i = 1:9
    h1 = findobj(filemenu,'tag',sprintf('File%2.2i',i));
    if isempty(h1)
        break
    else
        h(i) = h1;
    end
end

children = get(filemenu,'children');
children(ismember(children,h))=[];

for i = 1:9
    ThisFile = RecentFiles{i};
    if isempty(ThisFile)
        if h(i)~=0
            set(h(i),'visible','off')
        else
            h(i) = uimenu('parent',filemenu, ...
                'label','XXX', ...
                'tag',sprintf('File%2.2i',i), ...
                'callback','', ...
                'visible','off');
            if i==1
                set(h(i),'separator','on')
            end
        end
    else
        quotes = strfind(ThisFile,'''');
        AbbFile = abbrevfn(ThisFile(quotes(1)+1:quotes(2)-1));
        label = sprintf('&%i %s',i,AbbFile);
        callb = ['d3d_qp openfile ' ThisFile];
        if h(i)==0
            h(i) = uimenu('parent',filemenu, ...
                'label',label, ...
                'tag',sprintf('File%2.2i',i), ...
                'callback',callb, ...
                'visible','on');
            if i==1
                set(h(i),'separator','on')
            end
        else
            set(h(i), 'label',label, ...
                'callback',callb, ...
                'visible','on')
        end
    end
end

if isempty(gcbo)
    h(h==0)=[];
    children = [children(1);flipud(h);children(2:end)];
    %
    % The following command destroys the whole menu in MATLAB R13 on Linux when
    % called from a callback. Therefore, this code is called only once. The
    % menu items are kept in place by hiding and showing them.
    %
    set(filemenu,'children',children)
end
