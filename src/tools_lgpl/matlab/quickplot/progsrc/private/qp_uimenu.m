function H=qp_uimenu(hP,mainlabel,submenu)
%QP_UIMENU Create a new menu with submenus.

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

if nargin==3
    m0=uimenu('Label',mainlabel, ...
        'Enable','on', ...
        'Serializable','off', ...
        'Parent',hP);
else
    m0=hP;
    submenu=mainlabel;
end
offon={'off','on'};
nsub=size(submenu,1);
H=zeros(1+nsub,1);
H(1)=m0;
for i=1:nsub
    cmd=submenu{i,1};
    label=submenu{i,2};
    vis=offon{1+submenu{i,3}};
    enab=offon{1+submenu{i,4}};
    sep=offon{1+submenu{i,5}};
    if isempty(cmd)
        callb='';
        tag=label;
    else
        callb=cat(2,'d3d_qp ',cmd);
        tag=cmd;
    end
    H(i+1)=uimenu('Label',label, ...
        'Callback',callb, ...
        'Enable',enab, ...
        'Tag',tag, ...
        'Visible',vis, ...
        'Separator',sep, ...
        'Serializable','off', ...
        'Parent',m0);
    if size(submenu,2)>=6 & ~isempty(submenu{i,6})
        set(H(i+1),'Accelerator',submenu{i,6})
    end
end
