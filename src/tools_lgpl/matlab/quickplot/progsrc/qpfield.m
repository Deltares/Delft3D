function [DomainNr,Props,subf] = qpfield
%QPFIELD Get information about the active quantity/field in QuickPlot.
%   [DOMAIN,PROP,SUBFIELD] = QPFIELD returns the domain number, the field
%   property structure and a cell array containing the optional subfield
%   index.
%
%   See also: QPFILE.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2025 Stichting Deltares.                                     
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

if isempty(gcbf) || ~strcmp(get(gcbf,'tag'),'Delft3D-QUICKPLOT')
    mfig = findobj(allchild(0),'flat','tag','Delft3D-QUICKPLOT');
else
    mfig = gcbf;
end

Handle_Domain = findobj(mfig,'tag','selectdomain');
DomainNr = get(Handle_Domain,'value');

datafields = findobj(mfig,'tag','selectfield');
Props = get(datafields,'userdata');
subf = {};
if isempty(Props)
    return
end

fld = get(datafields,'value');
Props = Props(fld);

UD = getappdata(mfig,'QPHandles');
MW = UD.MainWin;
if strcmp(get(MW.SubFld,'enable'),'on')
    subf = {get(MW.SubFld,'value')};
end
