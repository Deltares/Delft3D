function Out = qpfile(DataRes)
%QPFILE Get information about the active file in QuickPlot.
%   FILE = QPFILE returns a structure containing for the data file
%   currently selected in QuickPlot. The structure may be used to call
%   QPREAD to read data.

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

if isempty(gcbf) || ~strcmp(get(gcbf,'tag'),'Delft3D-QUICKPLOT')
    mfig = findobj(allchild(0),'flat','tag','Delft3D-QUICKPLOT');
else
    mfig = gcbf;
end

Handle_FileList = findobj(mfig,'tag','selectfile');
File = get(Handle_FileList,'userdata');
NrInList = get(Handle_FileList,'value');
if nargin>0
    Str = get(Handle_FileList,'string');
    if isempty(File)
        i = 1;
        Str = {};
    else
        i = length(File)+1;
    end
    if ischar(DataRes) && strcmpi(DataRes,'unwrapped')
        if isempty(File)
            Out = -1;
        else
            Out = File(NrInList);
            Out = qp_unwrapfi(Out);
        end
        return
    elseif ischar(DataRes) && strcmpi(DataRes,'name')
        if isempty(File)
            Out = '';
        else
            Out = File(NrInList).Name;
        end
        return
    elseif isa(DataRes,'qp_data_resource')
        File(i).QPF = 1;
        File(i).Name = DataRes.Key.Name;
        File(i).Data = DataRes;
        File(i).FileType = 'QP Data Resource Object';
        File(i).Options = 0;
        File(i).Otherargs = {};
    elseif isfield(DataRes,'QPF') && ...
            isfield(DataRes,'Name') && ...
            isfield(DataRes,'Data') && ...
            isfield(DataRes,'FileType') && ...
            isfield(DataRes,'Options') && ...
            isfield(DataRes,'Otherargs')
        try
            File(i) = DataRes;
        catch
            error('Please specify Data Resource Object or QPFOPEN file structure.')
        end
    elseif isfield(DataRes,'FileName') && ...
            isfield(DataRes,'FileType')
        try
            dummy = qp_getdata(DataRes,'domains');
        catch
            error('Please specify Data Resource Object or QPFOPEN file structure.')
        end
        File(i).QPF = 1;
        File(i).Name = DataRes.FileName;
        File(i).Data = DataRes;
        if isequal(lower(DataRes.FileType),'nefis')
            File(i).FileType = DataRes.SubType;
        else
            File(i).FileType = DataRes.FileType;
        end
        File(i).Options = 0;
        File(i).Otherargs = {};
    else
        error('Please specify Data Resource Object or QPFOPEN file structure.')
    end
    Active=[1 1 1];
    Str{i} = abbrevfn(File(i).Name,60);
    set(Handle_FileList,'userdata',File,'string',Str,'value',i,'enable','on','backgroundcolor',Active)
    d3d_qp selectfile*
else
    if isempty(File)
        Out = -1;
    else
        Out = File(NrInList);
    end
end
