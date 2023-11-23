function Prop=qp_datafield_name2prop(FI,Domain,Name)
%QP_DATAFIELD_NAME2PROP Convert data field string to structure.
%
%   PropStruct = qp_datafield_name2prop(FileInfo,Domain,DataFieldName)

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

Fcn = qp_file2function(FI);
if isempty(Fcn)
    return
end
%
FI = qp_unwrapfi(FI);
if isequal(Domain,'NEW_RESOURCE_CALL')
    DataProps = feval(Fcn,FI,[],'quantities');
else
    DataProps = feval(Fcn,FI,Domain);
end
%
Names = {DataProps.Name};
[Id,IdAll] = ustrcmpi(Name,Names);
if Id<0
    if isempty(IdAll)
        error('Unknown data field: %s.',Name)
    else
        message = cat(2,sprintf('Datafield name ''%s'' not unique, matching:\n',Name), ...
            sprintf('''%s''\n',Names{IdAll}));
        message(end)=[];
        error(message)
    end
end
Prop = DataProps(Id);
