function qp_update_evalhistmenu(uim,cmd,evalcmd)
%QP_UPDATE_EVALHISTMENU Update list of recent QuickPlot macro commands.

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

switch cmd
    case 'initialize'
        for i = 1:99
            evalcmd = qp_settings(sprintf('EvalCmd%2.2i',i),'');
            if isempty(evalcmd)
                break
            else
                localadd(uim,evalcmd)
            end
        end
    case 'add'
        localadd(uim,evalcmd)
end


function localadd(uim,evalcmd)
evalcmds = get(uim,'userdata');
included = strmatch(evalcmd,evalcmds,'exact');
if isempty(included)
    if length(evalcmd)>50
        evalcmdstr = [evalcmd(1:50) '...'];
    else
        evalcmdstr = evalcmd;
    end
    uimenu('parent',uim, ...
        'label',evalcmdstr, ...
        'userdata',evalcmd, ...
        'callback','d3d_qp evalhistmenu')
    evalcmds{end+1}=evalcmd;
    set(uim,'userdata',evalcmds)
end
