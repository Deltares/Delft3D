function writelog(logfile,logtype,varargin)
%WRITELOG Write QuickPlot logfile or MATLAB script.

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

if logtype<0
    %
    % Output command directly
    %
    logtype=-logtype;
    switch logtype
        case {2,6}
            Format='%s\n';
            Args{1}=varargin{1};
        case 4
            Format='%s';
            Args{1}=varargin{1};
        otherwise
            return
    end
else
    %
    % Combine arguments to command
    %
    for i=1:length(varargin)
        if i==1
            switch logtype
                case {1,3,5}
                    Format='%s';
                case {2,4,6}
                    Format='d3d_qp(''%s''';
            end
            Args=varargin(1);
        elseif ischar(varargin{i})
            switch logtype
                case {1,3,5}
                    Format=[Format ' ''%s'''];
                case {2,4,6}
                    Format=[Format ',''%s'''];
            end
            Args{end+1}=strrep(varargin{i},'''','''''');
        elseif isequal(size(varargin{i}),[1 1])
            switch logtype
                case {1,3,5}
                    Format=[Format ' %g'];
                case {2,4,6}
                    Format=[Format ',%g'];
            end
            Args{end+1}=varargin{i};
        else
            switch logtype
                case {1,3,5}
                    Format=[Format ' %s'];
                case {2,4,6}
                    Format=[Format ',%s'];
            end
            Args{end+1}=vec2str(varargin{i},'noones');
        end
    end
    switch logtype
        case {1,5}
            Format=[Format '\n'];
        case {2,6}
            Format=[Format ')\n'];
        case 3
            % nothing to add
        case 4
            Format=[Format ')'];
    end
end
if logtype==3 | logtype==4
    ui_message('',Format,Args{:})
elseif logtype==5 | logtype==6
    fprintf(1,Format,Args{:});
else
    fprintf(logfile,Format,Args{:});
end
