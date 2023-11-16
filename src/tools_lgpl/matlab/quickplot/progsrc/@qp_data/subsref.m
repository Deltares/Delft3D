function B1 = subsref(A,S)
%SUBSREF Subscripted reference qp_data object.

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

final_si = false;
nsubscripts = length(S);
si=1;
%
if nargout==0
    B = 'QuickPlot.NoOutput';
end
%
while si<=nsubscripts
    si_start=si;
    if ~strcmp(S(si).type,'.')
        error('Invalid subscript type')
    elseif ~ischar(S(si).subs)
        error('Subscript string expected')
    end
    %
    cmd = lower(S(si).subs);
    switch cmd
        case 'unit'
            if si+1>nsubscripts
                error('Missing unit string');
            elseif ~isequal(S(si+1).type,'()') ...
                    || length(S(si+1).subs)~=1 ...
                    || ~ischar(S(si+1).subs{1})
                error('Invalid unit syntax: OBJECT.Unit(''unit specification'')')
            end
            B = A;
            RequestedUnit = S(si+1).subs{1};
            if ~isequal(RequestedUnit,A.Unit)
                a = qp_unitconversion(RequestedUnit,A.Unit);
                if ischar(a)
                    error(a)
                else
                    for i = 1:length(B.Value)
                        B.Value(i).Data = qp_unitconversion(RequestedUnit,A.Unit,B.Value(i).Data);
                    end
                    B.Unit = RequestedUnit;
                end
            end
            si = si+2;
        case 'name'
            B = A.Name;
            si = si+1;
            final_si = true;
        case 'grid'
            B = A.Grid;
            si = si+1;
            final_si = true;
        case 'data'
            B = A.Value;
            si = si+1;
            final_si = true;
        case 'dimension'
            B = A.Dimensions;
            si = si+1;
            final_si = true;
        case 'plotstyles'
            if nargout==0
                plotstyles(A);
            else
                B = plotstyles(A);
            end
            si = si+1;
            final_si = true;
        case 'plotoptions'
            if si+1>nsubscripts
                error('Missing plot style string');
            elseif ~isequal(S(si+1).type,'()') ...
                    || length(S(si+1).subs)~=1 ...
                    || ~ischar(S(si+1).subs{1})
                error('Invalid plotoptions syntax: OBJECT.PlotOptions(''PlotStyle'')')
            end
            %
            if nargout==0
                plotoptions(A,S(si+1).subs{1});
            else
                B = plotoptions(A,S(si+1).subs{1});
            end
            si = si+2;
            final_si = true;
        case 'plot'
            if si+1>nsubscripts
                error('Missing plot style string');
            elseif ~isequal(S(si+1).type,'()') ...
                    || isempty(S(si+1).subs) ...
                    || ~ischar(S(si+1).subs{1})
                error('Invalid plot syntax: OBJECT.Plot(''plot style'')')
            end
            %
            if nargout==0
                plot(A,S(si+1).subs{:});
            else
                B = plot(A,S(si+1).subs{:});
            end
            si = si+2;
            final_si = true;
        otherwise
            %
            % Throw error
            %
            if si == si_start
                if isfield(A,cmd)
                    error('Direct access to fields has been blocked');
                else
                    error('Unknown field or function ''%s''',cmd)
                end
            end
    end
    if final_si
        if si <= nsubscripts
            B = subsref(B,S(si:end));
            si = nsubscripts+1;
        end
    else
        if si_start >= si
            error('Missing si increment in while loop!')
        end
        A = B;
    end
end

if ~isequal(B,'QuickPlot.NoOutput')
    B1 = B;
end
