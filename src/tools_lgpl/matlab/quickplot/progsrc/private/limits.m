function varargout = limits(h, limtype)
%LIMITS Determine real x, y, z and color limits.
%   [LIM1, LIM2, LIM3, LIM4] = LIMITS(H, LIMSTR)
%   returns the actual upper and lower limits of the x, y, z or color
%   values set for the objects specified by H. The order in which the
%   limits are returned and the number of dimensions for which the limits
%   are determined is determined by the (order of) occurrence of the
%   characters x, y, z and c in the argument LIMSTR.
%
%   [LIM1, LIM2, LIM3, LIM4] = LIMITS(AX, LIMSTR)
%   returns the limits for the children of the selected axes object.
%
%   Note that this routine returns the actual limits derived from the
%   objects, not the MATLAB derived or user specified limits set in the
%   axes object. Use the MATLAB standard routines XLIM, YLIM, and ZLIM for
%   that.
%
%   See also XLIM, YLIM, ZLIM.

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

if length(h) == 1 && isequal(get(h, 'type'), 'axes')
    ch = get(h, 'children');
    xlab = get(h, 'xlabel');
    ylab = get(h, 'ylabel');
    zlab = get(h, 'zlabel');
    titl = get(h, 'title');
    ch = setdiff(ch, [xlab ylab zlab titl]);
else
    ch = h;
end
ch = ch(:);
%
i = 1;
while i <= length(ch)
    if strcmp(get(ch(i), 'type'), 'hggroup')
        chd = get(ch(i), 'children');
        ch(i,:) = [];
        ch = cat(1, ch, chd);
    else
        i = i+1;
    end
end
%
% reduce limit specification to just the characters c, x, y and z.
%
dlim = lower(limtype);
dlim(~ismember(dlim, 'cxyz')) = [];
if length(dlim) ~= nargout
    if length(dlim) ~= 1 || nargout ~= 0
        error('Number of output arguments does not match the number of quantities for which limits are requested.')
    end
end
%
ndims = length(dlim);
if isempty(ch)
    varargout = repmat({[0 1]}, [1, ndims]);
    return
else
    lim = repmat({[inf -inf]}, [1, ndims]);
end
%
for i = 1:length(ch)
    chtype = get(ch(i), 'type');
    if strcmp(chtype, 'text')
        p = get(ch(i), 'position');
        for j = 1:ndims
            switch dlim(j)
                case 'x'
                    lim{j}(1) = min(lim{j}(1), p(1));
                    lim{j}(2) = max(lim{j}(2), p(1));
                case 'y'
                    lim{j}(1) = min(lim{j}(1), p(2));
                    lim{j}(2) = max(lim{j}(2), p(2));
                case 'z'
                    lim{j}(1) = min(lim{j}(1), p(3));
                    lim{j}(2) = max(lim{j}(2), p(3));
            end
        end
    else
        for j = 1:ndims
            switch dlim(j)
                case 'c'
                    if ~strcmp(chtype, 'line')
                        c = get(ch(i), 'cdata');
                        cmap = get(ch(i), 'cdatamapping');
                        if (size(c, 3) == 1) && strcmp(cmap, 'scaled') && ~isempty(c)
                            lim{j}(1) = min(lim{j}(1), min(c(:)));
                            lim{j}(2) = max(lim{j}(2), max(c(:)));
                        end
                    end
                case {'x','y','z'}
                    if ~strcmp(chtype, 'image')
                        x = get(ch(i), [dlim(j) 'data']);
                        if ~isempty(x)
                            lim{j}(1) = min(lim{j}(1), min(x(:)));
                            lim{j}(2) = max(lim{j}(2), max(x(:)));
                        end
                    end
            end
        end
    end
end
varargout = lim;
