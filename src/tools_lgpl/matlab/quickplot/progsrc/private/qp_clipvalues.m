function data = qp_clipvalues(data, Ops)
%QP_CLIPVALUES Clip values in a QUICKPLOT data structure.
%   DATA = QP_CLIPVALUES(DATA, OPS)
%   Clips the DATA structure based on the options set in OPS.
%   This includes:
%    * Clipping of DATA>Val, DATA.XDamVal and DATA.YDamVal based on the
%    content of OPS.clippingvalues.
%    * Clipping of DATA.X or DATA.XYZ(:,1) based on OPS.xclipping.
%    * Clipping of DATA.Y or DATA.XYZ(:,2) based on OPS.yclipping.
%   Clipping is implemented by setting the selected values or coordinates
%   to 0. The listed OPS fields may not exist or they must be empty or
%   compatible with REALSET.
%
%   See also: REALSET.

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

clippingspatial = 0;
for clipi = 1:4
    switch clipi
        case 1
            % clipping based on components gives all kinds of practical problems
            % (e.g. rotated components), so not implemented.
            fld = {'Val','XDamVal','YDamVal'};
            clp = 'clippingvalues';
        case 2
            fld = {'X'};
            clp = 'xclipping';
        case 3
            fld = {'Y'};
            clp = 'yclipping';
        case 4
            fld = {'Z'};
            clp = 'zclipping';
    end
    if isfield(Ops, clp)
        clippingvals = Ops.(clp);
    else
        clippingvals = [];
    end
    %
    if ~isempty(clippingvals)
        if clipi > 1
            clippingspatial = 1;
        end
        for f = 1:length(fld)
            fldf = fld{f};
            if isfield(data, fldf)
                for d = 1:length(data)
                    val = getfield(data, {d}, fldf);
                    data = setfield(data, {d}, fldf, []);
                    if isnumeric(clippingvals)
                        check = logical(ismember(val, clippingvals));
                        if any(check(:))
                            if ~isa(val, 'double') && ~isa(val, 'single')
                                val = double(val);
                            end
                            val(check) = NaN;
                        end
                    else
                        val = realset(clippingvals, val);
                    end
                    data = setfield(data, {d}, fldf, val);
                end
            end
        end
    end
end
if clippingspatial
    if isfield(data, 'XYZ')
        for d = length(data):-1:1
            val = data(d).XYZ;
            szVal = size(val);
            val = reshape(val, prod(szVal(1:end-1)), szVal(end));
            for clipi = 2:4
                switch clipi
                    case 2 % X
                        dim = 1;
                        clippingvals = Ops.xclipping;
                    case 3 % Y
                        dim = 2;
                        clippingvals = Ops.yclipping;
                    case 4 % Z
                        dim = 3;
                        clippingvals = Ops.zclipping;
                end
                if isempty(clippingvals) || size(val,2) < dim
                    % nothing
                elseif isnumeric(clippingvals)
                    val(logical(ismember(val(:,dim), clippingvals)), :) = NaN;
                else
                    val(:,dim) = realset(clippingvals, val(:,dim));
                    val(isnan(val(:,dim)),:) = NaN;
                end
            end
            val = reshape(val, szVal);
            data(d).XYZ = val;
        end
    elseif isfield(data,'FaceNodeConnect') && isfield(data,'ValLocation') && strcmp(data.ValLocation,'FACE')
        % to be implemented
    end
end