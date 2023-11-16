function X=interp2cen(varargin)
%INTERP2CEN Interpolate to center.
%      X=INTERP2CEN(x)
%      Interpolates data from cell corners to centers (NM dirs).
%      x is a N x M matrix
%
%      X=INTERP2CEN(x,flag)
%      Interpolates data from cell corners to centers (NM dirs).
%      x is a nTim x N x M x ... matrix

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

Input = varargin;
interptype='mean';
if ischar(varargin{end})
    switch lower(varargin{end})
        case {'mean','max','min'}
            interptype = lower(varargin{end});
            Input(end)=[];
    end
end

switch interptype
    case 'max'
        fcn = @max;
    case 'min'
        fcn = @min;
end

x = Input{1};
switch length(Input)
    case 1
        if isempty(x)
            X=x;
            return
        end
        if ndims(x)==3
            m=2:size(x,2);
            n=2:size(x,3);
            switch interptype
                case 'mean'
                    X(:,m,n)=(x(:,m,n)+x(:,m-1,n)+x(:,m,n-1)+x(:,m-1,n-1))/4;
                case {'max','min'}
                    X=x;
                    X(:,m,n)=feval(fcn,X(:,m,n),x(:,m-1,n));
                    X(:,m,n)=feval(fcn,X(:,m,n),x(:,m,n-1));
                    X(:,m,n)=feval(fcn,X(:,m,n),x(:,m-1,n-1));
            end
            X(:,1,:)=NaN;
            X(:,:,1)=NaN;
        else
            m=2:size(x,1);
            n=2:size(x,2);
            switch interptype
                case 'mean'
                    X(m,n)=(x(m,n)+x(m-1,n)+x(m,n-1)+x(m-1,n-1))/4;
                case {'max','min'}
                    X=x;
                    X(m,n)=feval(fcn,X(m,n),x(m-1,n));
                    X(m,n)=feval(fcn,X(m,n),x(m,n-1));
                    X(m,n)=feval(fcn,X(m,n),x(m-1,n-1));
            end
            X(1,:)=NaN;
            X(:,1)=NaN;
        end
    case 2
        X=x;
        if isempty(x)
            return
        end

        for i=1:ndims(x)
            idx{i}=1:size(x,i);
        end

        idx{2}=2:size(x,2);
        idx{3}=2:size(x,3);
        idd=idx; idd{2}=idx{2}-1;
        switch interptype
            case 'mean'
                X(idx{:})=X(idx{:})+x(idd{:});
                idd{3}=idx{3}-1;
                X(idx{:})=X(idx{:})+x(idd{:});
                idd{2}=idx{2};
                X(idx{:})=X(idx{:})+x(idd{:});
                X(idx{:})=X(idx{:})/4;
            case {'max','min'}
                X(idx{:})=feval(fcn,X(idx{:}),x(idd{:}));
                idd{3}=idx{3}-1;
                X(idx{:})=feval(fcn,X(idx{:}),x(idd{:}));
                idd{2}=idx{2};
                X(idx{:})=feval(fcn,X(idx{:}),x(idd{:}));
        end
        X(:,1,:,:)=NaN;
        X(:,:,1,:)=NaN;
end
