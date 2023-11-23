function q = compute_percentile(x, p, base)
%COMPUTE_PERCENTILE Determines percentiles or quantiles of a data set.
%   Y = COMPUTE_PERCENTILE(X,P,B) returns B-tiles of the values in X. B is
%   the base which is 100 by default for percentiles, but can be set to 1
%   for quantiles. The following wording applies B=100. P is a scalar or a
%   vector of percent values.  When X is a vector, Y is the same size as P,
%   and Y(i) contains the P(i)-th percentile, otherwise the i-th row of Y
%   contains the P(i)-th percentiles of each column of X.
%
%   Percentiles are specified using percentages, from 0 to 100.  For an N
%   element vector X, COMPUTE_PERCENTILE determines percentiles as follows:
%      1) The sorted values in X are taken as the 100*(0.5/N), 100*(1.5/N),
%         ..., 100*((N-0.5)/N) percentiles.
%      2) Linear interpolation is used to compute percentiles for percent
%         values between 100*(0.5/N) and 100*((N-0.5)/N)
%      3) The minimum or maximum values in X are assigned to percentiles
%         for percent values outside that range.
%
%   COMPUTE_PERCENTILE treats NaNs as missing values, and removes them.
%
%   Examples:
%      y = compute_perctile(x,50); % the median of x
%      y = compute_perctile(x,0.5,1); % the median of x
%
%   See also PRCTILE, QUANTILE (Statistics Toolbox).

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

if nargin<3
    base = 100;
end
if ~isnumeric(x)
    error('First argument must be a numeric vector or matrix')
end
if ~isnumeric(p) || ~isvector(p) || any(p<0) || any(p>base)
    error ('Second argument must be a numeric vector with values between 0 and %g',base)
end

% convert source array x into a matrix
szx = size (x);
x = reshape (x, szx(1), []);

% sort values along first dimension
x = sort(x,1);

% count number of number of non-NaN values
n = sum(~isnan(x),1);

% make sure that quantile is column vector
p = p(:)/base;

% determine number of rows of original data (x), number of locations (n)
% and percentile data (p)
len_p = length(p);
len_n = size(x,2);
len_x = size(x,1);

% determine fractional index of the quantiles
i = repmat(n,len_p,1);
i_float = min(p * n + 0.5, i);

% convert fraction index into base index and fraction
i = floor(i_float);
a = i_float - i;

% clip indices below 1
z = i==0;
i(z) = 1;
a(z) = 0;

% convert column index into matrix index
I = i + repmat((0:(len_n-1))*len_x,len_p,1);

% obtain base values
q = x(I);

% increment base values in case fractional part is non-zero
nza = a>0;
Inza = I(nza);
q(nza) = q(nza) + a(nza) .* (x(Inza+1) - x(Inza));

% unwrap dimensions to full x shape
q = reshape(q,[len_p, szx(2:end)]);
