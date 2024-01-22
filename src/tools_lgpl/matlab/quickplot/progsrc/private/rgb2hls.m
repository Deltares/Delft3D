function [hout,l,s] = rgb2hls(r,g,b)
%RGB2HLS Convert red-green-blue colors to hue-lightness-saturation.
%   H = RGB2HLS(M) converts an RGB color map to an HLS color map.
%   Hue, lightness, and saturation values scaled between 0 and 1.
%
%   See HLS2RGB, RGB2HSV

%   Based on RGB2HSV by Cleve Moler, The MathWorks

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

switch nargin
    case 1
        if isa(r, 'uint8')
            r = double(r) / 255;
        elseif isa(r, 'uint16')
            r = double(r) / 65535;
        end
    case 3
        if isa(r, 'uint8')
            r = double(r) / 255;
        elseif isa(r, 'uint16')
            r = double(r) / 65535;
        end

        if isa(g, 'uint8')
            g = double(g) / 255;
        elseif isa(g, 'uint16')
            g = double(g) / 65535;
        end

        if isa(b, 'uint8')
            b = double(b) / 255;
        elseif isa(b, 'uint16')
            b = double(b) / 65535;
        end

    otherwise
        error('Wrong number of input arguments.')
end

threeD = (ndims(r)==3); % Determine if input includes a 3-D array

if threeD
    g = r(:,:,2); b = r(:,:,3); r = r(:,:,1);
    siz = size(r);
    r = r(:); g = g(:); b = b(:);
elseif nargin==1
    g = r(:,2); b = r(:,3); r = r(:,1);
    siz = size(r);
else
    if ~isequal(size(r),size(g),size(b))
        error('R,G,B must all be the same size.')
    end
    siz = size(r);
    r = r(:); g = g(:); b = b(:);
end

cmax = max([r g b],[],2);
cmin = min([r g b],[],2);

l = (cmax+cmin)/2;
chrom = cmax~=cmin; % achrom r==g==b -> gray

s = zeros(size(cmax));
h = s;

mami=cmax-cmin;

i = chrom & (l<=1/2);
s(i)=mami(i) ./ (cmax(i)+cmin(i));
i = chrom & (l>1/2);
s(i)=mami(i) ./ (2-cmax(i)-cmin(i));

rd = cmax-r;
gd = cmax-g;
bd = cmax-b;

i = chrom & r==cmax;
h(i)=2*mami(i)+bd(i)-gd(i);
i = chrom & g==cmax;
h(i)=4*mami(i)+rd(i)-bd(i);
i = chrom & b==cmax;
h(i)=6*mami(i)+gd(i)-rd(i);

h(chrom)=mod(h(chrom)/6./mami(chrom),1);

if nargout<=1
    if (threeD | nargin==3)
        hout = zeros([siz,3]);
        hout(:,:,1) = reshape(h,siz);
        hout(:,:,2) = reshape(l,siz);
        hout(:,:,3) = reshape(s,siz);
    else
        hout = [h l s];
    end
else
    hout = reshape(h,siz);
    l = reshape(l,siz);
    s = reshape(s,siz);
end
