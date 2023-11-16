function [rout,g,b] = hls2rgb(h,l,s)
%HLS2RGB Convert hue-lightness-saturation to red-green-blue colors.
%   H = HLS2RGB(M) converts an HLS color map to an RGB color map.
%   Hue, lightness, and saturation values scaled between 0 and 1.
%
%   See RGB2HLS

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
        if isa(h, 'uint8')
            h = double(h) / 255;
        elseif isa(h, 'uint16')
            h = double(h) / 65535;
        end
    case 3
        if isa(h, 'uint8')
            h = double(h) / 255;
        elseif isa(h, 'uint16')
            h = double(h) / 65535;
        end

        if isa(l, 'uint8')
            l = double(l) / 255;
        elseif isa(l, 'uint16')
            l = double(l) / 65535;
        end

        if isa(s, 'uint8')
            s = double(s) / 255;
        elseif isa(s, 'uint16')
            s = double(s) / 65535;
        end

    otherwise
        error('Wrong number of input arguments.')
end

threeD = (ndims(h)==3); % Determine if input includes a 3-D array

if threeD
    l = h(:,:,2); s = h(:,:,3); h = h(:,:,1);
    siz = size(h);
    h = h(:); l = l(:); s = s(:);
elseif nargin==1
    l = h(:,2); s = h(:,3); h = h(:,1);
    siz = size(h);
else
    if ~isequal(size(h),size(l),size(s))
        error('H,L,S must all be the same size.')
    end
    siz = size(h);
    h = h(:); l = l(:); s = s(:);
end

ma=l.*(1+s);
i=l>1/2;
ma(i)=l(i)+s(i)-l(i).*s(i);
mi=2*l-ma;

mami=ma-mi;

r=zeros(size(l));
g=r;
b=r;

i1=h<1/6;
r(i1) = mi(i1)+6*mami(i1).*h(i1);
i2=h<1/2;
I2=i2 & ~i1;
r(I2) = ma(I2);
i3=h<2/3;
I3=i3 & ~i2;
r(I3)=mi(I3)+6*mami(I3).*(2/3-h(I3));
I4=~i3;
r(I4)=mi(I4);

i1=h<1/3;
g(i1) = mi(i1);
i2=h<1/2;
I2=i2 & ~i1;
g(I2) = mi(I2)+6*mami(I2).*(h(I2)-1/3);
i3=h<5/6;
I3=i3 & ~i2;
g(I3)=ma(I3);
I4=~i3;
g(I4)=mi(I4)+6*mami(I4).*(1-h(I4));

i1=(h<1/6) | (h>=5/6);
b(i1) = ma(i1);
i2=h<1/3;
I2=i2 & ~i1;
b(I2) = mi(I2)+6*mami(I2).*(1/3-h(I2));
i3=h<2/3;
I3=i3 & ~i2;
b(I3)=mi(I3);
I4=~i3 & ~i1;
b(I4)=mi(I4)+6*mami(I4).*(h(I4)-2/3);


r=min(1,max(0,r));
g=min(1,max(0,g));
b=min(1,max(0,b));


if nargout<=1
    if (threeD | nargin==3)
        rout = zeros([siz,3]);
        rout(:,:,1) = reshape(r,siz);
        rout(:,:,2) = reshape(g,siz);
        rout(:,:,3) = reshape(b,siz);
    else
        rout = [r g b];
    end
else
    rout = reshape(r,siz);
    g = reshape(g,siz);
    b = reshape(b,siz);
end
