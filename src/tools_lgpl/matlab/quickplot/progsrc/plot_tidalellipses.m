function h = plot_tidalellipses(X,Y,argtype,varargin)
%PLOT_TIDALELLIPSES Plot tidal ellipses on a map.
%   PLOT_TIDALELLIPSES(X,Y,'AP',AMPU,PHIU,AMPV,PHIV) plots tidal ellipses
%   on a map at the X,Y locations based on the amplitudes and phases of the
%   two velocity components. The dimensions of X, Y, amplitude and phase
%   arrays should be identical. For the conversion of amplitudes and phases
%   into ellipse parameters the private AP2EP routine is used. PHIU,PHIV 
%   are in degrees.
%
%   PLOT_TIDALELLIPSES(X,Y,'EP',SEMA,ECC,INC,PHA) plots tidal ellipses
%   on a map at the X,Y locations based on the given by the semi-major
%   axes, eccentricity, inclination, and phase angles. The dimensions of
%   these six arrays should be identical, INC and PHA are in degrees.
%
%   H = PLOT_TIDALELLIPSES(...) returns a handle to the created line
%   object.
%
%   PLOT_TIDALELLIPSES(...,Prop1,Value1,Prop2,Value2,...) sets an
%   additional set of optional property values. Supported properties are:
%      'PlotType'    - sets the plot type of the tidal ellipse to
%
%            'cross'           - two axes of the ellipse
%            'ellipse'         - ellipse
%            'ellipsephase'    - ellipse with reference phase line and
%                                gap to indicate rotation direction.
%            'ellipsephasevec' - ellipse with reference phase line and
%                                vector to indicate rotation direction.
%
%      'PhaseOffset' - sets the phase offset for the reference phase
%                      indicator.
%      'Scale'       - scale factor: plots 1 m/s as specified map distance.
%                      The default scaling is automatic.
%      'Parent'      - sets the parent axes in which the ellipses will be
%                      plotted.
%      'Color'       - sets the line color.
%      'LineStyle'   - sets the line style.
%
%   See also TBA_PLOTELLIPSES.

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

if nargin<7
    error('Too few input arguments.')
end
switch lower(argtype)
    case 'ap'
        % Au, PHIu, Av, PHIv
        [SEMA,  ECC, INC, PHA] = ap2ep(varargin{1:4});
    case 'ep'
        % SEMA, ECC, INC, PHA
        [SEMA,  ECC, INC, PHA] = deal(varargin{1:4});
    otherwise
        error('Invalid argument type: ''ap'' or ''ep''.')
end
%
phase_offset = 0;
scale = NaN;
forward_args = false(size(varargin));
plottype = 'ellipsephase';
%
supported_args = {'Color','LineStyle','Parent','PhaseOffset','PlotType', ...
    'Scale'};
for i = 5:2:nargin-4
    ipar = ustrcmpi(varargin{i},supported_args);
    if ipar<0
        if ischar(varargin{i})
            error('Invalid parameter %s',varargin{i})
        else
            error('Invalid parameter')
        end
    else
        switch supported_args{ipar}
            case 'PhaseOffset'
                phase_offset = varargin{i+1};
            case 'PlotType'
                plottype = varargin{i+1};
            case 'Scale'
                scale = varargin{i+1};
            otherwise
                forward_args(i:i+1) =true;
        end
    end
end
parval = varargin(forward_args);

n = numel(X);
X = reshape(X,[1 n]);
Y = reshape(Y,[1 n]);
SEMA = reshape(SEMA,[1 n]);
ECC = reshape(ECC,[1 n]);
INC = reshape(INC,[1 n]);
PHA = reshape(PHA,[1 n]);

XY = X+1i*Y;

if isnan(scale)
    %
    % Autoscale.
    %
    dist = sqrt((max(X)-min(X))*(max(Y)-min(Y))/n);
    if dist>0
        SEMA = 0.5*SEMA*dist/max(SEMA);
    end
else
    SEMA = scale*SEMA;
end

Wp = (1+ECC)/2 .*SEMA;
Wm = (1-ECC)/2 .*SEMA;
THETAp = INC-PHA;
THETAm = INC+PHA;

%convert degrees into radians
THETAp = THETAp/180*pi;
THETAm = THETAm/180*pi;

%Calculate wp and wm.
wp = Wp.*exp(1i*THETAp);
wm = Wm.*exp(1i*THETAm);

sep = repmat(NaN+NaN*1i,1,n);
switch plottype
    case {'ellipse','ellipsephase','ellipsephasevec'}
        ot = phase_offset+(0:72)'*pi/36;
        a = exp(1i*ot)*wp;
        b = exp(-1i*ot)*wm;
        w = a+b;
        switch plottype
            case 'ellipse'
                w = cat(1, ...
                    w, ...
                    sep);
            case 'ellipsephase'
                w = cat(1, ...
                    w(2:end,:), ...
                    zeros(1,n), ...
                    sep);
            case 'ellipsephasevec'
                w1 = 1.15*w(1,:);
                dw = w(2,:)-w(1,:);
                %
                w = cat(1, ...
                    zeros(1,n), ...
                    w1, ...
                    w1+4*dw, ...
                    w1+3*dw*exp(1i*pi/20), ...
                    w1+4*dw, ...
                        w1+3*dw*exp(-1i*pi/20), ...
                    sep, ...
                    w, ...
                    sep);
        end
        w = repmat(XY,size(w,1),1) + w;
        w = w(:);
    case 'cross'
        INCrad = INC*pi/180;
        w = cat(1, ...
            XY+SEMA.*exp(1i*INCrad), ...
            XY-SEMA.*exp(1i*INCrad), ...
            sep, ...
            XY+ECC.*SEMA.*exp(1i*(INCrad+pi/2)), ...
            XY-ECC.*SEMA.*exp(1i*(INCrad+pi/2)), ...
            sep);
        w = w(:);
end
hi = line(real(w), imag(w),parval{:});
if nargout>0
    h = hi;
end
