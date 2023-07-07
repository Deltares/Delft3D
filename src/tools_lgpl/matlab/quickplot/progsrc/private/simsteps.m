function [OutTxt,OutFig]=simsteps(C,i)
%SIMSTEPS Performs an timestep analysis.
%   SIMSTEPS(NfsTrimFile,i)
%   analyses the i-th dataset written to the
%   Delft3D FLOW file. It returns information on
%   maximum allowed timestep and the used timestep.
%   By default the last dataset written is used.

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

Txt={'NOTE: This function provides only indicative values.'
    'For theory see chapter 10 (in particular sections 10.4 and 10.6) of the'
    sprintf('Delft3D-FLOW manual.\n')};
if nargin==0
    C=vs_use;
end

switch lower(vs_type(C))
    case 'delft3d-trim'
        T=C;
        Cfile = strrep([C.FileName C.DatExt],'trim-','com-');
        if isempty(Cfile)
            C=[];
        else
            try
                C=vs_use(Cfile,'quiet');
            catch
                C=[];
            end
        end
        vs_use(T)
    case 'delft3d-com'
        T=vs_use(strrep(C.FileName,'com-','trim-'),'quiet');
        vs_use(C)
end
if nargin<2
    if isstruct(T)
        Info=vs_disp(T,'map-series',[]);
        i=Info.SizeDim;
    else
        Info=vs_disp(C,'CURTIM',[]);
        i=Info.SizeDim;
    end
end
if isstruct(T)
    ms=vs_get(T,'map-series',{i},'*','nowarn','quiet');
else
    ms=vs_get(C,'CURTIM',{i},'*','nowarn','quiet');
end
ms.U1=max(ms.U1,[],3);
ms.V1=max(ms.V1,[],3);
magU=sqrt(ms.U1.^2+ms.V1.^2);
magU(magU==0)=eps;

if isstruct(T)
    mc=vs_get(T,'map-const','*','quiet');
    mmc=mc;
else
    mc=vs_get(C,'GRID','*','quiet');
    mmc=vs_get(C,'KENMCNST','*','quiet');
end
X=mc.XCOR;
Y=mc.YCOR;
if isstruct(T)
    DP0=mc.DP0;
else
    Info=vs_disp(C,'BOTTIM',[]);
    DP0=vs_get(C,'BOTTIM',{Info.SizeDim},'DP','quiet');
end
DPZ=corner2center(DP0,'same');
H=ms.S1+DPZ;

I=vs_disp(T,'map-const','XZ');
if strcmp(I.ElmUnits,'[  DEG  ]')
    spherical = 1;
else
    spherical = 0;
end
distU=[];
distV=[];
if spherical
    distU(2:size(X,1),:)=geodist(X(1:end-1,:),Y(1:end-1,:),X(2:end,:),Y(2:end,:));
else
    distU(2:size(X,1),:)=sqrt(diff(X).^2+diff(Y).^2);
end
if spherical
    distV(:,2:size(X,2))=geodist(X(:,1:end-1),Y(:,1:end-1),X(:,2:end),Y(:,2:end));
else
    distV(:,2:size(X,2))=sqrt(diff(X,1,2).^2+diff(Y,1,2).^2);
end

distU=distU.*mmc.KCU;
distU(mmc.KCU~=1)=NaN;
distV=distV.*mmc.KCV;
distV(mmc.KCV~=1)=NaN;

dist=min(distU,distV);
isqdist=sqrt(distU.^(-2)+distV.^(-2));

if isstruct(T)
    dtused=mc.DT*mc.TUNIT;
else
    dtused=vs_get(C,'PARAMS','DT','quiet')*60;
end

%
% Horizontal viscosity
%
if isfield(ms,'VICUV') & ~isempty(ms.VICUV) & ~isequal(size(ms.VICUV),[1 1])
    VICUV=max(ms.VICUV,[],3);
    VICUV(VICUV==0)=NaN;
    Dt1=1./(VICUV.*(isqdist.^2));
    Dt1(Dt1==0)=NaN;

    createplot(jet(64),X,Y,spherical,dtused./Dt1, ...
        'Courant number for viscosity', ...
        [0 2],'Courant number');

    dt1=min(Dt1(:));
    Txt{end+1}=sprintf('The maximum allowed timestep based on Reynolds stresses is %f seconds.\n',dt1);
else
    Dt1=1./(isqdist.^2);
    dt1=min(Dt1(:));
    Txt{end+1}=sprintf('The maximum allowed timestep based on Reynolds stresses cannot be determined.',dt1);
    Txt{end+1}=sprintf('Use as an estimate %f/[horizontal viscosity] seconds.\n',dt1);
    dt1=[];
end

%
% Barotropic mode (wave propagation)
%
AG=9.83;
vK=0.41;
maxCFLwav=10;
Ct2=(2*sqrt(AG*max(eps,H)).*isqdist);

createplot(jet(64),X,Y,spherical,Ct2*dtused, ...
    'Courant number for barotropic mode (wave propagation)', ...
    [0 10],'Courant number');

Dt2=maxCFLwav./Ct2;
dt2=min(Dt2(:));
Txt{end+1}=sprintf('The maximum allowed timestep for accurate computation of wave propagation');
Txt{end+1}=sprintf('is %f seconds based on a maximum Courant number for free surface',dt2);
Txt{end+1}=sprintf('waves of 10.\n');

%
% Advection (explicit drying flooding: Cmax=2, transport: 1)
%
Dt3=dist./magU;

createplot(jet(64),X,Y,spherical,dtused./Dt3, ...
    'Courant number for advection (drying/flooding, transport)', ...
    [0 2],'Courant number');

dt3=min(Dt3(:));
Txt{end+1}=sprintf('The maximum allowed timestep for horizontal advection is %f seconds.',dt3);

%
% Secondary flow
%
if isstruct(T)
    rsp=strmatch('Secondary flow',mc.NAMCON,'exact');
else
    rsp=1;
end
if ~isempty(rsp) & ~isempty(C)
    RGH=vs_get(C,'ROUGHNESS','*','quiet');
    rgh=RGH.ROUFLO;
    RGH=(RGH.CFUROU+RGH.CFVROU)/2;
    switch rgh
        case 'WHIT'
            RGH(RGH<=0)=NaN;
            RGH=18*log10(12*H./RGH);
        case 'MANN'
            RGH(RGH<=0)=NaN;
            RGH=H.^(1/6)./RGH;
        case 'Z   '
            RGH(RGH<=0)=NaN;
            RGH=18*log10(12*H./(30*RGH));
    end
    if isstruct(T)
        RSP=ms.R1(:,:,1,rsp);
    else
        RSP=ms.RSP;
    end
    alpha=sqrt(AG)./(vK.*RGH);
    denom=2*RSP.*(5*alpha-15.6*alpha.^2+37.5*alpha.^3);
    denom(denom==0)=NaN;
    Dt4=dist./abs(denom);

    createplot(jet(64),X,Y,spherical,dtused./Dt4, ...
        'Courant number for spiral flow', ...
        [0 2],'Courant number');

    dt4=min(Dt4(:));
    Txt{end+1}=sprintf('The maximum allowed value of Betac * Dt for spiral flow is %f seconds.',dt4);
elseif ~isempty(rsp)
    Txt{end+1}=sprintf('The maximum allowed value of Betac * Dt for spiral flow');
    Txt{end+1}=sprintf('is not applicable or it cannot be determined.\n');
end

Txt{end+1}=sprintf('The flow timestep used in the simulation equals %f seconds.',dtused);

if ~isempty(dt1)
    Dt=min(Dt1,Dt2); dt=min(dt1,dt2);
else
    Dt=Dt2; dt=dt2;
end
Dt=min(Dt,Dt3); dt=min(dt,dt3);

Fig=createplot(flipud(jet(64)),X,Y,spherical,Dt, ...
    'Spatial variation of the maximum allowed timestep', ...
    [min(dt,dtused) max(1e-10,min(4*dt,4*dtused))],'seconds');

if nargout==0
    fprintf('%s\n',Txt{:})
else
    OutTxt=Txt;
    if nargout>1
        OutFig=Fig;
    end
end

function Fig = createplot(cmap,X,Y,spherical,V,ttl,clm,val)
X(X==0 & Y==0) = NaN;
X(isnan(V)) = NaN;
Y(isnan(X)) = NaN;
Fig = qp_createfig('quick',ttl);
set(Fig,'colormap',cmap);
Ax = qp_createaxes(Fig,'oneplot');
surf(Ax,X,Y,V);
shading interp
title(ttl)
if spherical
    setaxesprops(Ax,'Lon-Lat')
else
    setaxesprops(Ax,'X-Y',{},{'m' 'm'})
end
set(Ax,'clim',clm);
C=qp_colorbar('horz');
xlabel(C,[val ' \rightarrow'])
set(findall(gcf),'deletefcn','')
