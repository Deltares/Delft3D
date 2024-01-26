function [OutTxt, OutFig] = simsteps(comFile, i)
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

if nargin == 0
    comFile = vs_use;
end

lastFile = vs_use('lastread');
switch lower(vs_type(comFile))
    case 'delft3d-trim'
        trimFile = comFile;
        comName = strrep([comFile.FileName comFile.DatExt],'trim-','com-');
        if ~exist(comName,'file')
            % comName doesn't exist ... try looking for a partition
            comName = [comName(1:end-4) '-001.dat'];
        end
        if ~exist(comName,'file')
            % comName still doesn't exist ... give up
            comName = [];
        end
        comFile = [];
        try
            comFile = qpfopen(comName);
        catch
        end
    case 'delft3d-com'
        trimName = strrep([comFile.FileName comFile.DatExt],'com-','trim-');
        if ~exist(trimName,'file')
            % trimName doesn't exist ... remove partition info
            trimName = [trimName(1:end-8) trimName(end-4:end)];
        end
        if ~exist(trimName,'file')
            % trimName still doesn't exist ... give up
            trimName = [];
        end
        trimFile = [];
        try
            trimFile = qpfopen(trimName);
        catch
        end
end
vs_use(lastFile)

if isfield(trimFile,'NEFIS')
    trimDomain = length(trimFile.NEFIS)+2;
else
    trimDomain = 1;
end
if isfield(comFile,'NEFIS')
    comDomain = length(comFile.NEFIS)+2;
else
    comDomain = 1;
end

if nargin<2
    if isstruct(trimFile)
        Info = vs_disp(trimFile,'map-series',[]);
        i = Info.SizeDim;
    else
        Info = vs_disp(comFile,'CURTIM',[]);
        i = Info.SizeDim;
    end
end

if isstruct(trimFile)
    trimFields = qpread(trimFile,trimDomain);
end
if isstruct(comFile)
    comFields = qpread(comFile,comDomain);
end

% read data ...
if isstruct(trimFile)
    % TODO: maximum over depth of horizontal velocity
    if ismember('horizontal velocity',{trimFields.Name})
        velocity = qpread(trimFile,trimDomain,'horizontal velocity','gridcelldata',i);
        magnitude = sqrt(velocity.XComp.^2 + velocity.YComp.^2);
        velocity.magnitude = max(magnitude,[],3);
        velocity.X = velocity.X(:,:,1);
        velocity.Y = velocity.Y(:,:,1);
    else
        velocity = qpread(trimFile,trimDomain,'depth averaged velocity','gridcelldata',i);
        velocity.magnitude = sqrt(velocity.XComp.^2 + velocity.YComp.^2);
    end
    waterdepth = qpread(trimFile,trimDomain,'water depth','celldata',i);
    viscosity = qpread(trimFile,trimDomain,'horizontal viscosity','celldata',i);
else
    velocity = qpread(comFile,comDomain,'depth averaged velocity','gridcelldata',i);
    velocity.magnitude = sqrt(velocity.XComp.^2 + velocity.YComp.^2);
    waterlevel = qpread(comFile,comDomain,'water level','gridcelldata',i);
    bedlevel = qpread(comFile,comDomain,'time-varying bed level','celldata',i);
    waterdepth.Val = waterlevel.Val - bedlevel.Val;
    viscosity = [];
end

mesh.X = velocity.X;
mesh.Y = velocity.Y;
if strcmp(velocity.XUnits,'deg')
    spherical = 1;
else
    spherical = 0;
end
if spherical
    mesh.distN = geodist(mesh.X(:,1:end-1),mesh.Y(:,1:end-1),mesh.X(:,2:end),mesh.Y(:,2:end));
else
    mesh.distN = sqrt(diff(mesh.X,1,2).^2+diff(mesh.Y,1,2).^2);
end
mesh.distN = (mesh.distN(1:end-1,:) + mesh.distN(2:end,:))/2;

if spherical
    mesh.distM = geodist(mesh.X(1:end-1,:),mesh.Y(1:end-1,:),mesh.X(2:end,:),mesh.Y(2:end,:));
else
    mesh.distM = sqrt(diff(mesh.X,1,1).^2+diff(mesh.Y,1,1).^2);
end
mesh.distM = (mesh.distM(:,1:end-1) + mesh.distM(:,2:end))/2;

mesh.distN(isnan(waterdepth.Val)) = NaN;
mesh.distM(isnan(waterdepth.Val)) = NaN;

dist = min(mesh.distN, mesh.distM);
inverseSquaredDist = mesh.distN.^(-2) + mesh.distM.^(-2);

if isstruct(trimFile)
    dtused = vs_get(trimFile,'map-const','DT','quiet') * ...
        vs_get(trimFile,'map-const','TUNIT','quiet');
else
    dtused = vs_get(comFile,'PARAMS','DT','quiet') * 60;
end

%
% Horizontal viscosity
%
if ~isempty(viscosity)
    viscosity.MaxVal = max(viscosity.Val,[],3);
    viscosity.MaxVal(viscosity.MaxVal==0) = NaN;
    Dt1 = 1./(viscosity.MaxVal.*inverseSquaredDist);
    Dt1(Dt1==0) = NaN;

    createplot(jet(64),mesh.X,mesh.Y,spherical,dtused./Dt1, ...
        'Courant number for viscosity', ...
        [0 2],'Courant number');

    dt1=min(Dt1(:));
    Txt{end+1}=sprintf('The maximum allowed timestep based on Reynolds stresses is %f seconds.\n',dt1);
else
    Dt1=1./inverseSquaredDist;
    dt1=min(Dt1(:));
    Txt{end+1}=sprintf('The maximum allowed timestep based on Reynolds stresses cannot be determined.');
    Txt{end+1}=sprintf('Use as an estimate %f/[horizontal viscosity] seconds.\n',dt1);
    dt1=[];
end

%
% Barotropic mode (wave propagation)
%
gravity = 9.83;
vKarman = 0.41;
maxCFLwav = 10;
Ct2 = 2 * sqrt(gravity * max(eps,waterdepth.Val) .* inverseSquaredDist);

createplot(jet(64),mesh.X,mesh.Y,spherical,Ct2*dtused, ...
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
Dt3=dist./velocity.magnitude;

createplot(jet(64),mesh.X,mesh.Y,spherical,dtused./Dt3, ...
    'Courant number for advection (drying/flooding, transport)', ...
    [0 2],'Courant number');

dt3=min(Dt3(:));
Txt{end+1}=sprintf('The maximum allowed timestep for horizontal advection is %f seconds.',dt3);

%
% Secondary flow
%
secflow = [];
try
    if ismember('secondary flow',{trimFields.Name})
        secflow = qpread(trimFile,trimDomain,'secondary flow','celldata',i);
    end
catch
end

roughnessM = [];
roughnessN = [];
try
    if ustrcmpi('u roughness',{comFields.Name})
        roughnessM = qpread(comFile,comDomain,'u roughness','celldata');
        roughnessN = qpread(comFile,comDomain,'v roughness','celldata');
    end
catch
end

if ~isempty(secflow) && ~isempty(roughnessM)
    RGH = (roughnessM.Val + roughnessN.Val)/2;
    switch roughnessM.Name
        case 'WHIT'
            RGH(RGH<=0)=NaN;
            RGH=18*log10(12*waterdepth./RGH);
        case 'MANN'
            RGH(RGH<=0)=NaN;
            RGH=waterdepth.^(1/6)./RGH;
        case 'Z   '
            RGH(RGH<=0)=NaN;
            RGH=18*log10(12*waterdepth./(30*RGH));
    end
    if isstruct(trimFile)
        RSP=ms.R1(:,:,1,rsp);
    else
        RSP=ms.RSP;
    end
    alpha=sqrt(gravity)./(vKarman.*RGH);
    denom=2*RSP.*(5*alpha-15.6*alpha.^2+37.5*alpha.^3);
    denom(denom==0)=NaN;
    Dt4=dist./abs(denom);

    createplot(jet(64),mesh.X,mesh.Y,spherical,dtused./Dt4, ...
        'Courant number for spiral flow', ...
        [0 2],'Courant number');

    dt4=min(Dt4(:));
    Txt{end+1}=sprintf('The maximum allowed value of Betac * Dt for spiral flow is %f seconds.',dt4);
else
    Txt{end+1}=sprintf('The maximum allowed value of Betac * Dt for spiral flow');
    Txt{end+1}=sprintf('is not applicable or it cannot be determined.\n');
end

Txt{end+1}=sprintf('The flow timestep used in the simulation equals %f seconds.',dtused);

if ~isempty(dt1)
    Dt=min(Dt1,Dt2);
    dt=min(dt1,dt2);
else
    Dt=Dt2;
    dt=dt2;
end
Dt=min(Dt,Dt3);
dt=min(dt,dt3);

Fig=createplot(flipud(jet(64)),mesh.X,mesh.Y,spherical,Dt, ...
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
Fig = qp_createfig('quick',ttl);
set(Fig,'colormap',cmap);
Ax = qp_createaxes(Fig,'oneplot');
surf(Ax,X,Y,0*Y,V);
shading flat
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
