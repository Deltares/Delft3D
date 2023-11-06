function Data = map2golder(mapfile,TSelect,ZGolder,golderfile)
%MAP2GOLDER Convert bed stratigraphy into Golder input file.
%   S = MAP2GOLDER(TRIMFILE,T,ZLAYERS,GOLDERFILE) opens the Delft3D-FLOW
%   map-file called TRIMFILE and reads the data of the specified time step
%   T (last time step if [] is specified) from the map-file and writes the
%   data to the file called GOLDERFILE in GOLDER ASCII file format. The
%   array ZLAYERS specifies the z-levels of the layer interfaces of the
%   Golder file.
%
%   Example
%      map2golder('trim-xxx.dat',[],[],'golder.txt');
%
%   See also GOLDER.

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

% Open Delft3D-TRIM file.
if isstruct(mapfile)
    T = mapfile;
else
    T = vs_use(mapfile);
end
%
% Determine number of time steps in file and determine time step to read.
I = vs_disp(T,'map-sed-series',[]);
NTimes = I.SizeDim;
if nargin<2 | isempty(TSelect) %#ok<OR2>
    TSelect = NTimes;
elseif ~isequal(size(TSelect),[1 1]) | TSelect(1)<1 | TSelect(1)>NTimes | TSelect(1)~=round(TSelect(1)) %#ok<OR2>
    error('Invalid time step selected.')
end
%
% Read the data from the TRIM file.
Fracs = vs_get(T,'map-sed-series',{TSelect},'LYRFRAC','quiet');
Thick = vs_get(T,'map-sed-series',{TSelect},'THLYR','quiet');
ZbLvl = -vs_get(T,'map-sed-series',{TSelect},'DPS','quiet');
KCS   = vs_get(T,'map-const','KCS','quiet');
%
% Remove the dummy grid cells surrounding the domain.
Fracs = Fracs(2:end-1,2:end-1,:,:);
Thick = Thick(2:end-1,2:end-1,:);
ZbLvl = ZbLvl(2:end-1,2:end-1);
KCS   = KCS  (2:end-1,2:end-1);
%
% Determine the number of sediment fractions.
nfrac = size(Fracs,4);
%
% Determine the bed level range.
HighestBedLevel = max(ZbLvl(KCS==1));
%LowestBedLevel  = min(ZbLvl(KCS==1));
%
% Determine the z-coordinates of the layer interfaces.
ZLvl = cumsum(cat(3,ZbLvl,-Thick),3);
%
% Determine the z-coordinates of the roch layer; ZrLvl = ZLvl(:,:,end)
TotThick = sum(Thick,3);
ZrLvl = ZbLvl - TotThick;
%
% Determine the rock level range.
%HighestRockLevel = max(ZrLvl(KCS==1));
LowestRockLevel  = min(ZrLvl(KCS==1));
%
% Optionally export the information on fractions and layer interfaces
% (original data).
if nargin<3
    Data.Z = ZLvl;
    Data.Val = Fracs;
    return
end
%
% Determine the z-layer interface for which the data should be exported.
if isempty(ZGolder) | length(ZGolder)==1 %#ok<OR2>
    if isempty(ZGolder)
        dzMin = (HighestBedLevel-LowestRockLevel)/100;
        dzMax = (HighestBedLevel-LowestRockLevel)/20;
        dz = [0.1 0.125 0.2 0.25 0.4 0.5];
        while 1
            if all(dz<dzMin)
                dz = dz*10;
            elseif all(dz>dzMax)
                dz = dz/10;
            else
                dz = [dz/10 dz dz*10]; %#ok<AGROW>
                dz = dz(dz>=dzMin & dz<=dzMax);
                idz = floor(length(dz)/2);
                dz = dz(idz);
                break
            end
        end
    else
        dz = ZGolder;
    end
    ZGolder = dz*(floor(LowestRockLevel/dz):ceil(HighestBedLevel/dz));
end
%
% Determine the average sediment fractions for all GOLDER layers.
ZGolder = sort(ZGolder,'descend');
nlayers = length(ZGolder)-1;
FGolder = zeros([size(KCS) nlayers nfrac]);
for i = 1:nlayers
   ZLim = min(ZGolder(i),max(ZGolder(i+1),ZLvl));
   Dz   = ZLim(:,:,1:end-1)-ZLim(:,:,2:end);
   %
   DzTot = sum(Dz,3);
   DzTot(DzTot==0) = 1;
   Temp = zeros([size(KCS) 1 nfrac]);
   for f = 1:nfrac
      Temp(:,:,1,f) = sum(Dz.*Fracs(:,:,:,f),3)./DzTot;
   end
   Temp = reshape(Temp,[numel(KCS) nfrac]);
   Mask = sum(Temp,2)==0;
   Temp(Mask,:) = 2;
   Temp = reshape(Temp,[size(KCS) 1 nfrac]);
   FGolder(:,:,i,:) = Temp;
end
%
% Optionally export the information on fractions and layer interfaces
% (interpolated data).
if nargin<4
    Data.Z = ZGolder;
    Data.Val = FGolder;
    return
end
%
% Convert the data into GOLDER input arguments.
fracs = cell(1,2*nfrac);
for f = 1:nfrac
   fracs{(f-1)*2+1} = sprintf('VolFrac_%i',f);
   fracs{f*2} = FGolder(:,:,:,f);
end
%
% Write the data to the GOLDER input file.
golder('write',golderfile,fracs{:})
if nargout>0
    Data = {sprintf('Data exported to Golder file: %s.',golderfile)
        sprintf('Layer interfaces located at z values from %g to %g with step %g.',ZGolder(end),ZGolder(1),dz)};
end
