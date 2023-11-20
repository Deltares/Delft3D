function I = spirint(varargin)
%SPIRINT Computes spiral intensity from 3D flow field.
%
%   SPI=SPIRINT(Chezy,NefisFile,TimeStep)
%   The Chezy value is required (see explanation below). The other
%   two input arguments may be dropped. In that case the following
%   defaults are used:
%   Default NefisFile = last opened Nefis file
%   Default TimeStep = last stored in Nefis file
%   If the file is a communication file, the specified Chezy may
%   be empty, that is []. In that case the roughness is read from
%   the file.
%
%   For 2D simulations, the spiral intensity is read directly
%   from the NEFIS file (Chezy value is a dummy argument). For
%   3D simulations, it is computed as
%
%       (vda * qx1 - uda * qy1) * Uda
%   I = -----------------------------
%       (uda * qx1 + vda * qy1) * alf
%
%   Uda = sqrt(uda^2+vda^2)
%   alf = (2/(kappa^2)) * (1.0 - 0.5 * sqrt(g) / (kappa * C) )
%
%   uda/vda : depth averaged flow velocity in u/v direction
%   qx1/qy1 : discharge in lowest layer
%   g       : gravitational acceleration
%   kappa   : von Karman constant
%   C       : Chezy roughness value

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

% determine Chezy value ... Chz
j=1;
if (nargin>=j) & ~isstruct(varargin{j}),
    Chz=varargin{j};
    if ischar(Chz),
        Chz=varargin([j j+1]);
        j=j+2;
    else
        j=j+1;
    end
else,
    error('Missing Chezy value.');
end;

% determine NEFIS file ... C
C=vs_use('lastread');
if (nargin>=j) & isstruct(varargin{j}),
    C=varargin{j};
    j=j+1;
end;

% determine timestep ... i
switch vs_type(C),
    case 'Delft3D-com',
        Info=vs_disp(C,'CURTIM',[]);
        i0=Info.SizeDim;
        i=Info.SizeDim;
    case 'Delft3D-trim',
        Info=vs_disp(C,'map-series',[]);
        i0=Info.SizeDim;
        i=Info.SizeDim;
    otherwise
        error('File type not supported.');
end;
if (nargin>=j),
    i=varargin{j};
    j=j+1;
end;

% read data from NEFIS file ...
switch vs_type(C),
    case 'Delft3D-com',
        kmax=vs_get(C,'GRID','KMAX','quiet');
        if (kmax==1),
            I=vs_get(C,'CURTIM',{i},'RSP','quiet');
            return;
        end;
        thk=vs_get(C,'GRID','THICK','quiet');
        U=vs_get(C,'CURTIM',{i},'U1','quiet');
        V=vs_get(C,'CURTIM',{i},'V1','quiet');
        kfu=vs_get(C,'KENMTIM',{i},'KFU','quiet');
        kfv=vs_get(C,'KENMTIM',{i},'KFV','quiet');
        if isempty(Chz)
            RouFlo=vs_get(C,'ROUGHNESS','ROUFLO','quiet');
            ChzU=vs_get(C,'ROUGHNESS','CFUROU','quiet');
            ChzV=vs_get(C,'ROUGHNESS','CFVROU','quiet');
            Chz={RouFlo ChzU ChzV};
        end
        if iscell(Chz)
            S1=vs_get(C,'CURTIM',{i},'S1','quiet');
            lstDP=vs_disp(C,'BOTTIM',[]);
            dp=vs_get(C,'BOTTIM',{lstDP.SizeDim},'DP','quiet');
            [nfltp,Chk]=vs_get(C,'PARAMS','NFLTYP','quiet');
            switch nfltp
                case 2
                    dp22=dp(2:end,2:end);
                    dp22=max(dp(1:end-1,1:end-1),dp22);
                    dp22=max(dp(2:end,1:end-1),dp22);
                    dp22=max(dp(1:end-1,2:end),dp22);
                    dp(2:end,2:end)=dp22; dp=-dp;
                case 3
                    dp22=dp(2:end,2:end);
                    dp22=min(dp(1:end-1,1:end-1),dp22);
                    dp22=min(dp(2:end,1:end-1),dp22);
                    dp22=min(dp(1:end-1,2:end),dp22);
                    dp(2:end,2:end)=dp22; dp=-dp;
                otherwise
                    dp=interp2cen(-dp);
            end
            H=S1-dp;
        end
    case 'Delft3D-trim',
        kmax=vs_get(C,'map-const','KMAX','quiet');
        if (kmax==1),
            I=vs_get(C,'map-series',{i},'R1',{0 0 1 1},'quiet');
            return;
        end;
        thk=vs_get(C,'map-const','THICK','quiet');
        U=vs_get(C,'map-series',{i},'U1','quiet');
        V=vs_get(C,'map-series',{i},'V1','quiet');
        kfu=vs_get(C,'map-series',{i},'KFU','quiet');
        kfv=vs_get(C,'map-series',{i},'KFV','quiet');
        if iscell(Chz)
            S1=vs_get(C,'map-series',{i},'S1','quiet');
            DP=vs_get(C,'map-const','DP0','quiet');
            DP=(DP+DP([2:end end],[2:end end])+DP([2:end end],:)+DP(:,[2:end end]))/4;
            H=S1+DP;
        end
end;

% Select data only in active points (maybe not necessary) ...

kmax=size(U,3);
for k=1:kmax, % for each layer
    U(:,:,k)=U(:,:,k).*kfu;
    V(:,:,k)=V(:,:,k).*kfv;
end;

% Interpolate to waterlevel point ...
U=U+U(:,[1 1:end-1],:);
V=V+V([1 1:end-1],:,:);
actu=max(1,conv2(double([kfu(:,1)>0 kfu>0]),[1 1],'valid'));
actv=max(1,conv2(double([kfv(1,:)>0;kfv>0]),[1;1],'valid'));
for k=1:kmax, % for each layer
    U(:,:,k)=U(:,:,k)./actu;
    V(:,:,k)=V(:,:,k)./actv;
end;

% Compute depth average using layer thickness ...
Um=zeros([size(U,1) size(U,2)]);
Vm=zeros([size(V,1) size(V,2)]);
for k=1:kmax, % for each layer
    Um=Um+U(:,:,k)*thk(k);
    Vm=Vm+V(:,:,k)*thk(k);
end;

% Compute velocity magnitude ...
UU=sqrt(Um.^2+Vm.^2);

lyr=kmax;
% Compute numerator ...
Inum=(Vm.*U(:,:,lyr)-Um.*V(:,:,lyr)).*UU;

% Compute denominator ...
if iscell(Chz),
    H=H.*(H>0);
    H(H==0)=NaN;
    for i=2:length(Chz)
        switch lower(Chz{1}),
            case {'manning','mann','n'},
                Chz{i}=H.^(1/6)./Chz{i};
            case {'white-colebrook','whit','nikuradse','k'},
                Chz{i}=18*log10(12*H./Chz{i});
            case {'z   ','z0','z'},
                Chz{i}=18*log10(12*H./(30*Chz{i}));
            case 'chez'
            otherwise
                error('Unknown roughness indicator.');
        end
    end
    if length(Chz)==2
        Chz=Chz{2};
    else
        ChzU=Chz{2}.*kfu;
        ChzU(:,2:end)=ChzU(:,2:end)+ChzU(:,1:end-1);
        K=kfu; K(:,2:end)=K(:,2:end)+kfu(:,1:end-1); K(K==0)=NaN;
        ChzU=ChzU./K;
        ChzU(isnan(ChzU))=0;
        %
        ChzV=Chz{3}.*kfv;
        ChzV(2:end,:)=ChzV(2:end,:)+ChzV(1:end-1,:);
        K=kfv; K(2:end,:)=K(2:end,:)+kfv(1:end-1,:); K(K==0)=NaN;
        ChzV=ChzV./K;
        ChzV(isnan(ChzV))=0;
        %
        K=(ChzU~=0)+(ChzV~=0); K(K==0)=NaN;
        Chz=(ChzU+ChzV)./K;
    end
end
if min(Chz(:))<5 | max(Chz(:))>90,
    fprintf('Extreme Chezy values:\nMinimum: %f\nMaximum: %f\n',[min(Chz(:)) max(Chz(:))]);
end
kappa=0.4;
alf = (2/(kappa^2)) * (1.0 - 0.5 * sqrt(9.81) ./ ( kappa * Chz ) );
Iden=(Um.*U(:,:,lyr)+Vm.*V(:,:,lyr)).*alf;

% Compute I ...
Iden(Iden==0)=NaN;
I=Inum./Iden;

I(isnan(I))=0;
