function varargout = xyveloc(varargin)
%XYVELOC Reads X,Y,U,V from a trim- or com-file.
%   [U,V] = XYVELOC(NFStruct,TSTEP) reads the velocity field at the time
%   step TSTEP from the NEFIS file specified by NFStruct as obtained from
%   VS_USE. By default TSTEP is the last field of the CURTIM or map-series
%   group. The default NEFIS file is the last opened NEFIS file.
%
%   [X,Y,U,V] = XYVELOC(NFStruct,TSTEP) reads also the coordinates of the
%   gridpoints at which the velocities are given (cell centres: water level
%   points).
%
%   [U,V,W] = XYVELOC(NFStruct,TSTEP) reads a 3D velocity field.
%
%   [X,Y,Z,U,V,W] = XYVELOC(NFStruct,TSTEP) reads the 3D velocity field and
%   returns the 3D coordinates of the velocity values.
%
%   [...] = XYVELOC(...,'option') where option equals:
%   * total, fluc, mean
%     Reads the total velocity, fluctuation component, or mean velocity
%     field in case of a HLES simulation. (trim-file, 2D only).
%     Default seting is 'total'.
%   * vort
%     Computes the z-component of the vorticity:
%     Vort = XYVELOC(...,'vort')
%
%   See also VS_USE, QPREAD.

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

if nargout<1
    error('Not enough output arguments.')
elseif nargout>6
    error('Too many output arguments.')
end

C = vs_use('lastread');
j = 1;
if nargin>0 && isstruct(varargin{j})
    C = varargin{j};
    j = j+1;
end
typeC = vs_type(C);

switch typeC
    case 'Delft3D-com'
        Info = vs_disp(C,'CURTIM',[]);
        if ~isstruct(Info)
            error('The file does not contain a flow data group.')
        end
        nTimes = Info.SizeDim;
        iTime = Info.SizeDim;
        Info = vs_disp(C,'CURTIM','U1');
    case 'Delft3D-trim'
        Info = vs_disp(C,'map-series',[]);
        if ~isstruct(Info)
            error('The file does not contain a flow data group.')
        end
        nTimes = Info.SizeDim;
        iTime = Info.SizeDim;
        Info = vs_disp(C,'map-series','U1');
    otherwise
        error('A %s file is not supported by this function.',typeC)
end
if length(Info.SizeDim)==3
    kmax = Info.SizeDim(3);
else
    kmax = 1;
end

if nargin>=j && ~ischar(varargin{j})
    iTime = varargin{j};
    j = j+1;
end

VecType = 'total';
Output = 'vector';
nOutput = 0;
k = j;
while k<=nargin
    if ischar(varargin{k})
        cmd = lower(varargin{k});
        switch cmd
            case {'total','fluc','mean'}
                VecType = cmd;
                k = k+1;
            case {'vort','relstdfluc'}
                Output = cmd;
                nOutput = 1;
                k = k+1;
            case 'stdfluc'
                Output = cmd;
                if kmax>1 && strcmp(typeC,'Delft3D-trim')
                    nOutput = 3;
                else
                    nOutput = 2;
                end
                k = k+1;
            otherwise
                warning('Unknown option: %s.',varargin{k});
        end
    end
end

if strcmp(Output,'vector')
    if nargout<2
        error('Not enough output arguments.')
    end
else
    if nargout~=nOutput
        error('Invalid number of output arguments.')
    end
end

switch typeC
    case 'Delft3D-com'
        if ~strcmp(VecType,'total')
            warning('Only ''total'' velocity supported for com-file.')
            VecType = 'total';
        end
        if kmax>1
            quant = 'velocity (horizontal)';
        else
            quant = 'depth averaged velocity';
        end
    case 'Delft3D-trim'
        switch VecType
            case 'total'
                if kmax>1
                    quant = 'velocity';
                else
                    quant = 'depth averaged velocity';
                end
            case 'mean'
                quant = 'filtered depth averaged velocity';
            case 'fluc'
                quant = 'd.a. velocity fluctuations';
        end
    otherwise
        error('Invalid NEFIS file for this action.')
end

switch Output
    case 'vector'
        if nargout>3
            griddata = 'griddata';
        else
            griddata = 'data';
        end
        [success,data] = qp_getdata(C,quant,griddata,iTime);
        if strcmp(typeC,'Delft3D-com') &&  nargout>2 && (nargout~=4) % 3D output requested for com-file
            warning('Only U and V components available in com-file.')
            data.ZComp = zeros(size(data.XComp));
        end
        switch nargout
            case 2 % U,V
                varargout = {data.XComp data.YComp};
            case 3 % U,V,W
                varargout = {data.XComp data.YComp data.ZComp};
            case 4 % X,Y,U,V
                varargout = {data.X data.Y data.XComp data.YComp};
            case 5 % X,Y,U,V,W
                varargout = {data.X data.Y data.XComp data.YComp data.ZComp};
            case 6 % X,Y,Z,U,V,W
                varargout = {data.X data.Y data.Z data.XComp data.YComp data.ZComp};
        end 
    case 'vort'
        switch typeC
            case 'Delft3D-com'
                U = vs_get(C,'CURTIM',{iTime},'U1');
                U = permute(U,[2 1 3]);
                V = vs_get(C,'CURTIM',{iTime},'V1');
                V = permute(V,[2 1 3]);
                %
                U(U==0) = NaN;
                V(V==0) = NaN;
            case 'Delft3D-trim'
                if strcmp(quant,'velocity')
                    quant = 'horizontal velocity';
                end
                %
                Cc=qp_data_resource(C);
                Data=struct(Cc.quantity(quant).time(iTime).raw.getdata);
                U = Data.Value(1).Data;
                V = Data.Value(2).Data;
                U(U==-999) = NaN;
                %
                sz = size(U);
                U = reshape(U,sz(2:end));
                V = reshape(V,sz(2:end));
                V(V==-999) = NaN;
        end
        %
        switch typeC
            case 'Delft3D-com'
                X = vs_get(C,'GRID','XCOR','quiet')';
                Y = vs_get(C,'GRID','YCOR','quiet')';
            case 'Delft3D-trim'
                X = vs_get(C,'map-const','XCOR','quiet')';
                Y = vs_get(C,'map-const','YCOR','quiet')';
        end
        X(X==0 & Y==0)=NaN;
        Y(isnan(X))=NaN;
        %
        GUU = sqrt( (X-X(:,[1 1:end-1])).^2 + (Y-Y(:,[1 1:end-1])).^2 );
        GUU(GUU==0) = NaN;
        GUU = (GUU(:,[2:end end],:)+GUU)/2; % this is actually GUD
        %
        GVV = sqrt( (X-X([1 1:end-1],:)).^2 + (Y-Y([1 1:end-1],:)).^2 );
        GVV(GVV==0) = NaN;
        GVV = (GVV([2:end end],:,:)+GVV)/2; % this is actually GVD
        %
        DU = U(:,[2:end end],:)-U;
        DV = V([2:end end],:,:)-V;
        for k = kmax:-1:1 % for each layer
            v = DV(:,:,k)./GVV-DU(:,:,k)./GUU;
            v(isnan(v))=0;
            n = (v(:,:)~=0) + (v([1 1:end-1],:)~=0) + (v(:,[1 1:end-1])~=0) + (v([1 1:end-1],[1 1:end-1])~=0);
            n(n==0) = NaN;
            Vort(:,:,k) = (v(:,:) + v([1 1:end-1],:) + v(:,[1 1:end-1]) + v([1 1:end-1],[1 1:end-1]))./n;
        end
        varargout = {Vort};
    case {'relstdfluc','stdfluc'}
        U2Sum = zeros(size(U));
        UMSum = U2Sum;
        V2Sum = U2Sum;
        VMSum = U2Sum;
        if kmax>1
            WMSum = U2Sum;
            W2Sum = U2Sum;
        end
        h0 = waitbar(0,'Please wait while computing ...');
        try
            for iTime = 1:nTimes
                [success,data] = qp_getdata(C,quant,'data',iTime);
                U2Sum = U2Sum+data.XComp.^2;
                UMSum = UMSum+data.XComp;
                V2Sum = V2Sum+data.YComp.^2;
                VMSum = VMSum+data.YComp;
                if kmax>1
                    W2Sum = W2Sum+data.ZComp.^2;
                    WMSum = WMSum+data.ZComp;
                end
                if ishandle(h0)
                    waitbar(iTime/nTimes,h0);
                end
            end
        catch
        end
        if ishandle(h0)
            close(h0);
        end
        U2Sum = (U2Sum-(UMSum.^2)/nTimes)/(nTimes-1);
        UMSum = UMSum/nTimes;
        V2Sum = (V2Sum-(VMSum.^2)/nTimes)/(nTimes-1);
        VMSum = VMSum/nTimes;
        if kmax>1
            W2Sum = (W2Sum-(WMSum.^2)/nTimes)/(nTimes-1);
            WMSum = WMSum/nTimes;
        end
        switch Output
            case 'stdfluc'
                if kmax==1
                    varargout = {sqrt(U2Sum) sqrt(V2Sum)};
                else % kmax>1
                    varargout = {sqrt(U2Sum) sqrt(V2Sum) sqrt(W2Sum)};
                end
            case 'relstdfluc'
                if kmax==1
                    U2Sum = sqrt(U2Sum+V2Sum);
                    UMSum = sqrt(UMSum.^2+VMSum.^2);
                else % kmax>1
                    U2Sum = sqrt(U2Sum+V2Sum+W2Sum);
                    UMSum = sqrt(UMSum.^2+VMSum.^2+WMSum.^2);
                end
                UMSum(UMSum==0) = NaN;
                varargout = {U2Sum./UMSum};
        end
end
