function out=vs_diff(VS1,VS2,varargin)
%VS_DIFF Locates the differences between two NEFIS files.
%   VS_DIFF(VS1,VS2,...options...) displays the names of the
%   groups/elements of the NEFIS files referred to by VS1 and VS2 that are
%   different. VS1 and VS2 may either be data structures as obtained from
%   vs_use calls, or they may be file names which can be passed to vs_use
%   to give such data structures.
%
%   The following options are accepted:
%    * 'Quantify': indicates the absolute and relative differences for the
%      elements that are different.
%    * FID: writes the information to an already opened text file with
%      handle FID.
%    * 'FailOnNaN': marks Not-a-Number values as not equal. By default two
%      files are considered equal if they have NaNs at exactly the same
%      locations.
%    * 'PropCheck': also compares group and element properties.
%    * 'CheckMinSize': use the minimum group and element dimensions to
%      compare the data. This enables you to check for instance if the
%      first time steps of an ongoing run match the results of another
%      run that already has written more time steps.
%
%   Diff = VS_DIFF(VS1,VS2) returns 1 if there are differences, 0 if there
%   are none. In this case no detailed printed listing is produced.
%
%   Example
%      F1 = vs_use('trim-xx1.dat','trim-xx1.def');
%      F2 = vs_use('trim-xx2.dat','trim-xx2.def');
%      vs_diff(F1,F2)
%
%   See also VS_USE, VS_DISP, VS_LET, VS_GET, VS_FIND, VS_TYPE.

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

fid=1;
FailOnNaN=0;
Quantify=0;
PropCheck=0;
CheckMinSize=0;
if nargin<2
    error('At least two input arguments required.')
elseif nargin>2
    for i=1:length(varargin)
        if ischar(varargin{i})
            switch lower(varargin{i})
                case 'failonnan'
                    FailOnNaN = 1;
                case 'quantify'
                    Quantify = 1;
                case 'propcheck'
                    PropCheck = 1;
                case 'checkminsize'
                    CheckMinSize = 1;
                otherwise
                    error('Unknown input argument: %s',varargin{i})
            end
        else
            fid = varargin{i};
        end
    end
end

if ischar(VS1)
    VS1=vs_use(VS1);
end
if ischar(VS2)
    VS2=vs_use(VS2);
end

gNames1=vs_disp(VS1,[]);
gNames2=vs_disp(VS2,[]);
DiffFound=0;

verbose=nargout==0;
if verbose
    fprintf(fid,'Comparing NEFIS files ...\n');
    fprintf(fid,'File 1: %s\n',[VS1.FileName VS1.DatExt]);
    fprintf(fid,'File 2: %s\n\n',[VS2.FileName VS2.DatExt]);
end

gNames1NotIn2=setdiff(gNames1,gNames2);
if ~isempty(gNames1NotIn2)
    DiffFound=1;
    if verbose
        fprintf(fid,'The following groups are contained in File 1 and not in File 2:\n');
        fprintf(fid,'%s\n',gNames1NotIn2{:});
        fprintf(fid,'\n');
    else
        out=DiffFound;
        return
    end
end
gNames2NotIn1=setdiff(gNames2,gNames1);
if ~isempty(gNames2NotIn1)
    DiffFound=1;
    if verbose
        fprintf(fid,'The following groups are contained in File 2 and not in File 1:\n');
        fprintf(fid,'%s\n',gNames2NotIn1{:});
        fprintf(fid,'\n');
    else
        out=DiffFound;
        return
    end
end

gNamesNotMatch={gNames1NotIn2{:},gNames2NotIn1{:}};
gNamesMatch=setdiff(gNames1,gNamesNotMatch);

for i=1:length(gNamesMatch)
    gInfo1=vs_disp(VS1,gNamesMatch{i},[]);
    gInfo2=vs_disp(VS2,gNamesMatch{i},[]);
    if ~isequal(gInfo1.SizeDim,gInfo2.SizeDim) && ~CheckMinSize
        DiffFound=1;
        if verbose
            fprintf(fid,'Group dimensions differ for: %s\n',gNamesMatch{i});
        else
            out=DiffFound;
            return
        end
    elseif ~isequal(gInfo1,gInfo2) && PropCheck
        DiffFound=1;
        if verbose
            fprintf(fid,'Group properties differ for: %s\n',gNamesMatch{i});
        else
            out=DiffFound;
            return
        end
    else
        eNames1=vs_disp(VS1,gNamesMatch{i});
        eNames2=vs_disp(VS2,gNamesMatch{i});
        
        eNames1NotIn2=setdiff(eNames1,eNames2);
        if ~isempty(eNames1NotIn2)
            DiffFound=1;
            if verbose
                fprintf(fid,'The following elements are part of group %s in File 1 and not in File 2:\n',gNamesMatch{i});
                fprintf(fid,'%s\n',eNames1NotIn2{:});
                fprintf(fid,'\n');
            else
                out=DiffFound;
                return
            end
        end
        eNames2NotIn1=setdiff(eNames2,eNames1);
        if ~isempty(eNames2NotIn1)
            DiffFound=1;
            if verbose
                fprintf(fid,'The following elements are part of group %s in File 2 and not in File 1:\n',gNamesMatch{i});
                fprintf(fid,'%s\n',eNames2NotIn1{:});
                fprintf(fid,'\n');
            else
                out=DiffFound;
                return
            end
        end
        
        eNamesNotMatch={eNames1NotIn2{:},eNames2NotIn1{:}};
        eNamesMatch=setdiff(eNames1,eNamesNotMatch);
        
        if verbose
            fprintf(fid,'Checking elements of %s ...\n',gNamesMatch{i});
        end
        for j=1:length(eNamesMatch)
            eInfo1=vs_disp(VS1,gNamesMatch{i},eNamesMatch{j});
            eInfo2=vs_disp(VS2,gNamesMatch{i},eNamesMatch{j});
            
            if ~isequal(eInfo1.SizeDim,eInfo2.SizeDim) && ~CheckMinSize
                DiffFound=1;
                if verbose
                    fprintf(fid,'  Element dimensions of %s of group %s differ.\n',eNamesMatch{j},gNamesMatch{i});
                else
                    out=DiffFound;
                    return
                end
            elseif ~isequal(eInfo1,eInfo2) && PropCheck
                DiffFound=1;
                if verbose
                    fprintf(fid,'  Properties of element %s of group %s differ.\n',eNamesMatch{j},gNamesMatch{i});
                else
                    out=DiffFound;
                    return
                end
            else % compare data
                if CheckMinSize
                    gSizeDim = min(gInfo1.SizeDim,gInfo2.SizeDim);
                    gSel = repmat({0},1,length(gSizeDim));
                    %
                    if isequal(eInfo1.SizeDim,eInfo2.SizeDim)
                        eSizeDim = eInfo1.SizeDim;
                        eSel = {};
                    else
                        eSizeDim = min(eInfo1.SizeDim,eInfo2.SizeDim);
                        eSel=cell(1,length(eSizeDim));
                        for k=1:length(eSizeDim)
                            eSel{k}=1:eSizeDim(k);
                        end
                        eSel = {eSel};
                    end
                else
                    gSizeDim = gInfo1.SizeDim;
                    eSizeDim = eInfo1.SizeDim;
                    gSel = repmat({0},1,length(gSizeDim));
                    eSel = {};
                end
                NBytes=prod(gSizeDim)*prod(eSizeDim)*8; % size of eData1 (not correct for complex and char arrays)
                szData = [gSizeDim eSizeDim];
                if (NBytes>5e6) && gInfo1.VarDim % above 5MB, use variable dimension if possible
                    dAMax = 0;
                    dRMax = 0;
                    szData(gInfo1.VarDim)=1;
                    for k=1:gSizeDim(gInfo1.VarDim)
                        gSel{gInfo1.VarDim}=k;
                        eData1=vs_let(VS1,gNamesMatch{i},gSel,eNamesMatch{j},eSel{:},'quiet','nowarn');
                        eData2=vs_let(VS2,gNamesMatch{i},gSel,eNamesMatch{j},eSel{:},'quiet','nowarn');
                        LDiffFound=0;
                        if ~isequal(size(eData1),size(eData2))
                            reason = 'differ in size';
                            LDiffFound=1;
                        elseif ~isequal(eData1,eData2) % fast and efficient check (but a NaN results in a difference)
                            % check for NaNs
                            NaNorEqual=eData1==eData2 | (isnan(eData1) & isnan(eData2));
                            LDiffFound=~all(NaNorEqual(:));
                            if LDiffFound
                                reason = 'differ';
                            elseif FailOnNaN
                                LDiffFound = 1;
                                reason = 'contain NaNs';
                            end
                        end
                        if LDiffFound
                            if verbose
                                DiffFound=LDiffFound;
                                if Quantify && strcmp(reason,'differ') && eInfo1.TypeVal~=1
                                    if any(xor(isnan(eData1(:)),isnan(eData2(:))))
                                        dMax  = NaN;
                                        iMax  = find(xor(isnan(eData1(:)),isnan(eData2(:))));
                                        [lAMax{1:length(szData)}] = ind2sub(szData,iMax(1));
                                        lAMax{gInfo1.VarDim} = k;
                                        break
                                    else
                                        dData = abs(eData1-eData2);
                                        dMax  = max(dData(:));
                                        if dMax>dAMax
                                            dAMax = dMax;
                                            iMax  = find(dData==dMax);
                                            [lAMax{1:length(szData)}] = ind2sub(szData,iMax(1));
                                            lAMax{gInfo1.VarDim} = k;
                                        end
                                        %
                                        dData = 2*dData./abs(eData1+eData2);
                                        dMax  = max(dData(:));
                                        if dMax>dRMax
                                            dRMax = dMax;
                                            iMax  = find(dData==dMax);
                                            [lRMax{1:length(szData)}] = ind2sub(szData,iMax(1));
                                            lRMax{gInfo1.VarDim} = k;
                                        end
                                    end
                                else
                                    fprintf(fid,'  Data of element %s of group %s %s at step %i.\n',eNamesMatch{j},gNamesMatch{i},reason,k);
                                    break
                                end
                            else
                                out=LDiffFound;
                                return
                            end
                        end
                    end
                    if LDiffFound && Quantify && strcmp(reason,'differ')
                        fprintf(fid,'  Data of element %s of group %s %s.\n',eNamesMatch{j},gNamesMatch{i},reason);
                        fprintf(fid,['    Maximum absolute difference %g located at index: (',repmat('%i,',1,length(szData)-1) '%i)\n'],dAMax,lAMax{:});
                        if ~isnan(dAMax) && eInfo1.TypeVal==5
                            fprintf(fid,['    Maximum relative difference %g located at index: (',repmat('%i,',1,length(szData)-1) '%i)\n'],dRMax,lRMax{:});
                        end
                    end
                else
                    gSel{gInfo1.VarDim} = 1:gSizeDim;
                    eData1=vs_let(VS1,gNamesMatch{i},gSel,eNamesMatch{j},eSel{:},'quiet','nowarn');
                    eData2=vs_let(VS2,gNamesMatch{i},gSel,eNamesMatch{j},eSel{:},'quiet','nowarn');
                    LDiffFound=0;
                    kStr = '';
                    if ~isequal(size(eData1),size(eData2))
                        reason = 'differ in size';
                        LDiffFound=1;
                    elseif ~isequal(eData1,eData2) % fast and efficient check (but a NaN results in a difference)
                        % check for NaNs
                        NaNorEqual=eData1==eData2 | (isnan(eData1) & isnan(eData2));
                        LDiffFound=~all(NaNorEqual(:));
                        if LDiffFound
                            reason = 'differ';
                        elseif FailOnNaN
                            LDiffFound = 1;
                            reason = 'contain NaNs';
                        end
                        %
                        % if there is a difference and we need to report in
                        % detail and there is a non-trivial variable group
                        % dimension then figure out what is the first index
                        % for which some data is different.
                        %
                        if LDiffFound && verbose && gInfo1.VarDim && gSizeDim(gInfo1.VarDim)>1
                            dims = [find((1:ndims(eData1))~=gInfo1.VarDim) gInfo1.VarDim];
                            eData1 = permute(eData1,dims);
                            eData2 = permute(eData2,dims);
                            eData1 = reshape(eData1,[numel(eData1)/gSizeDim(gInfo1.VarDim) gSizeDim(gInfo1.VarDim)]);
                            eData2 = reshape(eData2,size(eData1));
                            if FailOnNaN
                                k = find(~all(eData1==eData2,1));
                            else
                                k = find(~all(eData1==eData2 | (isnan(eData1) & isnan(eData2)),1));
                            end
                            kStr = sprintf(' at step %i',k(1));
                        end
                    end
                    if LDiffFound
                        if verbose
                            fprintf(fid,'  Data of element %s of group %s %s%s.\n',eNamesMatch{j},gNamesMatch{i},reason,kStr);
                            DiffFound=LDiffFound;
                            if Quantify && strcmp(reason,'differ') && eInfo1.TypeVal~=1
                                if any(xor(isnan(eData1(:)),isnan(eData2(:))))
                                    dMax  = NaN;
                                    iMax  = find(xor(isnan(eData1(:)),isnan(eData2(:))));
                                    lMax  = {};
                                    [lMax{1:length(szData)}]=ind2sub(szData,iMax(1));
                                    fprintf(fid,['    Maximum absolute difference %g located at index: (',repmat('%i,',1,length(szData)-1) '%i)\n'],dMax,lMax{:});
                                else
                                    dData = abs(eData1-eData2);
                                    dMax  = max(dData(:));
                                    iMax  = find(dData==dMax);
                                    lMax  = {};
                                    [lMax{1:length(szData)}]=ind2sub(szData,iMax(1));
                                    fprintf(fid,['    Maximum absolute difference %g located at index: (',repmat('%i,',1,length(szData)-1) '%i)\n'],dMax,lMax{:});
                                    %
                                    if  eInfo1.TypeVal==5
                                        dData = 2*dData./abs(eData1+eData2);
                                        dMax  = max(dData(:));
                                        iMax  = find(dData==dMax);
                                        lMax  = {};
                                        [lMax{1:length(szData)}]=ind2sub(szData,iMax(1));
                                        fprintf(fid,['    Maximum relative difference %g located at index: (',repmat('%i,',1,length(szData)-1) '%i)\n'],dMax,lMax{:});
                                    end
                                end
                            end
                        else
                            out=LDiffFound;
                            return
                        end
                    end
                end
            end
        end
    end
end

if verbose
    if DiffFound==0
        fprintf(fid,'Data contained in files is identical.\n');
    else
        fprintf(fid,'... comparison finished.\n');
    end
else
    %
    % For consistency convert to double always.
    %
    out=double(DiffFound);
end
