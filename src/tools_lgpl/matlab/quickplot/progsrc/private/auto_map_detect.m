function DataProps=auto_map_detect(FI,DataProps,nm,k,SkipGroup,SkipElem)
%AUTO_MAP_DETECT Autodetect function for Delft3D map files.

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

if nargin<6
    SkipElem={};
end
%
if size(DataProps,2)==16
    % d3d_trimfil "infile"
    propversion = 2;
    igrp = 12;
    ielm = 13:14;
elseif size(DataProps,2)==10
    % d3d_trimfil "quantities"
    propversion = 1;
    igrp = 8;
    ielm = 9:10;
else
    % other cases
    propversion = 0;
    igrp = 10;
    ielm = 11:12;
end
%
% Don't want to sort the Grps and therefore I don't use setdiff here ...
%
Grps=vs_disp(FI,[]);
Grps(ismember(Grps,SkipGroup))=[];
%
fld=size(DataProps,1);
for i=1:length(Grps)
    Grpi=strmatch(Grps{i},DataProps(:,igrp),'exact');
    InfoG=vs_disp(FI,Grps{i},[]);
    %
    % Don't want to sort the Elms and therefore I don't use setdiff here ...
    %
    Elms=vs_disp(FI,Grps{i});
    Elms(ismember(Elms,SkipElem))=[];
    %
    for j=1:length(Elms)
        ELMSinGRP = DataProps(Grpi,ielm);
        Elmi=strmatch(Elms{j},ELMSinGRP(:),'exact');
        Info=vs_disp(FI,Grps{i},Elms{j});
        if (isempty(Elmi) || isempty(Grpi)) && isstruct(Info) && all(InfoG.SizeDim>0)
            sz=Info.SizeDim; sz(6)=1;
            if isequal(sz([1 2]),nm)
                if isempty(Info.ElmDescription)
                    ediscr=sprintf('%s of %s',Elms{j},Grps{i});
                else
                    ediscr=Info.ElmDescription;
                end
                eunit=strtrim(Info.ElmUnits);
                if ~isempty(eunit) && eunit(1)=='[' && eunit(end)==']'
                    eunit = strtrim(eunit(2:end-1));
                end
                switch eunit
                    case '-'
                        eunit='';
                    case 'M'
                        eunit='m';
                    case 'M/S'
                        eunit='m/s';
                    case 'M2/S'
                        eunit='m^2/s';
                    case 'MM/H'
                        eunit='mm/h';
                    case 'DEG'
                        eunit='deg';
                end
                if k==1
                    if Info.NDim==2
                        [DataProps,fld] = addprop(DataProps,fld,propversion,ediscr,eunit,'',Grps{i},Elms{j},[]);
                    elseif Info.NDim>=3
                        [DataProps,fld] = addpropmulti(DataProps,fld,propversion,ediscr,eunit,'',Grps{i},Elms{j},sz(3:Info.NDim));
                    end
                else
                    if sz(3)==k
                        loc3d='c';
                    elseif sz(3)==k+1
                        loc3d='i';
                    else
                        loc3d='';
                    end
                    if isempty(loc3d)
                        if sz(3)==0
                            [DataProps,fld] = addprop(DataProps,fld,propversion,ediscr,eunit,'',Grps{i},Elms{j},[]);
                        elseif Info.NDim>=3
                            [DataProps,fld] = addpropmulti(DataProps,fld,propversion,ediscr,eunit,'',Grps{i},Elms{j},sz(3:Info.NDim));
                        end
                    else
                        if Info.NDim==3
                            [DataProps,fld] = addprop(DataProps,fld,propversion,ediscr,eunit,loc3d,Grps{i},Elms{j},[]);
                        elseif Info.NDim>=4
                            [DataProps,fld] = addpropmulti(DataProps,fld,propversion,ediscr,eunit,loc3d,Grps{i},Elms{j},sz(4:Info.NDim));
                        end
                    end
                end
            end
        end
    end
    [DataProps,fld] = addsep(DataProps,fld,propversion);
end

function [DataProps,fld] = addpropmulti(DataProps,fld,propversion,ediscr,eunit,loc3d,Grp,Elm,sz)
nel = prod(sz);
ndim = length(sz);
for e=1:nel
    [x{1:ndim}] = ind2sub(sz,e);
    if nel>1
        Str=sprintf('%i, ',x{:});
        Str=sprintf('%s (%s)',ediscr,Str(1:end-2));
    else
        Str=ediscr;
    end
    [DataProps,fld] = addprop(DataProps,fld,propversion,Str,eunit,loc3d,Grp,Elm,x);
end

function [DataProps,fld] = addprop(DataProps,fld,propversion,ediscr,eunit,loc3d,Grp,Elm,subf)
fld = fld+1;
switch propversion
    case {0,2}
        % infile
        if isempty(loc3d)
            dimflag = [1 0 1 1 0];
        else
            dimflag = [1 0 1 1 1];
        end
        if propversion==0
            DataProps(fld,:) = {ediscr eunit dimflag 1 1 '' 'z' 'z' loc3d Grp Elm '' subf 0};
        else
            DataProps(fld,:) = {ediscr eunit dimflag 1 1 'sQUAD' 'xy'     '' 'z' 'z' loc3d Grp Elm '' subf 0};
        end
    case 1
        % quantities
        subf = sprintf('%i',subf);
        switch loc3d
            case 'c'
                Stagger = 'Voxels3D';
                VCoord = 'F';
            case 'i'
                Stagger = 'HFaces3D';
                VCoord = 'F';
            otherwise
                Stagger = 'Faces2D';
                VCoord = '';
        end
        DataProps(fld,:) = {ediscr eunit 'float' 'Time' Stagger subf VCoord Grp Elm ''};
end

function [DataProps,fld] = addsep(DataProps,fld,propversion)
switch propversion
    case 0
        fld = fld+1;
        DataProps(fld,:)={'-------'                       ''       [0 0 0 0 0]  0         0    ''        ''    ''        ''      ''           ''        ''       []       0};
    case 2
        fld = fld+1;
        DataProps(fld,:)={'-------'                       ''       [0 0 0 0 0]  0         0    ''      ''       ''        ''    ''        ''      ''           ''        ''       []       0};
    otherwise
        % no separators
end
