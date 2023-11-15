function varargout=qp_unitconversion(varargin)
%QP_UNITCONVERSION Convert unit strings.
%   SystemList = QP_UNITCONVERSION('systems') returns the unit systems
%   supported. Currently these are SI, CGS, FPS, IPS, NMM.
%
%   UnitList = QP_UNITCONVERSION('units') returns a list of all supported
%   elementary unit strings. Unit strings may be combined of any
%   combination of units. E.g. 'km/h', 'ft/s', 'N*s/kg'.
%
%   QP_UNITCONVERSION(UStr1,UStr2,TempType) displays a conversion table
%   for transforming quantities expressed in UStr1 into UStr2 and vice
%   versa. The TempType can be either 'absolute' or 'relative' (the latter
%   being the default value). For temperatures you should use 'absolute' to
%   convert actual temperatures and use 'relative' to convert temperature
%   differences. In the latter case the offsets aren't used. The TempType
%   can be specified at any argument location, e.g. TempType,UStr1,UStr2 is
%   also allowed.
%
%   C = QP_UNITCONVERSION(UStr,UStr2,TempType) returns the conversion
%   factor needed for the conversion of quantities expressed in UStr1 into
%   UStr2. If TempType equals 'absolute' and the unit contains an offset
%   then C will be a 1x2 array. The conversion rule will in that case be
%    [Q in UStr2] = C(1) * ([Q in UStr1] + C(2))
%   That is, the second value is the offset. In all other cases C will be
%   just the scalar conversion factor (no offset).
%
%   QP_UNITCONVERSION(UStr1,System,TempType) displays a conversion table
%   for transforming quantities expressed in UStr1 into the equivalent in
%   the selected unit system and vice versa. The default System is SI.
%
%   [ConversionFactor,UStr2] = QP_UNITCONVERSION(UStr1,System,TempType)
%   returns the factor needed for the conversion of quantities expressed in
%   unit1 into the equivalent in the selected unit system and returns the
%   unit string UStr2 in that system as well.
%
%   [DATAOUT1,DATAOUT2,...] = 
%        QP_UNITCONVERSION(UStr1,UStr2,TempType,DATAIN1,DATAIN2,...)
%   converts the data provided by DATAIN1, DATAIN2, ... in UStr1 unit into
%   UStr2 units.

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

persistent unittableread
if isempty(unittableread)
    initialize_unittable;
    unittableread=1;
end

unitsystems={'SI','CGS','FPS','IPS','NMM'};
PhysQuant ={'length','time','electric_current','mass','luminous_intensity','amount_of_substance','temperature','angle'};
if nargout>0
    varargout=cell(1,nargout);
end
Out=[];

TempType = RELATIVE;
arguments = varargin;
for i=1:length(arguments)
    if ischar(arguments{i})
        switch lower(arguments{i})
            case {'absolute'}
                TempType = ABSOLUTE;
                arguments(i) = [];
                break
            case {'relative'}
                TempType = RELATIVE;
                arguments(i) = [];
                break
            case {'unspecified'}
                TempType = UNSPECIFIED;
                arguments(i) = [];
                break
        end
    end 
end

unit1=arguments{1};
if length(arguments)>1
    unit2=arguments{2};
else
    unit2='SI';
end

if isempty(unit1)
    unit1='-';
end

if nargin==1 && strcmpi(unit1,'systems')
    varargout={unitsystems};
elseif nargin==1 && strcmpi(unit1,'units')
    varargout=search_elem('retrievetable');
else
    if isempty(unit2)
        unit2='-';
    end
    if isequal(unit1,unit2)
        convfactor=[0 1];
        if nargin<=2
            varargout{1}=convfactor(2); % maintain temporarily backward compatibility
            varargout{2}=unit2;
            varargout{3}=zeros(1,8);
        else
            varargout=varargin;
        end
    else
        if isequal(unit1,'-')
            factor1=[0 1];
            SI1=zeros(1,8);
        else
            [factor1,SI1]=parse(unit1,TempType);
        end
        if ischar(factor1)
            Out=factor1;
            SI2=NaN;
        else
            if isequal(unit2,'-')
                factor2=[0 1];
                SI2=zeros(1,8);
            elseif ~isempty(strmatch(unit2,unitsystems,'exact'))
                SI2=SI1;
                SIUnits={'m','s','A','kg','cd','mol','K','deg'};
                switch unit2
                    case 'SI'
                        Units=SIUnits;
                    case 'CGS'
                        Units={'cm','s','A','g','cd','mol','K','deg'};
                    case 'FPS'
                        Units={'ft','s','A','lb','cd','mol','K','deg'};
                    case 'IPS'
                        Units={'in','s','A','lb','cd','mol','K','deg'};
                    case 'NMM'
                        Units={'mm','s','A','g','cd','mol','K','deg'};
                    otherwise
                        error('Unit system %s not yet implemented.',unit2)
                end
                factor2=[0 1];
                if ~strcmp(unit2,'SI')
                    for i=1:length(Units)
                        sifactor = search_elem(Units{i},TempType);
                        factor2(2)=factor2(2)*sifactor(2)^SI2(i);
                    end
                end
                unit2=dispunit(SI2,Units);
                if isempty(unit2)
                    unit2='-';
                end
            else
                [factor2,SI2]=parse(unit2,TempType);
            end
            if ischar(factor2)
                Out=factor2;
            end
        end
        if ~isempty(Out)
            varargout={Out ''};
        elseif ~isequal(SI1,SI2)
            SI1unit=dispunit(SI1);
            if isempty(SI1unit)
                SI1unit='-';
            end
            if ~isequal(unit1,SI1unit)
                unit1=sprintf('%s (in SI base units: %s)',unit1,SI1unit);
            end
            SI2unit=dispunit(SI2);
            if isempty(SI2unit)
                SI2unit='-';
            end
            if ~isequal(unit2,SI2unit)
                unit2=sprintf('%s (in SI base units: %s)',unit2,SI2unit);
            end
            varargout={sprintf('Incompatible units: %s cannot be converted into %s.',unit1,unit2) ''};
        else
            convfactor=factor1(2)/factor2(2);
            offset=0;
            if factor1(1)~=0 || factor2(1)~=0
                offset=factor1(1)-factor2(1)/convfactor;
            end
            if length(arguments)<=2
                if nargout==0
                    cf='';
                    if convfactor~=1
                        cf=sprintf('%g ',convfactor);
                    end
                    xunit1=sprintf('[quantity in %s]',unit1);
                    if offset~=0
                        sign='+';
                        dispoffset=offset;
                        if dispoffset<0
                            sign='-';
                            dispoffset=-dispoffset;
                        end
                        xunit1=sprintf('%s %c %g',xunit1,sign,dispoffset);
                        if ~isempty(cf)
                            xunit1=['(' xunit1 ')'];
                        end
                    end
                    %
                    f1 = [0 1 2 50 100];
                    f2 = [f1/convfactor-offset;f1];
                    f1(2,:) = convfactor*(f1+offset);
                    fprintf('[quantity in %s] = %s%s\n',unit2,cf,xunit1);
                    for f = unique([f1 f2]','rows')'
                        fprintf('%10g %s = %10g %s\n',f(1),unit1,f(2),unit2)
                    end
                else
                    if TempType==ABSOLUTE && offset~=0
                        varargout={[convfactor offset] unit2 SI2};
                    else
                        varargout={convfactor unit2 SI2};
                    end
                end
            else
                for i=1:length(arguments)-2
                    if isstruct(arguments{i+2})
                        data=arguments{i+2};
                        flds={'Val','XComp','YComp','ZComp'};
                        for fldi=1:length(flds)
                            fld=flds{fldi};
                            if isfield(data,fld)
                                for d=1:length(data)
                                    Temp=convfactor*(getfield(data(d),fld)+offset);
                                    data(d)=setfield(data(d),fld,Temp);
                                end
                            end
                        end
                        [data(:).Units]=deal(unit2);
                        varargout{i}=data;
                    else
                        varargout{i}=convfactor*(arguments{i+2}+offset);
                    end
                end
            end
        end
    end
end
if nargout>2
    SI = [PhysQuant;num2cell(varargout{3})];
    varargout{3} = struct(SI{:});
end


function Str=dispunit(SI,Units)
if nargin==1
    Units={'m','s','A','kg','cd','mol','K','deg'};
end
Str='';
if all(SI==0)
    Str='';
    return
end
for i=1:length(SI)
    if SI(i)>0
        if SI(i)==1
            Str=[Str '*' Units{i}];
        else
            Str=[Str '*' Units{i} '^' num2str(SI(i))];
        end
    end
end
if isempty(Str)
    Str=' 1';
end
for i=1:length(SI)
    if SI(i)<0
        if SI(i)==-1
            Str=[Str '/' Units{i}];
        else
            Str=[Str '/' Units{i} '^' num2str(-SI(i))];
        end
    end
end
if ~isempty(Str)
    Str=Str(2:end);
end


function unit = powercheck(unit,k)
if isequal(unit(k),'*') && k<length(unit) && isequal(unit(k+1),'*')
    % two asteriks form a power operator instead of a multiplication operator.
    unit(k+1) = [];
    unit(k) = '^';
end


function [factor1,si1]=factor_combine(cmd,factor1,si1,factor2,si2)
switch cmd
    case {'*','·'}
        factor1(1)=factor1(1)*factor2(2)+factor1(2)*factor2(1);
        factor1(2)=factor1(2)*factor2(2);
        si1=si1+si2;
    case '/'
        if factor2(1)~=0
            error('Unable to divide by offset')
        end
        factor1(1)=factor1(1)/factor2(2);
        factor1(2)=factor1(2)/factor2(2);
        si1=si1-si2;
end


function [factor1,si1]=parse(unit,TempType)
%fprintf('Parsing: %s\n',unit);
factor1=[0 1];
si1=zeros(1,8);
unit=strtrim(unit);

nob=0;
prevcmd='*';
ki=1;
if isempty(unit)
    return
end
k=1;
while k<=length(unit)+1
    if k<=length(unit)
        unit = powercheck(unit,k);
        unitk = unit(k);
    else
        unitk = '*';
    end
    switch unitk
        case '('
            if nob==0 && ~isempty(deblank(unit(ki:k-1)))
                % implicitly insert a multiply before
                [factor2,si2]=parse(unit(ki:k-1));
                [factor1,si1]=factor_combine(prevcmd,factor1,si1,factor2,si2);
                prevcmd='*';
            end
            nob=nob+1;
            if nob==1
                ki = k;
            end
        case ')'
            nob=nob-1;
            if nob==0
                [factor2,si2]=parse(unit(ki+1:k-1));
                if k<length(unit)
                    unit = powercheck(unit,k);
                    unitk = unit(k);
                end
            elseif nob<0
                break
            end
        case {'*','·','/',' '}
            if nob==0
                if unitk==' '
                    % SPACE ... The final frontier ...
                    %
                    % this could be just a space in a long unit name
                    % or this could represent an implicit multiplication *
                    %
                    % since the long unit name might be something like
                    % "minute of arc" or "degrees celsius" we need to first
                    % check the long name before going down the wrong path
                    % of interpreting the first part as a separate unit
                    % "minute" or "degrees".
                    %
                    [unit,k,factor2,si2] = checkspace(unit,ki,k,TempType);
                    if k<=length(unit)
                        unitk = unit(k);
                        if unitk~='/'
                            unitk = '*';
                        end
                    else
                        unitk = '*';
                    end
                else
                    [factor2,si2] = findfirst(unit(ki:k-1),TempType);
                end
                if isequal(factor2,-1)
                    error('Unable to interpret unit "%s"',unit(ki:k-1))
                elseif factor1(1)~=0 || (factor2(1)~=0 && any(si1~=0))
                    error('Cannot multiply units with offset for %s', unit(ki:k-1))
                else
                    [factor1,si1]=factor_combine(prevcmd,factor1,si1,factor2,si2);
                end
                prevcmd=unitk;
                ki=k+1;
            end
        otherwise
    end
    k=k+1;
end


function [unit,k,factor1,si1] = checkspace(unit,ki,k,TempType)
% We started parsing the "unit" at position ki. We encountered a space at
% position k. Now determine whether this should be interpreted as just a
% space in a long unit name or as an implicit multiplication *.
for k2 = k+1:length(unit)
    unit = powercheck(unit,k2);
    switch unit(k2)
        case {'*','·','/','(',')'}
            k2 = k2-1;
            break
    end
end
% tropical year light year2
% tropical year light year^2
%
[factor1,si1,kb] = findfirst(unit(ki:k2),TempType);
if isequal(si1,-1)
    error('Unable to interpret unit "%s"',unit(ki:k2))
end
k = ki-1+kb;


function [factor,si,kb] = findfirst(unit,TempType)
% Check which unit string is just a number.
kp = length(unit);
kb = kp+1;
pow = str2double(unit);
if ~isnan(pow)
    factor = [0 pow];
    si  = 0;
    return
end
% Check whether unit strings ends on a number.
pow = [];
switch unit(end)
    case '¹'
        pow = 1;
        kp = kp-1;
    case '²'
        pow = 2;
        kp = kp-1;
    case '³'
        pow = 3;
        kp = kp-1;
    case '¼'
        pow = 0.25;
        kp = kp-1;
    case '½'
        pow = 0.5;
        kp = kp-1;
    case '¾'
        pow = 0.75;
        kp = kp-1;
    otherwise
        number = ismember(unit,'1234567890');
        if number(end)
            if all(number)
                kp = 0;
            else
                kp = max(find(~number));
            end
            pow = str2double(unit(kp+1:end));
        end
end
if ~isempty(pow)
    if kp>0 && unit(kp)=='-'
        kp = kp-1;
        pow = -pow;
    end
    if kp>1 && unit(kp)=='^'
        kp = kp-1;
    end
else
    kp = length(unit);
end
if kp==0
    % shouldn't happen ... already checked at start of routine!
else
    [factor,si] = search_elem(unit(1:kp),TempType);
end
if isequal(si,-1)
    spaces = find(unit==' ');
    if ~isempty(spaces)
        [factor,si,kb] = findfirst(deblank(unit(1:spaces(end))),TempType);
    end
elseif ~isempty(pow)
    if factor(1)~=0
        error('offset power')
    end
    factor(2) = factor(2)^pow;
    si = si*pow;
end


function [factor,si]=search_elem(unit,TempType)
persistent unittable
factor=[0 1];
si='';
if isequal(unit,'newtable')
    unittable = TempType;
    return
elseif isequal(unit,'retrievetable')
    factor = unittable;
    return
end
i=strmatch(unit,unittable{1},'exact');
LocalTempType = TempType;
if isempty(i) && length(unit)>5 && strcmpi(unit(end-4:end),' abs.') % this is more generic than temperature ...
    LocalTempType = ABSOLUTE;
    unit=unit(1:end-5);
    i=strmatch(unit,unittable{1},'exact');
end
%
prefix=1;
if isempty(i)
    [v,n,e]=sscanf(unit,'%f',2);
    if n==1 && isempty(e)
        factor=[0 v];
        si=zeros(1,8);
    else
        prefixfound=0;
        pref='';
        if length(unit)>3
            j=strmatch(unit(1:3),{'exa'},'exact');
            if ~isempty(j)
                scale=1e18;
                prefixfound=1;
                prefix=scale(j);
                pref=unit(1:3);
                unit=unit(4:end);
            end
        end
        if ~prefixfound && length(unit)>4
            j=strmatch(unit(1:4),{'peta','tera','giga','mega','kilo','deca','deci','nano','pico','atto'},'exact');
            if ~isempty(j)
                scale=[1e15 1e12 1e9 1e6 1e3 10 1/10 1e-9 1e-12 1e-18];
                prefixfound=1;
                prefix=scale(j);
                pref=unit(1:4);
                unit=unit(5:end);
            end
        end
        if ~prefixfound && length(unit)>5
            j=strmatch(unit(1:5),{'ronna','yotta','zetta','hecto','centi','milli','micro','femto','zepto','yocto','ronto'},'exact');
            if ~isempty(j)
                scale=[1e27 1e24 1e21 100 1/100 1e-3 1e-6 1e-15 1e-21 1e-24 1e-27];
                prefixfound=1;
                prefix=scale(j);
                pref=unit(1:5);
                unit=unit(6:end);
            end
        end
        if ~prefixfound && length(unit)>6
            j=strmatch(unit(1:6),{'quetta','quecto'},'exact');
            if ~isempty(j)
                scale=[1e30 1e-30];
                prefixfound=1;
                prefix=scale(j);
                pref=unit(1:6);
                unit=unit(7:end);
            end
        end
        if ~prefixfound && length(unit)>2
            j=strmatch(unit(1:2),{'da'},'exact');
            if ~isempty(j)
                scale=[10];
                prefixfound=1;
                prefix=scale(j);
                pref=unit(1:2);
                unit=unit(3:end);
            end
        end
        if ~prefixfound && length(unit)>1
            j=find(unit(1)=='QRYZEPTGMkhdcmµunpfazyrq');
            if ~isempty(j)
                scale=[1e30 1e27 1e24 1e21 1e18 1e15 1e12 1e9 1e6 1e3 10 1e-1 1e-2 1e-3 1e-6 1e-6 1e-9 1e-12 1e-15 1e-18 1e-21 1e-24 1e-27 1e-30];
                prefixfound=1;
                prefix=scale(j);
                pref=unit(1);
                unit=unit(2:end);
            end
        end
        if prefixfound
            i=strmatch(unit,unittable{1},'exact');
        end
        if isempty(i)
            % no match ...
            factor = -1;
            si = -1;
            return
        end
    end
end
i=unittable{2}(i);
factor=[0 unittable{3}(i,2)*prefix];
if LocalTempType==ABSOLUTE
    factor(1)=unittable{3}(i,1);
elseif LocalTempType==UNSPECIFIED && unittable{3}(i,1)~=0
    error('Unable to convert unit because its unknown whether "%s" should be interpreted relative or absolute.',unit)
end
si=unittable{4}(i,:);


function initialize_unittable
table=basisunittable;
search_elem('newtable',table);
%
filename=[qp_basedir('exe') filesep 'units.ini'];
fid=fopen(filename,'r');
if fid<0
    % for ease of debugging ...
    filename='units.ini';
    fid=fopen(filename,'r');
    if fid<0
        ui_message('error','Unit conversion table not found.')
        return
    end
end
fclose(fid);
%
UNIT=inifile('open',filename);
units=inifile('chapters',UNIT);
identified=zeros(1,length(units));
nNames=size(table{1},1);
anychange=1;
while anychange && any(~identified)
    anychange=0;
    i_notidentified=find(~identified);
    for i=i_notidentified
        j=size(table{4},1);
        Def=inifile('get',UNIT,i,'definition','');
        [Names,iChap]=inifile('cgetstring',UNIT,i,'name');
        if isempty(Names)
            identified(i)=1;
            if ~strcmpi(units{i},'general')
                ui_message('', ...
                    'No names in chapter #%i: %s', ...
                    i,units{i})
            end
        elseif isempty(Def)
            identified(i)=1;
            ui_message('', ...
                'No definition for %s in chapter #%i: %s', ...
                list2string(Names),i,units{i})
        else
            if ischar(Def)
                try
                    [factor,si]=parse(Def,true);
                catch
                    factor = 'failed';
                end
            else
                factor(2)=Def;
                si=zeros(1,8);
            end
            if ~ischar(factor)
                j=j+1;
                nNewNames=length(Names);
                n=1;
                while n<nNewNames
                    ii=strmatch(Names{n},table{1},'exact');
                    if ~isempty(ii)
                        ii=table{2}(ii);
                        ui_message('', ...
                            '%s defined multiple times, using definition: %g %s', ...
                            Names{n},table{3}(ii,2),dispunit(table{4}(ii,:)))
                        Names(n)=[];
                        nNewNames=nNewNames-1;
                    else
                        n=n+1;
                    end
                end
                identified(i)=1;
                anychange=1;
                factor(1)=inifile('get',UNIT,i,'absoffset',0);
                table{3}(j,:)=factor;
                table{4}(j,:)=si;
                table{1}(nNames+(1:nNewNames))=Names;
                table{2}(nNames+(1:nNewNames))=j;
                nNames=nNames+nNewNames;
            end
        end
        search_elem('newtable',table);
    end
end
if any(~identified)
    i_notidentified=find(~identified);
    for i=i_notidentified
        Def=inifile('get',UNIT,i,'definition','');
        Names=inifile('get',UNIT,i,'name','');
        if ~iscell(Names)
            Names={Names};
        end
        ui_message('', ...
            'Cannot understand definition ''%s''\ntherefore the definition of %s has been cancelled.', ...
            Def,list2string(Names))
    end
end


function strNames=list2string(Names)
if ~iscell(Names)
    strNames=Names;
    return
end
if length(Names)>1
    strNames=sprintf('%s, ',Names{1:end-1});
    strNames(end-1:end)=[];
    strNames=sprintf('%s and %s',strNames,Names{end});
else
    strNames=Names{1};
end


function table=basisunittable
table={'m'   [0 1]          [1 0 0 0 0 0 0 0]
    'ft'     [0 0.3048]     [1 0 0 0 0 0 0 0]
    'in'     [0 0.3048/12]  [1 0 0 0 0 0 0 0]
    's'      [0 1]          [0 1 0 0 0 0 0 0]
    'A'      [0 1]          [0 0 1 0 0 0 0 0]
    'g'      [0 0.001]      [0 0 0 1 0 0 0 0]
    'lb'     [0 0.45359237] [0 0 0 1 0 0 0 0]
    'cd'     [0 1]          [0 0 0 0 1 0 0 0]
    'mol'    [0 1]          [0 0 0 0 0 1 0 0]
    'K'      [0 1]          [0 0 0 0 0 0 1 0]
    'deg'    [0 1]          [0 0 0 0 0 0 0 1]
    'radian' [0 180/pi]     [0 0 0 0 0 0 0 1]};
table={table(:,1) (1:size(table,1))' cat(1,table{:,2}) cat(1,table{:,3})};


function v = RELATIVE
v = -1;

function v = ABSOLUTE
v = 1;

function v = UNSPECIFIED
v = 0;