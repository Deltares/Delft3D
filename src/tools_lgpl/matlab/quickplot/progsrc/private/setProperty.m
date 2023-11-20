function [OPT Set Default] = setproperty(OPT, inputCell, varargin)
% SETPROPERTY  Generic routine to set values in PropertyName-PropertyValue pairs
%
% Routine to set properties based on PropertyName-PropertyValue 
% pairs (aka <keyword,value> pairs). Can be used in any function 
% where PropertyName-PropertyValue pairs are used.
%   
%
% [OPT Set Default] = setproperty(OPT, inputCell, varargin)
%
% input:
% OPT       = structure in which fieldnames are the keywords and the values 
%             are the defaults 
% inputCell = must be a cell array containing single struct, or a set of
%             property-value pairs. It is best practice to pass the varargin 
%             of the calling function as the second argument.  
%             property names must be strings. If they contain a . this is
%             interpreted a field separator. This can be used to assign 
%             properties on a subfield level.
% varargin  = series of PropertyName-PropertyValue pairs to set methods for
%             setproperty itself
%           
% output:
% OPT     = structure, similar to the input argument OPT, with possibly
%           changed values in the fields
% Set     = structure, similar to OPT, values are true where OPT has been 
%           set (and possibly changed)
% Default = structure, similar to OPT, values are true where the values of
%           OPT are equal to the original OPT
%
% Example calls:
% 
% [OPT Set Default] = setproperty(OPT, vararginOfCallingFunction)
% [OPT Set Default] = setproperty(OPT, {'PropertyName', PropertyValue,...})
% [OPT Set Default] = setproperty(OPT, {OPT2})
% [OPT Set Default] = setproperty(OPT,  OPT2)
% [OPT Set Default] = setproperty(OPT,  OPT2 ,<'PropertyName', PropertyValue)
%
%     Different methods for dealing with class changes of variables, or 
%     extra fields (properties that are not in the input structure) can 
%     be defined as property-value pairs. Valid properties are
%     onExtraField and onClassChange:
%
% PROPERTY       VALUE
% onClassChange: ignore          ignore (default)
%                warn            throw warning
%                error           throw error
% onExtraField:  silentIgnore    silently ignore the field
%                warnIgnore      ignore the field and throw warning      
%                silentAppend    silently append the field to OPT
%                warnAppend      append the field to OPT and throw warning 
%                error           throw error (default)
%
% Example calls:
%
% [OPT Set Default] = setproperty(OPT, vararginOfCallingFunction,'onClassChange','warn')
% [OPT Set Default] = setproperty(OPT, {'PropertyName', PropertyValue},'onExtraField','silentIgnore')
%
%
% Example: 
%
% +------------------------------------------->
% function y = dosomething(x,'debug',1)
% OPT.debug  = 0;
% OPT        = setproperty(OPT, varargin);
% y          = x.^2;
% if OPT.debug; plot(x,y);pause; end
% +------------------------------------------->
%
% legacy syntax is also supported, but using legacy syntax prohibits the 
% setting of onClassChange and onExtraField methods:
%
% [OPT Set Default] = setproperty(OPT, varargin{:})
%  OPT              = setproperty(OPT, 'PropertyName', PropertyValue,...)
%  OPT              = setproperty(OPT, OPT2)
%
% input:
% OPT      = structure in which fieldnames are the keywords and the values are the defaults 
% varargin = series of PropertyName-PropertyValue pairs to set
% OPT2     = is a structure with the same fields as OPT. 
%
%            Internally setproperty translates OPT2 into a set of
%            PropertyName-PropertyValue pairs (see example below) as in:
%            OPT2    = struct( 'propertyName1', 1,...
%                              'propertyName2', 2);
%            varcell = reshape([fieldnames(OPT2)'; struct2cell(OPT2)'], 1, 2*length(fieldnames(OPT2)));
%            OPT     = setproperty(OPT, varcell{:});
%
% Change log:
%    2011-09-30: full code rewrite to include:
%                 - setpropertyInDeeperStruct functionality
%                 - user defined handling of extra fields 
%                 - class change warning/error message
%
% See also: VARARGIN, STRUCT, MERGESTRUCTS

%% Copyright notice
%   --------------------------------------------------------------------
%   Copyright (C) 2009-2023 Delft University of Technology
%       C.(Kees) den Heijer
%
%       C.denHeijer@TUDelft.nl	
%
%       Faculty of Civil Engineering and Geosciences
%       P.O. Box 5048
%       2600 GA Delft
%       The Netherlands
%
%   This library is free software; you can redistribute it and/or
%   modify it under the terms of the GNU Lesser General Public
%   License as published by the Free Software Foundation; either
%   version 2.1 of the License, or (at your option) any later version.
%
%   This library is distributed in the hope that it will be useful,
%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%   Lesser General Public License for more details.
%
%   You should have received a copy of the GNU Lesser General Public
%   License along with this library; if not, write to the Free Software
%   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
%   USA
%   or http://www.gnu.org/licenses/licenses.html, http://www.gnu.org/, http://www.fsf.org/
%   --------------------------------------------------------------------

% This tools is part of <a href="http://OpenEarth.Deltares.nl">OpenEarthTools</a>.
% OpenEarthTools is an online collaboration to share and manage data and 
% programming tools in an open source, version controlled environment.
% Sign up to recieve regular updates of this function, and to contribute 
% your own tools.

%% Version <http://svnbook.red-bean.com/en/1.5/svn.advanced.props.special.keywords.html>
% Created: 26 Feb 2009
% Created with Matlab version: 7.4.0.287 (R2007a)

% $Id$
% $Date$
% $Author$
% $Revision$
% $HeadURL$
% $Keywords: $

%% shortcut function if there is nothing to set (1)
if nargin==1||isempty(inputCell)
    if nargout > 1 % process Set
        flds1         = fieldnames(OPT);
        Set = [flds1,repmat({false},size(flds1))]';
        Set = struct(Set{:});
    end
    if nargout > 2 % process Default
        Default = [flds1,repmat({true},size(flds1))]';
        Default = struct(Default{:});
    end
    return
end

%% check and parse inputCell (usually the varargin struct in the calling function)
% determine mode from class of the second argument
switch(class(inputCell))
    case 'struct'
        % legacy syntax mode
        % turn struct into <keyword,value> pairs and append rest of varargin
        inputCell = struct2arg(inputCell);
        if ~isempty(varargin)
            inputCell = {inputCell{:},varargin{:}};
        end
    case 'char'
        % legay syntax mode
        inputCell = [{inputCell} varargin];
        varargin  = {};
    case 'cell'
        % new syntax mode, do nothing
    otherwise
        error('SETPROPERTY:inputCell',...
            'Second input must be a cell, (or a char or struct for legacy syntax)')
end


%% shortcut function if there is nothing to set (2)
if numel(inputCell) == 1 && isempty(inputCell{1})
    if nargout > 1 % process Set
        flds1         = fieldnames(OPT);
        Set = [flds1,repmat({false},size(flds1))]';
        Set = struct(Set{:});
    end
    if nargout > 2 % process Default
        Default = [flds1,repmat({true},size(flds1))]';
        Default = struct(Default{:});
    end
    return
end

if odd(length(inputCell))
    %then length must be 1
    if length(inputCell) == 1
        %then the inputCell must be a structure
        if ~isstruct(inputCell{1})
            error('SETPROPERTY:inputCell',...
                'Second argument inputCell must be a cell containing a single struct or property/value pairs')
        end
        flds2  = fieldnames(inputCell{1})';
        newVal = struct2cell(inputCell{1})';
    else
        error('SETPROPERTY:inputCell',...
            'Second argument inputCell must be a cell containing a single struct or property/value pairs')
    end
else
    flds2  = inputCell(1:2:end);
    if ~all(cellfun(@ischar,flds2))
        error('SETPROPERTY:inputCell',...
            'Second argument inputCell, property names should be strings')
    end
    newVal = inputCell(2:2:end);
end


%% check and parse varargin

if length(varargin)>4
    error('Too many input arguments.')
end
if odd(length(varargin))
     error('SETPROPERTY:varargin',...
                        'Set onClassChange and onExtraField with the varargin, as keyword value pairs');
end

onExtraField       = 'error';
onClassChange      = 'ignore';
if ~isempty(varargin)
    for ii = 1:2:length(varargin)
        switch varargin{ii}
            case 'onClassChange'
                if ischar(varargin{ii+1}) && ismember(varargin(ii+1),...
                        {'ignore','warn','error'})
                    onClassChange = varargin{ii+1};
                else
                    error('SETPROPERTY:InvalidMethod',...
                        'Valid methods for onClassChange are: ignore, warn and error');
                end
            case 'onExtraField'
                if ischar(varargin{ii+1}) && ismember(varargin(ii+1),...
                        {'silentAppend','warnAppend','silentIgnore','warnIgnore','error'})
                    onExtraField = varargin{ii+1};
                else
                    error('SETPROPERTY:InvalidMethod',...
                        'Valid methods for onExtraField are: append, silentIgnore, warnIgnore, error');
                end
            otherwise
                error('SETPROPERTY:InvalidKeyword', ...
                    'Supported keywords are onClassChange and onExtraField');
        end
    end
end

switch onClassChange
    case 'warn'
        warnOnClassChange  = true;
        errorOnClassChange = false;
    case 'error'
        warnOnClassChange  = false;
        errorOnClassChange = true;
    case 'ignore'
        warnOnClassChange  = false;
        errorOnClassChange = false;
end

%% copy the original OPT only if Default has to be returned
if nargout > 2
    OPToriginal = OPT;
end
%% check field names, and distinguish between field to set and extra fields
% flds1 contains field names of OPT
flds1         = fieldnames(OPT);

% fldsCell contains field names to assign, split per subfield
fldsCell      = regexp(flds2,'[^\.]*','match');
fldsCellLen   = cellfun(@length,fldsCell);
% check if all field names are valid to assign
allFieldNames  = [fldsCell{:}];
validFieldName = cellfun(@isvarname,allFieldNames);
if ~all(validFieldName)
    error('SETPROPERTY:invalidFieldName',...
        '\nThe following field name is not valid: %s',allFieldNames{~validFieldName});
end

% identify fldsToSet for simple field names (without subfields)

% fldsToSet     = ismember(flds2,flds1); 
% As ismember is rather slow, this is replaced by more optimized code

fldsToSet = false(size(flds2));
for ii = find(fldsCellLen==1)
    fldsToSet(ii) = any(strcmp(flds2(ii),flds1));
end

% identify fldsToSet for field names with subfields)
for ii = find(fldsCellLen>1)
    fldsToSet(ii) = isfield2(OPT,fldsCell{ii});
end


% all other fields are either to be set, or extra
fldsExtra     = ~fldsToSet;

%% process fldsToSet
if any(fldsToSet)
    for ii = find(fldsToSet)
        switch length(fldsCell{ii})
            case 1
                if warnOnClassChange || errorOnClassChange
                    class1 = class(OPT.(fldsCell{ii}{1}));
                    class2 = class(newVal{ii});
                    classChange(flds2{ii},class1,class2,warnOnClassChange,errorOnClassChange);
                end
                OPT.(fldsCell{ii}{1}) = newVal{ii};
            case 2
                if warnOnClassChange || errorOnClassChange
                    class1 = class(OPT.(fldsCell{ii}{1}).(fldsCell{ii}{2}));
                    class2 = class(newVal{ii});
                    classChange(flds2{ii},class1,class2,warnOnClassChange,errorOnClassChange);
                end
                OPT.(fldsCell{ii}{1}).(fldsCell{ii}{2}) = newVal{ii};
            otherwise
                if warnOnClassChange || errorOnClassChange
                    class1 = class(getfield(OPT,fldsCell{ii}{:}));
                    class2 = class(newVal{ii});
                    classChange(flds2{ii},class1,class2,warnOnClassChange,errorOnClassChange);
                end
                OPT = setfield(OPT,fldsCell{ii}{:},newVal{ii});
        end
    end
end

%% deal with fldsExtra
if any(fldsExtra)
    switch lower(onExtraField)
        case {'silentappend','warnappend'}
            % append extra fields to OPT
            for ii = find(fldsExtra)
                switch length(fldsCell{ii})
                    case 1
                        OPT.(flds2{ii}) = newVal{ii};
                    otherwise
                        OPT = setfield(OPT,fldsCell{ii}{:},newVal{ii});
                end
            end
            if strcmpi(onExtraField,'warnAppend')
                % throw a warning
                warning('SETPROPERTY:ExtraField',...
                '\nThe following field is Extra and appended to OPT: %s',flds2{fldsExtra});
            end
        case 'silentignore'
            % do nothing, silently ignore extra fields
        case 'warnignore'
            warning('SETPROPERTY:ExtraField',...
                '\nThe following field is Extra and thus ignored: %s',flds2{fldsExtra});
        case 'error'
            error('SETPROPERTY:ExtraField',...
                '\nThe following field is Extra: %s',flds2{fldsExtra});
    end
end

%% assign Output
if nargout > 1 % process Set
    Set = [flds1,repmat({false},size(flds1))]';
    Set = struct(Set{:});
    if any(fldsToSet)
        for ii = find(fldsToSet)
            switch length(fldsCell{ii})
                case 1
                    Set.(fldsCell{ii}{1}) = true;
                otherwise % this routine is rather slow, would be nice to add case 2 to cover the most common calls
                    for nn = 1:length(fldsCell{ii})-1
                        if ~isstruct(getfield(Set,fldsCell{ii}{1:nn})) %#ok<*GFLD>
                            substruct = [fieldnames(getfield(OPT,fldsCell{ii}{1:nn})),...
                                repmat({false},size(fieldnames(getfield(OPT,fldsCell{ii}{1:nn}))))]';
                            Set = setfield(Set,fldsCell{ii}{1:nn},[]); %#ok<*SFLD>
                            Set = setfield(Set,fldsCell{ii}{1:nn},struct(substruct{:}));
                        end  
                    end
                    Set = setfield(Set,fldsCell{ii}{:},true);
            end
        end
    end
    if any(fldsExtra)
        switch lower(onExtraField)
            case {'silentappend','warnappend'}
                for ii = find(fldsExtra)
                    switch length(fldsCell{ii})
                        case 1
                            Set.(flds2{ii}) = true;
                        otherwise
                            Set = setfield(Set,fldsCell{ii}{:},true);
                    end
                end
        end
    end
end

if nargout > 2 % process Default
    Default = [flds1,repmat({true},size(flds1))]';
    Default = struct(Default{:});
    if any(fldsToSet)
        for ii = find(fldsToSet)
            switch length(fldsCell{ii})
                case 1
                    Default.(fldsCell{ii}{1}) = ...
                        isequalwithequalnans(OPToriginal.(fldsCell{ii}{1}),OPT.(fldsCell{ii}{1}));
                otherwise % this routine is rather slow, would be nice to add case 2 to cover the most common calls
                    for nn = 1:length(fldsCell{ii})-1
                        if ~isstruct(getfield(Default,fldsCell{ii}{1:nn})) %#ok<*GFLD>
                            substruct = [fieldnames(getfield(OPT,fldsCell{ii}{1:nn})),...
                                repmat({true},size(fieldnames(getfield(OPT,fldsCell{ii}{1:nn}))))]';
                            Default = setfield(Default,fldsCell{ii}{1:nn},[]); %#ok<*SFLD>
                            Default = setfield(Default,fldsCell{ii}{1:nn},struct(substruct{:}));
                        end  
                    end
                    Default = setfield(Default,fldsCell{ii}{:},...
                        isequalwithequalnans(getfield(OPToriginal,fldsCell{ii}{:}),getfield(OPT,fldsCell{ii}{:})));
            end
        end
    end
    if any(fldsExtra)
        switch lower(onExtraField)
            case {'silentappend','warnappend'}
                for ii = find(fldsExtra)
                    switch length(fldsCell{ii})
                        case 1
                            Default.(flds2{ii}) = false;
                        otherwise
                            Default = setfield(Default,fldsCell{ii}{:},false);
                    end
                end
        end
    end
end

function tf = isfield2(OPT,fldsCell)
if isfield(OPT,fldsCell{1})
    if length(fldsCell) > 1
        tf = isfield2(OPT.(fldsCell{1}),fldsCell(2:end));
    else
        tf = true;
    end
else
    tf = false;
end

function classChange(fld,class1,class2,warnOnClassChange,errorOnClassChange)
if ~strcmp(class1,class2)
    if warnOnClassChange
        warning('SETPROPERTY:ClassChange', ...
            'Class change of field ''%s'' from %s to %s',...
            fld,class1,class2);
    elseif errorOnClassChange
        error('SETPROPERTY:ClassChange', ...
            'Class change of field ''%s'' from %s to %s not allowed',...
            fld,class1,class2);
    end
end

function out = odd(in)
%ODD   test whether number if odd 
out = mod(in,2)==1;
%% EOF