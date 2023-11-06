function varargout = xparse(cmd,varargin)
%XPARSE Various XML parsing utility routines.
%
%   See also XMLREAD, XMLWRITE.

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

switch cmd
    case 'getMembers'
        [varargout{1:nargout}] = getMembers(varargin{:});
    case 'getName'
        [varargout{1:nargout}] = getName(varargin{:});
    case 'getNodeName'
        [varargout{1:nargout}] = getNodeName(varargin{:});
    case 'getChildren'
        [varargout{1:nargout}] = getChildren(varargin{:});
    case 'getBool'
        [varargout{1:nargout}] = getBool(varargin{:});
    case 'checkName'
        [varargout{1:nargout}] = checkName(varargin{:});
    case 'getNamedChildren'
        [varargout{1:nargout}] = getNamedChildren(varargin{:});
    case 'getNamedChild'
        [varargout{1:nargout}] = getNamedChild(varargin{:});
    case 'getNameSpaces'
        [varargout{1:nargout}] = getNameSpaces(varargin{:});
    case 'getChar'
        [varargout{1:nargout}] = getChar(varargin{:});
    case 'checkNameNS'
        [varargout{1:nargout}] = checkNameNS(varargin{:});
    case 'getNamedChildrenNS'
        [varargout{1:nargout}] = getNamedChildrenNS(varargin{:});
    case 'getRecursiveNamedChild'
        [varargout{1:nargout}] = getRecursiveNamedChild(varargin{:});
    case 'getRecursiveNamedChildNS'
        [varargout{1:nargout}] = getRecursiveNamedChildNS(varargin{:});
    case 'getNamedChildNS'
        [varargout{1:nargout}] = getNamedChildNS(varargin{:});
    case 'getAttribute'
        [varargout{1:nargout}] = getAttribute(varargin{:});
    case 'getAttributeNS'
        [varargout{1:nargout}] = getAttributeNS(varargin{:});
    otherwise
        error('Invalid command "%s".',cmd)
end

function S = getMembers(Node)
S.XML = getChildren(Node);
S.Names = getName(S.XML,true);

function Name = getName(Node,forceCell)
nNode = length(Node);
if nNode==1
    Name = char(Node.getAttribute('key'));
    if nargin>1 && forceCell
        Name = {Name};
    end
else
    Name = cell(1,nNode);
    for i = 1:nNode
        Name{i} = char(Node(i).getAttribute('key'));
    end
end

function Name = getNodeName(Node,forceCell)
nNode = length(Node);
if nNode==1
    Name = char(Node.getNodeName);
    if nargin>1 && forceCell
        Name = {Name};
    end
else
    Name = cell(1,nNode);
    for i = 1:nNode
        Name{i} = char(Node(i).getNodeName);
    end
end

function Children = getChildren(Node)
nChild = Node.getLength;
c = cell(1,nChild);
c{1} = Node.getFirstChild;
for i = 2:nChild
    c{i} = c{i-1}.getNextSibling;
end
Children = [c{:}];

function bool = getBool(XML)
bool = char(XML.getTextContent);
switch bool
    case 'true'
        bool = true;
    case 'false'
        bool = false;
end

function OK = checkName(Item,name)
nodeName = char(Item.getNodeName);
ok = isequal(nodeName,name);
if nargout==0
    if ~ok
        error('Encountered tag <%s> while expecting tag <%s>',nodeName,name)
    end
else
    OK = ok;
end

function Items = getNamedChildren(Parent,name)
AllItems = getChildren(Parent);
for i = length(AllItems):-1:1
    matching(i) = checkName(AllItems(i),name);
end
Items = AllItems(matching);

function Item = getNamedChild(Parent,name)
Items = getChildren(Parent);
for i = 1:length(Items)
    if checkName(Items(i),name)
        Item = Items(i);
        return
    end
end
error('Tag <%s> does not include child tag <%s>',char(Parent.getNodeName),name)

function NS = getNameSpaces(Element)
NS = cell(0,2);
Attribs = Element.getAttributes;
nAtt = Attribs.getLength;
for i = 0:nAtt-1
    AttNam = char(Attribs.item(i).getName);
    if strncmp('xmlns:',AttNam,6)
        NS{end+1,1} = AttNam(7:end);
        NS{end,2}   = char(Attribs.item(i).getValue);
    elseif strcmp('xmlns',AttNam)
        NS{end+1,1} = '';
        NS{end,2}   = char(Attribs.item(i).getValue);
    end
end

function S = getChar(Element)
if isempty(Element)
    S = '';
elseif length(Element)>1
    S = cell(length(Element),1);
    for i = 1:length(Element)
        S{i} = getChar(Element(i));
    end
else
    S = char(Element.getTextContent);
end

function ok = checkNameNS(NameSpaces,Item,namespace,name)
fullNodeName = char(Item.getNodeName);
colon = strfind(fullNodeName,':');
if isempty(colon)
    ok = isequal(fullNodeName,name);
    Idx = strcmp('',NameSpaces(:,1));
else
    nodeName = fullNodeName(colon+1:end);
    ok = isequal(nodeName,name);
    Idx = strcmp(fullNodeName(1:colon-1),NameSpaces(:,1));
    if ~any(Idx)
        error('Unknown namespace encountered while processing attribute "%s".',fullNodeName)
    end
end
if ok && any(Idx)
    ok = isequal(NameSpaces{Idx,2},namespace);
end

function Items = getNamedChildrenNS(Parent,NameSpaces,namespace,element)
AllItems = getChildren(Parent);
if isempty(AllItems)
    Items = [];
else
    for i = length(AllItems):-1:1
        matching(i) = checkNameNS(NameSpaces,AllItems(i),namespace,element);
    end
    Items = AllItems(matching);
end

function Item = getRecursiveNamedChild(Parent,varargin)
Item = Parent;
for i = 1:length(varargin)-1
    Item = getNamedChild(Item,varargin{i});
end
Item = getNamedChildren(Item,varargin{end});

function Item = getRecursiveNamedChildNS(Parent,NameSpaces,varargin)
Item = Parent;
for i = 1:2:length(varargin)-2
    Item = getNamedChildNS(Item,NameSpaces,varargin{i},varargin{i+1});
end
Item = getNamedChildrenNS(Item,NameSpaces,varargin{end-1},varargin{end});

function Item = getNamedChildNS(Parent,NameSpaces,namespace,element)
Item = getNamedChildrenNS(Parent,NameSpaces,namespace,element);
if isempty(Item)
    error('Element <%s> does not include child element <%s>',char(Parent.getNodeName),element)
elseif length(Item)>1
    error('Element <%s> includes multiple child elements <%s>',char(Parent.getNodeName),element)
end

function [S,err] = getAttribute(Element,name)
Attribs = Element.getAttributes;
nAtt = Attribs.getLength;
err = false;
for i = 0:nAtt-1
    fullAttName = char(Attribs.item(i).getName);
    ok = isequal(fullAttName,name);
    if ok
        S = char(Attribs.item(i).getValue);
        return
    end
end
if nargout>1
    S = '';
    err = true;
else
    error('Attribute "%s" not found.',name)
end

function [S,err] = getAttributeNS(Element,NameSpaces,namespace,name)
Attribs = Element.getAttributes;
nAtt = Attribs.getLength;
err = false;
for i = 0:nAtt-1
    fullAttName = char(Attribs.item(i).getName);
    colon = strfind(fullAttName,':');
    if isempty(colon)
        ok = isequal(fullAttName,name);
        Idx = strcmp('',NameSpaces(:,1));
    else
        attName = fullAttName(colon+1:end);
        ok = isequal(attName,name);
        Idx = strcmp(fullAttName(1:colon-1),NameSpaces(:,1));
        if ~any(Idx)
            if nargout>1
                err = true;
                return
            else
                error('Unknown namespace encountered while processing attribute "%s".',fullAttName)
            end
        end
    end
    if ok && any(Idx) && isequal(NameSpaces{Idx,2},namespace)
        S = char(Attribs.item(i).getValue);
        return
    end
end
if nargout>1
    S = '';
    err = true;
else
    error('Attribute "%s" from namespace "%s" not found.',name,namespace)
end
