function [Items, UserDatas, Tags] = qp_get_items(ax)
% QP_GET_ITEMS Returns the QUICKPLOT items in one or more selected axes.

%----- LGPL --------------------------------------------------------------------
%
%   Copyright (C) 2011-2024 Stichting Deltares.
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

% get all children from all axes
Items=allchild(ax);

% if multiple axes were specified, we get a cell array as return argument.
% Concatenate the lists but insert 0 (root) objects as separators.
if iscell(Items)
    Items(:,2)={0};
    Items(end,2)={[]};
    Items=Items';
    Items=cat(1,Items{:});
end
Types=cget(Items,'type');
null = strcmp(Types,'root');

% get the tags ... make sure that the root objects have a valid tag
Tags=cget(Items,'tag');
for t=find(null)'
    Tags(t)={sprintf('QPPlotTag---%i',t)};
end

% get the user data ... make sure that it's non-empty for the root objects
UserDatas=cget(Items,'userdata');
UserDatas(null)={'---'};

% keep only the handles with non-empty tags and user data
TUDvalid=~cellfun('isempty',Tags) & ~cellfun('isempty',UserDatas);
Items=Items(TUDvalid);
Tags=Tags(TUDvalid);
UserDatas=UserDatas(TUDvalid);

% keep only the handles with valid tags
QPTag=strncmp('QPPlotTag',Tags,9);
Items=Items(QPTag);
Tags=Tags(QPTag);
UserDatas=UserDatas(QPTag);

% make sure to keep only unique tags (not sure why this is needed)
[Tags,I]=unique(Tags);
[I,Isort]=sort(I);
Tags=Tags(Isort);
Items=Items(I);
UserDatas=UserDatas(I);

% remove root objects at the beginning
while ~isempty(Items) && Items(end)==0
    Items(end)=[];
    UserDatas(end)=[];
    Tags(end)=[];
end

% remove root objects at the end
while ~isempty(Items) && Items(1)==0
    Items(1)=[];
    UserDatas(1)=[];
    Tags(1)=[];
end

% perform a get call, but make sure that the return argument is always a
% cell array
function v = cget(handle,prop)
v = get(handle,prop);
if isempty(v)
    v = {};
elseif ~iscell(v)
    v = {v};
end