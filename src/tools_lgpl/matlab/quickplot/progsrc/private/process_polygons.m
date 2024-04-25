function [xy,cLabels,cv] = process_polygons(xy,fc,cv,Thresholds)
%PROCESS_POLYGONS Convert MATLAB patch data to polygons (with holes).
%   [XYP,CLABEL,CVP] = PROCESS_POLYGONS(XY,FC,CV,MINMAX) expects as input
%   a cell array of vertices of the patches plotted by (tri)contourf (XY),
%   a cell array of faces (definitions) of the patches plotted (FC), a cell
%   array of the facevertexcdata (color) data of patches plotted (CV) and
%   an Nx2 array containing the Min and Max Threshold values for each
%   patch. It returns a cell array of length K containing the coordinates
%   of each polygon (XYP), a cell array of length 2 containing the names of
%   the values (cLabels), and an array of size Kx2 with the minimum and
%   maximum value per polygon (CVP).
%
%   The contourf function plots a number of overlapping patches creating
%   the illusion of shaded areas between contour lines. The area may even
%   have holes represented by patches copying the color of the axes/figure.
%   This function processes the geometric data and generates complex patch
%   definitions (possibly with holes) that can be exported to GIS as
%   polygons.
%
%   The tricontourf function plots a large number of triangles creating the
%   illusion of large shaded areas between contour lines. This function
%   processes the geometric data and generates merged complex partch
%   definitions (possibly with holes) that can be exported to GIS as
%   polygons.
%
%   Example:
%      [C, H] = contourf(...);
%      XY = get(H,'vertices');
%      FC = get(H,'faces');
%      CV = get(H,'facevertexcdata');
%      MINMAX = zeros(length(H),2);
%      for ip = 1:length(H)
%         MINMAX(ip,:) = [getappdata(H(ip),'MinThreshold') ...
%                         getappdata(H(ip),'MaxThreshold')];
%      end
%      [XYP,CLABEL,CVP] = process_polygons(XY,FC,CV,MINMAX);
%
%   Example:
%      H = contourf(...);
%      XY = get(H,'vertices');
%      FC = get(H,'faces');
%      CV = get(H,'facevertexcdata');
%      MINMAX = zeros(length(H),2);
%      for ip = 1:length(H)
%         MINMAX(ip,:) = [getappdata(H(ip),'MinThreshold') ...
%                         getappdata(H(ip),'MaxThreshold')];
%      end
%      [XYP,CLABEL,CVP] = process_polygons(XY,FC,CV,MINMAX);
%
%   See also contourf, tricontourf.

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

contourfstyle = 'structured';
for i=1:length(fc)
    if size(fc{i},2)>3
        % this is a real polygon, this can't be the output of a tricontourf
        % call.
        break
    elseif size(fc{i},1)>1
        % this is a set of triangles which is created by tricontourf and
        % not by contourf
        contourfstyle = 'tri';
        break
    end
end
%
% preprocess the values
%
if isequal(size(Thresholds),[size(cv,1) 2])
    cv = Thresholds;
else
    nThresh=length(Thresholds);
    for i=1:size(cv,1)
        if isempty(cv{i})
            cv{i} = [NaN NaN];
        else
            ci = cv{i}(1);
            if ci<nThresh
                cv{i} = Thresholds(ci+(0:1));
            else
                cv{i} = [Thresholds(ci) NaN];
            end
        end
    end
    cv=cat(1,cv{:});
end
cLabels={'Min','Max'};
%
% call the routine specific for the contourf style.
%
switch contourfstyle
    case 'structured'
        [xy,cv] = clip_polygons(xy,cv,false);
    case 'tri'
        [xy,cv] = tri2polygons(xy,fc,cv);
end


function [xy,cv] = tri2polygons(xy,fc,cv)
cv = mat2cell(cv,ones(1,size(cv,1)),2);
for part = 1:length(xy)
    xyp = xy{part};
    fcp = fc{part};
    edp = [fcp(:,[1 2]);fcp(:,[2 3]);fcp(:,[3 1])];
    sedp = sort(edp,2);
    [uedp,iorg,isorted] = unique(sedp,'rows');
    isedge = accumarray(isorted,1)==1;
    edgenr = iorg(isedge);
    edges = edp(edgenr,:);
    %
    contour = NaN(1,2*size(edges,1));
    contour(1:2) = edges(1,:);
    n1 = contour(1);
    n2 = contour(2);
    %
    [snodes,irow] = sort(edges(:));
    irow = mod(irow-1,size(edges,1))+1;
    istart = cumsum([1;accumarray(snodes,1)]);
    %
    % mark edge as traversed
    edges(1,:) = NaN;
    %
    i = 2;
    while 1
        %
        % search for an edge that connects to node n2.
        % these edges are numbered irow(istart(n2):istart(n2+1)-1)
        % they may already be traversed, so let's check which one is available
        %
        for r2 = istart(n2):istart(n2+1)-1
            l2 = irow(r2);
            if edges(l2,1)==n2
                n3 = edges(l2,2);
                break
            elseif edges(l2,2)==n2
                n3 = edges(l2,1);
                break
            end
        end
        %
        % add the node to the contour
        i=i+1;
        contour(i) = n3;
        %
        % mark edge as traversed
        edges(l2,:) = NaN;
        %
        if n3==n1
            % back at starting point
            % search for any edges that haven't been traversed yet
            todo = find(~isnan(edges(:,1)));
            if isempty(todo)
                % no more edges
                break
            else
                % still some edges to go: pick first
                e = todo(1);
                contour(i+2:i+3) = edges(e,:);
                n1 = contour(i+2);
                n2 = contour(i+3);
                i = i+3;
                edges(e,:) = NaN;
            end
        else
            % continue the search from the latest node
            n2 = n3;
        end
    end
    %
    % put contour back into xy
    contour = contour(1:i);
    bpoint = isnan(contour);
    nparts = sum(bpoint)+1;
    if nparts==1
        xy{part} = {xyp(contour,1:2)};
    else
        bpoint = [0 find(bpoint) i+1];
        xypc = cell(nparts,1);
        for i = 1:nparts
            xypc{i} = xyp(contour(bpoint(i)+1:bpoint(i+1)-1),1:2);
        end
        cvp = repmat(cv{part,:},nparts,1);
        %[xypc,cvp] = clip_polygons(xypc,cvp,true);
        xy{part} = xypc;
        cv{part} = cvp;
    end
end
xy = cat(1,xy{:});
cv = cat(1,cv{:});


function [xy,cv] = clip_polygons(xy,cv,quick)
%
% remove individual points
%
for i=length(xy):-1:1
    if size(xy{i},1)==1
        xy(i) = [];
        cv(i,:) = [];
    else
        xy{i} = xy{i}(:,1:2);
    end
end
%
% for every polygon check if it is completely inside another polygon
%
inside = false(length(xy));
s = warning('query','MATLAB:inpolygon:ModelingWorld');
warning('off','MATLAB:inpolygon:ModelingWorld')
if quick
    for i=1:length(xy)
        for j=1:length(xy)
            if i~=j
                inside(i,j) = inpolygon(xy{i}(1,1),xy{i}(1,2),xy{j}(:,1),xy{j}(:,2));
            end
        end
    end
else
    for i=1:length(xy)
        for j=1:length(xy)
            if i~=j
                inside(i,j) = all(inpolygon(xy{i}(:,1),xy{i}(:,2),xy{j}(:,1),xy{j}(:,2)));
            end
        end
    end
end
warning(s);
%
changed = 1;
while changed
    changed = 0;
    outerpolys = find(~any(inside,2));
    for i = 1:length(outerpolys)
        outerpoly = outerpolys(i);
        xy1 = xy{outerpoly};
        %
        % find polygons that fit inside this polygon, but not in any other.
        %
        inpoly = find(inside(:,outerpoly));
        biggestinnerpolys = inpoly(sum(inside(inpoly,:),2)==1);
        if isempty(biggestinnerpolys)
            continue
        end
        %
        % figure out which of the biggestinnerpolys are holes and which are
        % polygons that touch the boundary. The former ones are simple
        % cases in which the inner polygon is strictly inside the outer
        % polygon. The polygon should then just be appended as second
        % part/hole.
        %
        npolys = length(biggestinnerpolys);
        hole = false(1,npolys);
        for j = 1:npolys
            innerpoly = biggestinnerpolys(j);
            xy2 = flipud(xy{innerpoly});
            shared = ismember(xy2,xy1,'rows');
            hole(j) = ~any(shared);
        end
        holes = biggestinnerpolys(hole);
        nonholes = biggestinnerpolys(~hole);
        %
        % The non-holes are the tricky cases since they share part of their
        % boundary with the original outerpolygon. In this case we need to
        % adjust the outerpolygon. However, these inner polygons may cut
        % the outerpolygon in multiple pieces. Put xy1 into a cell array to
        % be able to handle multiple parts
        %
        xy1 = {xy1};
        for j = 1:length(nonholes)
            innerpoly = nonholes(j);
            xy2 = flipud(xy{innerpoly});
            %
            for part = 1:length(xy1)
                xy1p = xy1{part};
                [aligned,index] = ismember(xy1p,xy2,'rows');
                if ~any(aligned)
                    % can't be a hole, so xy2 is outside this part of
                    % polygon xy1.
                    continue
                end
                %
                % Partial alignment: There may be alignment along multiple
                % sections of the polygon. Make sure that the first point
                % is an aligned point, such that it will be cut away
                % by one of the points. All parts to be kept will then be
                % simple index ranges.
                %
                first = 1;
                npnt1 = size(xy1p,1);
                npnt2 = size(xy2 ,1);
                %
                if ~aligned(1)
                    %
                    % find first point aligned (could be implemented in
                    % recent MATLAB versions using find first).
                    %
                    i1 = 1;
                    while ~aligned(i1)
                        i1 = i1+1;
                    end
                    %
                    % renumber such that first point aligned becomes first
                    % point.
                    %
                    renumber = [i1:npnt1 2:i1];
                    xy1p     = xy1p(renumber,:);
                    aligned  = aligned(renumber);
                    index    = index(renumber);
                end
                %
                % determine last point aligned
                %
                i1 = 1;
                i2 = index(i1);
                if i2==1
                    i2 = npnt2;
                end
                while i1<npnt1
                    if isequal(xy1p(i1+1,:),xy2(i2-1,:))
                        i1 = i1+1;
                        i2 = i2-1;
                        if i2==1 % wrap around if necessary
                            i2 = npnt2;
                        end
                    else
                        % we should keep index i1F until i1L
                        i1F = i1+1;
                        i1L = i1+1;
                        while ~aligned(i1L+1)
                            i1L = i1L+1;
                        end
                        %
                        % as well as i2F to i2L
                        i2L = i2-1;
                        i2F = i2-1;
                        if i2F==1
                            i2F = npnt2;
                        end
                        while ~isequal(xy1p(i1L+1,:),xy2(i2F-1,:))
                            i2F = i2F-1;
                            if i2F==1 % wrap around if necessary
                                i2F = npnt2;
                            end
                        end
                        %
                        % so the new polygon is
                        if i2F<i2L
                            xynp = [xy1p(i1F:i1L,:);xy2(i2F:i2L,:);xy1p(i1F,:)];
                        else
                            xynp = [xy1p(i1F:i1L,:);xy2(i2F:end,:);xy2(2:i2L,:);xy1p(i1F,:)];
                        end
                        if first
                            xy1{part} = xynp;
                            first = 0;
                        else
                            xy1{end+1} = xynp;
                        end
                        %
                        % continue to search for another patch
                        i1 = i1L+1;
                        i2 = index(i1);
                    end
                end
            end
        end
        %
        % now process the holes
        %
        for j = 1:length(holes)
            innerpoly = holes(j);
            xy2 = flipud(xy{innerpoly});
            %
            % determine in which part this hole is located
            %
            for part = 1:length(xy1)
                % inpolygon accepts holes when hole outline is defined with
                % opposite direction and parts must be separated by NaNs
                if quick
                    c = inpolygon(xy2(1,1),xy2(1,2),xy1{part}(:,1),xy1{part}(:,2));
                else
                    c = all(inpolygon(xy2(:,1),xy2(:,2),xy1{part}(:,1),xy1{part}(:,2)));
                end
                if c
                    % inside this part
                    xy1{part} = [xy1{part};NaN NaN;xy2];
                    % process next hole
                    continue
                end
            end
        end
        %
        % insert the first remnant of the outer polygon at the original
        % position and the other remnants at the end. We can't mess with
        % the indices here since we may have a list of outer polygons
        % stored in outerpolys.
        %
        newpolys = length(xy)+(1:length(xy1)-1);
        xy{outerpoly} = xy1{1};
        xy(newpolys)  = xy1(2:end)';
        %
        % update inside administration - there are no polygons inside this
        % outer polygon anymore. Polygons that were inside were either one
        % of the biggest innner polygons that have just been cut away, or
        % they are inside one of these former biggest inner polygons. The
        % biggest inner polygons have thereby now become outer polygons
        % themselves and this is how the algorithm converges.
        %
        inside(:,outerpoly) = false;
        %
        % mark processed polygon as inside themselves
        %
        inside(outerpoly,outerpoly) = true;
        if ~isempty(newpolys)
            %
            % extend the inside administration with zeros for all new
            % polygons added. Just mark that these polygons are inside
            % themselves such that we don't have to check them again as
            % possible outer polygons -- they have been fully processed.
            %
            for part = length(newpolys):-1:1
                inside(newpolys(part),newpolys(part)) = true;
            end
            %
            % copy the polygon min/max values for the new polygons from the
            % old one.
            %
            cv(newpolys,:) = repmat(cv(outerpoly,:),length(newpolys),1);
        end
        changed = 1;
    end
end
