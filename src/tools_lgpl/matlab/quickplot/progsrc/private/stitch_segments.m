function [xy,cv] = stitch_segments(xy,cv)
%STITCH_SEGMENTS Merge short line segments to longer lines.
%   [XYN,CVN] = STITCH_SEGMENTS(XY,CV) processes the line segments
%   specified as NaN-separated sets in XY to a cell array XYN

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

if iscell(xy) && iscell(cv)
    if numel(xy)~=numel(cv)
        error('Number of source lines and number of values should match.')
    end
else
    if ~iscell(xy)
        xy = {xy};
    end
    if ~iscell(cv)
        cv = {cv};
    end
end

xy_new = cell(size(xy));
cv_new = cell(size(cv));
for part = 1:length(xy)
    xyp = xy{part};
    if ~ismatrix(xyp) || size(xyp,2)~=2 || ~isnumeric(xyp)
        error('Each line in XY should consist of an Nx2 matrix.')
    elseif ~isequal(size(cv{part}),[1,1]) || ~isnumeric(cv{part})
        error('Each value in CV should be a single number.')
    end
    xy_new{part} = seg2pol(xy{part});
    cv_new{part} = repmat(cv(part),size(xy_new{part}));
end
xy = cat(1,xy_new{:});
cv = cat(1,cv_new{:});


function xy_new = seg2pol(xy)
separators = [0;find(any(isnan(xy),2));size(xy,1)+1];
segStart = separators(1:end-1)+1;
segEnd = separators(2:end)-1;

emptySegments = segStart > segEnd;
segStart(emptySegments) = [];
segEnd(emptySegments) = [];
nSegments = length(segStart);

xy_new = cell(nSegments,1);

% the maximum number of points in a segment equals: one point to start with
% and every segment adds all its points except for the first one which
% overlaps with the previous one.
maxNPoints = sum(segEnd-segStart)+1;

startPoint = xy(segStart,:);
endPoint = xy(segEnd,:);

allPoints = [startPoint;endPoint];
[uniPoints,~,iStartEnd] = unique(allPoints,'rows');

iStartPoint = iStartEnd(1:nSegments);
iEndPoint = iStartEnd(nSegments+1:end);

processed = false(nSegments,1);

iNewSeg = 1;
while true
    iSegment = zeros(maxNPoints,1);
    
    % start from the first unprocessed segment
    iSeg = find(~processed,1,'first');
    if isempty(iSeg)
        break
    end
    processed(iSeg) = true;
    nPnt = segEnd(iSeg) - segStart(iSeg) + 1;
    iSegment(1:nPnt) = segStart(iSeg):segEnd(iSeg);
    iStart = iStartPoint(iSeg);
    iEnd = iEndPoint(iSeg);
    
    % search for segments that can be added at the starting point
    matchFound = true;
    while matchFound
        jSeg = find(iStartEnd == iStart);
        jSeg_ = jSeg;
        jSeg_(jSeg > nSegments) = jSeg_(jSeg > nSegments) - nSegments;
        jSeg(processed(jSeg_)) = [];
        
        if ~isempty(jSeg)
            jSeg = jSeg(1);
            
            if jSeg <= nSegments
                % matching start point
                newPnts = segEnd(jSeg):-1:segStart(jSeg)+1;
                % new starting point, is the end point of the added segment
                iStart = iEndPoint(jSeg);
            else
                % matching end point
                jSeg = jSeg - nSegments;
                newPnts = segStart(jSeg):segEnd(jSeg)-1;
                % new starting point, is the starting point of the added
                % segment
                iStart = iStartPoint(jSeg);
            end
            nNewPnts = length(newPnts);
            
            iSegment(nNewPnts+1:nNewPnts+nPnt) = iSegment(1:nPnt);
            iSegment(1:nNewPnts) = newPnts;
            nPnt = nPnt + nNewPnts;
            
            processed(jSeg) = true;
        else
            matchFound = false;
        end
        
        % break if the line closes
        if iStart == iEnd
            matchFound = false;
        end
    end
    
    % end point
    if iStart ~= iEnd
        % search for segments that can be added at the end point
        matchFound = true;
        while matchFound
            jSeg = find(iStartEnd == iEnd);
            jSeg_ = jSeg;
            jSeg_(jSeg > nSegments) = jSeg_(jSeg > nSegments) - nSegments;
            jSeg(processed(jSeg_)) = [];
            
            if ~isempty(jSeg)
                jSeg = jSeg(1);
                
                if jSeg <= nSegments
                    % matching start point
                    newPnts = segStart(jSeg)+1:segEnd(jSeg);
                    % new end point, is the end point of the added segment
                    iEnd = iEndPoint(jSeg);
                else
                    % matching end point
                    jSeg = jSeg - nSegments;
                    newPnts = segEnd(jSeg)-1:-1:segStart(jSeg);
                    % new end point, is the starting point of the added
                    % segment
                    iEnd = iStartPoint(jSeg);
                end
                nNewPnts = length(newPnts);
                
                iSegment(nPnt+1:nPnt+nNewPnts) = newPnts;
                nPnt = nPnt + nNewPnts;
                
                processed(jSeg) = true;
            else
                matchFound = false;
            end
            
            % break if the line closes
            if iStart == iEnd
                matchFound = false;
            end
        end
    end
    
    % store the formed line
    xy_new{iNewSeg} = xy(iSegment(1:nPnt),:);
    
    % continue with the next line
    iNewSeg = iNewSeg + 1;
end
% all segments parsed
xy_new = xy_new(1:iNewSeg-1);

