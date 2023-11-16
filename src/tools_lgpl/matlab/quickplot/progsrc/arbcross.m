function varargout = arbcross(varargin)
%ARBCROSS Arbitrary cross-section through grid.
%   [X,Y,V1,V2,...] = ARBCROSS(FNC,XNOD,YNOD,ENC,VAR1,VAR2,...,XB,YB)
%   Intersects an unstructured mesh defined by face-node connectivity FNC,
%   node coordinates XNOD and YNOD and edge-node connectivity ENC with an
%   arbitrary line defined by base points XB, YB. The output vectors X and
%   Y contain the co-ordinates at which the line crosses the grid lines of
%   the mesh. The vector Vi contains interpolated values at these locations
%   given the values VARi at the mesh nodes, edges or faces. The location
%   of the data is autodetected based on VARi size, but can be specified
%   explicitly by specifying {ViLOC,VARi} instead of VARi where ViLOC is
%   one of the strings 'NODE', 'EDGE' or 'FACE'.
%
%   [X,Y,V1,V2,...] = ARBCROSS(TRI,XTRI,YTRI,VAR1,VAR2,...,XB,YB)
%   Intersects a triangular mesh defined by TRI, XTRI and YTRI with an
%   arbitrary line defined by base points XB, YB. The output vectors X and
%   Y contain the co-ordinates at which the line crosses the grid lines of
%   the mesh. The vector Vi contains interpolated values at these locations
%   given the values VARi at the mesh nodes or faces. The location of the
%   data is autodetected based on VARi size, but can be specified
%   explicitly by specifying {ViLOC,VARi} instead of VARi where ViLOC is
%   one of the strings 'NODE' or 'FACE'.
%
%   [X,Y,V1,V2,...] = ARBCROSS(XGRID,YGRID,VAR1,VAR2,...,XB,YB)
%   Intersects a curvilinear mesh defined by XGRID and YGRID with an
%   arbitrary line defined by base points XB, YB. The output vectors X and
%   Y contain the co-ordinates at which the line crosses the grid lines of
%   the mesh. The vector Vi contains interpolated values at these locations
%   given the values VARi at the mesh points.
%
%   Computing the locations of the intersections of the mesh and the line
%   can take a significant amount of time. It can be more efficient to
%   compute these intersections and the associated coefficients for the
%   interpolation only once. The necessary intermediate information can
%   be stored in a structure by using the following syntax:
%   STRUCT = ARBCROSS(TRI,XTRI,YTRI,XB,YB)
%   STRUCT = ARBCROSS(XGRID,YGRID,XB,YB)
%
%   Optionally returning immediately the X and Y coordinates as well:
%   [X,Y,STRUCT] = ARBCROSS(TRI,XTRI,YTRI,XB,YB)
%   [X,Y,STRUCT] = ARBCROSS(XGRID,YGRID,XB,YB)
%
%   Subsequently, the interpolation of data to that line can be carried
%   out efficiently by providing the structure as a first argument
%   instead of the original mesh variables:
%   [V1,V2,...] = ARBCROSS(STRUCT,VGRID1,VGRID2,...)
%   [X,Y,V1,V2,...] = ARBCROSS(STRUCT,VGRID1,VGRID2,...)

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

varargout = cell(1, nargout);
output_offset = 2; % first two output arguments by default x and y
if nargin < 2
    % minimum number of arguments: STRUCT, VAR1
    error('Too few input arguments.')
elseif isstruct(varargin{1})
    %
    % The first input argument is a structure which should be a structure
    % obtained from a previous ARBCROSS call. Extract the variables for the
    % computation.
    %
    input_offset = 1;
    %
    % The number of output arguments should be one more or one less than
    % the number of input arguments.
    %
    if nargout == nargin - 1
        output_offset = 0;
    elseif nargout ~= nargin + 1
        error('Invalid number of output arguments for input consisting of a data structure and %i quantities.', nargin-1)
    end
    input_skip_end = 0;
    %
    Keep = varargin{1};
    x = Keep.x;
    y = Keep.y;
    wght = Keep.wght;
    iNode = Keep.iNode;
    iFace = Keep.iFace;
    iEdge = Keep.iEdge;
    outside = Keep.outside;
    %dxt = Keep.dxt;
    %dyt = Keep.dyt;
    VGRIDStr = Keep.VGRIDStr;
    szXGRID = Keep.szXGRID;
    nFaces = Keep.nFaces;
elseif nargin<4
    % next minimum number of arguments: XGRID, YGRID, XB, YB
    error('Too few input arguments or of incorrect type.')
elseif ~isequal(size(varargin{end-1}), size(varargin{end})) || ...
        ~isvector(varargin{end}) || ...
        ~isnumeric(varargin{end})
    error('Last two arrays don''t represent X- and Y-coordinates of a line.')
else
    %
    % No structure containing all necessary information.
    %
    get_structure = false;
    input_skip_end = 2;
    layeredxy = 0;
    if isequal(size(varargin{1}),size(varargin{2}))
        %
        % CURVILINEAR GRID specified using:
        % * X-coordinates of nodes - M-by-N
        % * Y-coordinates of nodes - M-by-N
        %
        if nargin == 4
            if nargout == 1
                output_offset = 0;
            elseif nargout ~= 3
                error('Number of output arguments should equal 1 or 3.')
            end
            get_structure = true;
        elseif nargout ~= nargin-2
            error('Number of output arguments should be %d for %d input arguments (structured mesh).', nargin-2, nargin)
        end
        %
        input_offset = 2;
        VGRIDStr = 'VGRID';
        XGRID = varargin{1}(:,:,1);
        YGRID = varargin{2}(:,:,1);
        [FaceNodeConnect,QUADTRI] = grid2tri(XGRID,YGRID);
        EdgeNodeConnect = [];
        layeredxy = size(varargin{1},3)>1;
        nFaces = 0;
    elseif nargin<5
        % next minimum number of arguments: TRI, XTRI, YTRI, XB, YB
        error('Too few input arguments or of incorrect type.')
    else
        %
        % UGRID MESH specified using:
        % * Face-node connectivity - nFaces-by-maxNNodes
        % * X-coordinates of nodes - nNodes
        % * Y-coordinates of nodes - nNodes
        % * Edge-node connectivity - nEdges-by-2 [optional]
        %
        if ~ismatrix(varargin{1}) || size(varargin{1},2)<3
            error('First argument is invalid FaceNodeConnectivity of unstructured mesh, or first two arguments form invalid pair of X/Y coordinates of structured mesh.')
        elseif ~isequal(size(varargin{2}), size(varargin{3}))
            error('Argument 2 and 3 should have equal size (X- and Y-coordinates of unstructured mesh nodes).')
        end
        %
        if nargin == 5
            % TRI, XTRI, YTRI, XB, YB
            % return STRUCT with optional X, Y
            if nargout == 1
                output_offset = 0;
            elseif nargout ~= 3
                error('Number of output arguments should equal 1 or 3.')
            end
            input_offset = 3;
            get_structure = true;
            
        elseif ismatrix(varargin{4}) && size(varargin{4},2) == 2
            % FNC, XNODE, YNODE, ENC, ..., XB, YB
            if nargin == 6
                % FNC, XNODE, YNODE, ENC, XB, YB
                % return STRUCT with optional X, Y
                if nargout == 1
                    output_offset = 0;
                elseif nargout ~= 3
                    error('Number of output arguments should equal 1 or 3.')
                end
                get_structure = true;
                
            elseif nargout ~= nargin-4
                error('Number of output arguments should be %d for %d input arguments (UGRID mesh with edges).', nargin-4, nargin)
            end
            %
            input_offset = 4;
            
        elseif nargout ~= nargin-3
            error('Number of output arguments should be %d for %d input arguments (UGRID mesh).', nargin-4, nargin)
        else
            % TRI, XTRI, YTRI, ..., XB, YB
            input_offset = 3;
        end
        %
        VGRIDStr = 'VTRI';
        FaceNodeConnect = varargin{1};
        XGRID = varargin{2}(:,:,1);
        YGRID = varargin{3}(:,:,1);
        if input_offset==4
            EdgeNodeConnect = varargin{4};
        else
            EdgeNodeConnect = [];
        end
        QUADTRI = [];
        nFaces = size(FaceNodeConnect,1);
    end

    %
    % Determine intersection points of cross-section line and curvilinear
    % grid or triangular mesh.
    %
    XB=varargin{end-1};
    YB=varargin{end};
    [x,y,iNode,wght,iFace,fracudist,dxt,dyt,outside]=int_lntri(XB,YB,FaceNodeConnect,XGRID,YGRID);
    iEdge = [];

    %
    % Add dummy points where the slice goes out of the computational domain
    % such that there will appear a break in the plots.
    %
    for i=length(outside):-1:1
        if outside(i)
            ii = [1:i i i+1:length(x)];
            x=x(ii); x(i+1)=NaN;
            y=y(ii); y(i+1)=NaN;
            iNode=iNode(ii,:);
            wght=wght(ii,:); wght(i+1,:)=NaN;
            fracudist=fracudist(ii);
            ii = [1:i i i+1:length(iFace)];
            iFace=iFace(ii);
            outside=outside(ii);
            dxt=dxt(ii);
            dyt=dyt(ii);
        end
    end
    %
    % Remove diagonals ... maybe it would be faster to not put them in in
    % the first place, but maybe I need them again in the future for
    % consistency.
    %
    if ~isempty(QUADTRI)
        iFace = QUADTRI(iFace);
        rm = find((iFace(1:end-1)==iFace(2:end)) & ~isnan(x(2:end-1,1)));
        x(rm+1,:)        =[];
        y(rm+1,:)        =[];
        wght(rm+1,:)     =[];
        iFace(rm+1,:)    =[];
        iNode(rm+1,:)    =[];
        fracudist(rm+1,:)=[];
        outside(rm,:)    =[];
        dxt(rm,:)        =[];
        dyt(rm,:)        =[];
    end
    %
    % Expand coordinates to 3D if original x/y arrays were 3D.
    %
    if layeredxy
        x = repmat(x,[1 1 size(varargin{1},3)]);
        y = repmat(y,[1 1 size(varargin{2},3)]);
    end
    %
    % Optionally identify the edges crossed.
    %
    if ~isempty(EdgeNodeConnect)
        edgeCrossing = sum(~isnan(wght) & wght~=0,2)==2;
        edgeNodes = sort(iNode(:,1:2),2);
        EdgeNodeConnect = sort(EdgeNodeConnect,2);
        [isEdge,edgeNr] = ismember(edgeNodes,EdgeNodeConnect,'rows');
        iEdge = NaN(size(wght,1),1);
        iEdge(isEdge & edgeCrossing) = edgeNr(isEdge & edgeCrossing);
    end
    %
    % Define structure for future use
    %
    szXGRID = size(XGRID);
    if get_structure
        Keep.x = x;
        Keep.y = y;
        Keep.wght = wght;
        Keep.iNode = iNode;
        Keep.fracudist = fracudist;
        Keep.iFace = iFace;
        Keep.iEdge = iEdge;
        Keep.outside = outside;
        Keep.dxt = dxt;
        Keep.dyt = dyt;
        Keep.VGRIDStr = VGRIDStr;
        Keep.szXGRID = szXGRID;
        Keep.nFaces = nFaces;
        varargout{nargout} = Keep;
    end
end

%
% Define output
%
if output_offset>0
    varargout{1}=x;
    varargout{2}=y;
end
%
% For each data field VGRIDi in the list of input arguments, i.e. those
% starting after the grid information (unless it is a 3D grid) and stopping
% before the last two arguments.
%
if strcmp(VGRIDStr,'VGRID')
    vdim = 3;
    hdims = {':',':'};
else
    vdim = 2;
    hdims = {':'};
end
for i = 1:nargin-input_offset-input_skip_end
    VLOC = '?';
    VGRID = varargin{input_offset+i};
    if iscell(VGRID)
        VLOC  = VGRID{1};
        VGRID = VGRID{2};
    end
    szVGRID = size(VGRID);
    if strcmp(VLOC,'?')
       if isequal(prod(szVGRID(1:vdim-1)),prod(szXGRID))
           VLOC = 'NODE';
       elseif isequal(szVGRID(1:2),szXGRID-1) || numel(VGRID)==nFaces
           VLOC = 'FACE';
       else
           error('Unable to auto-detect data location for argument %d.', i)
       end
    end
    v = [];
    switch VLOC
        case 'NODE' % Values defined at mesh nodes
            szVGRID = size(VGRID);
            for k = prod(szVGRID(vdim:end)):-1:1
                vgrid = VGRID(hdims{:},k);
                v(:,k) = sum(wght.*vgrid(iNode),2);
            end
            
        case 'FACE' % Values defined on mesh patches
            szVGRID = size(VGRID);
            for k = prod(szVGRID(vdim:end)):-1:1
                vgrid = VGRID(hdims{:},k);
                v(:,k) = vgrid(iFace);
                v(outside,k) = NaN;
            end
            
        case 'EDGE' % Values defined on mesh edges
            noEdge = isnan(iEdge);
            iEdge(noEdge) = 1;
            szVGRID = size(VGRID);
            for k = prod(szVGRID(vdim:end)):-1:1
                vgrid = VGRID(hdims{:},k);
                v(:,k) = vgrid(iEdge);
            end
            v(noEdge,:) = NaN;
            
        otherwise
            error('Unknown data location "%s".', VLOC)
    end
    varargout{output_offset+i} = v;
end


function [tri,quadtri] = grid2tri(X,Y)
%GRID2TRI converts a curvilinear grid into a triangular grid
%   [TRI,QUADTRI]=GRID2TRI(XGRID,YGRID)
%   Splits the quadrangles of the curvilinear grid along the main diagonal
%   and returns the triangle definition table TRI (indicating the corner
%   points of the triangles as indices into XGRID, YGRID) and an array
%   QUADTRI that contains for every triangle the index of the quadrangle to
%   which the triangle belongs (index into an array of size SIZE(XGRID)-1).

szX = size(X);
% [m,n]=ndgrid(1:szX(1),1:szX(2));
I = reshape(1:prod(szX),szX);
I = I(1:end-1,1:end-1);
I = I(:);
quad = (1:prod(szX-1))';

tri = [I I+1 I+szX(1)+1; I I+szX(1) I+szX(1)+1];
quadtri = [quad;quad];
% mtri = [m(I);m(I)];
% ntri = [n(I);n(I)];

k = any(isnan(X(tri)) | isnan(Y(tri)),2);
tri(k,:) = [];
quadtri(k) = [];
