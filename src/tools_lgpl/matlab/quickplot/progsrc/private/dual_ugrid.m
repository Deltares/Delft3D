function data = dual_ugrid(data,includeBoundaryNodes)
%DUAL_UGRID Create dual unstructured grid

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

X = data.X;
Y = data.Y;
FaceNodeConnect = data.FaceNodeConnect;
if isfield(data,'EdgeNodeConnect')
    EdgeNodeConnect = data.EdgeNodeConnect;
else
    EdgeNodeConnect = [];
end
%
% Get the list of all Edges included as a side of Faces.
% Edges may be duplicated.
%
FNCe = FaceNodeConnect(:,[ceil(1:0.5:size(FaceNodeConnect,2)) 1])';
FNCe(isnan(FNCe)) = [];
FNCe = reshape(FNCe,[2 numel(FNCe)/2])';
%
% If there is no EdgeNodeConnect, use the list of unique edges derived from
% the FaceNodeConnect.
%
if isempty(EdgeNodeConnect)
    EdgeNodeConnect = unique(sort(FNCe,2),'rows');
end
%
% Determine number of Nodes, Edges, and Faces.
%
nNodes = length(X);
nEdges = size(EdgeNodeConnect,1);
nFaces = size(FaceNodeConnect,1);
%
% Split FaceNodeConnect into a mask array "missing" of fill entries in the
% FaceNodeConnect array and an updated FaceNodeConnect array that can be
% used for indexing (NaNs replaced by 1). Determine the number of nodes per
% face.
%
missing = isnan(FaceNodeConnect);
FaceNodeConnect(missing) = 1;
nNodesPerFace = sum(~missing,2);
%
% Get or derive the Face coordinates.
%
if isfield(data,'XFace')
    xFace = data.XFace;
    yFace = data.YFace;
    data = rmfield(data,{'XFace','YFace'});
else
    xFace = X(FaceNodeConnect);
    xFace(missing)=0;
    xFace = sum(xFace,2)./nNodesPerFace;
    yFace = Y(FaceNodeConnect);
    yFace(missing)=0;
    yFace=sum(yFace,2)./nNodesPerFace;
end
%
% Determine the number of Faces connected to a Node
%
nFacesPerNode = accumarray(FaceNodeConnect(:),1,[nNodes 1]);
nFacesPerNode(1) = nFacesPerNode(1)-sum(missing(:));
%
% Determine the list of Faces connected to a Node.
% This list of Faces is not ordered!
%
NFC = [FaceNodeConnect(:) repmat((1:nFaces)',size(FaceNodeConnect,2),1)];
NFC(missing,:) = [];
NFC = sortrows(NFC);
j = ones(max(nFacesPerNode)+1,nNodes);
j(nFacesPerNode+1+(0:nNodes-1)'*size(j,1)) = NaN;
j = cumsum(j);
j = j(~isnan(j));
NodeFaceConnect = NaN(nNodes,max(nFacesPerNode));
NodeFaceConnect(NFC(:,1)+(j-1)*nNodes) = NFC(:,2); % not yet ordered clockwise/counterclockwise!
%
% Determine the number of Edges connected to a Node.
%
nEdgesPerNode = accumarray(EdgeNodeConnect(:),1,[nNodes 1]);
%
% Determine the list of Edges connected to a Node.
% This list of Edges is not ordered!
%
NEC = sortrows([EdgeNodeConnect(:) repmat((1:nEdges)',2,1)]);
j = ones(max(nEdgesPerNode)+1,nNodes);
j(nEdgesPerNode+1+(0:nNodes-1)'*size(j,1)) = NaN;
j = cumsum(j);
j = j(~isnan(j));
NodeEdgeConnect = NaN(nNodes,max(nEdgesPerNode));
NodeEdgeConnect(NEC(:,1)+(j-1)*nNodes) = NEC(:,2);
%
% Identify the boundary Nodes.
%
iBoundaryNodes = find(nEdgesPerNode>nFacesPerNode);
%
% Determine the two Faces (or for boundaries single Face) connected to an Edge.
%
FCe = repmat(1:nFaces,size(FaceNodeConnect,2),1);
FCe = FCe(~missing');
[lia,edgeNr] = ismember(FNCe,EdgeNodeConnect,'rows');
[lia2,edgeNr2] = ismember(FNCe(:,[2 1]),EdgeNodeConnect,'rows');
EdgeFaceConnect = NaN(nEdges,2);
EdgeFaceConnect(edgeNr(lia),1)   = FCe(lia);
EdgeFaceConnect(edgeNr2(lia2),2) = FCe(lia2);
%
% Identify the boundary Edges.
%
iBoundaryEdges = find(any(isnan(EdgeFaceConnect),2));
%
complexNodes = false(nNodes,1);
%
% Loop over all the nodes to make sure that NodeFaceConnect and
% NodeEdgeConnect entries are sorted clockwise.
%
for i=1:nNodes
    nEdges_i = nEdgesPerNode(i);
    nFaces_i = nFacesPerNode(i);
    %
    % If no faces or edges connected, skip this node.
    %
    if nFaces_i==0 && nEdges_i==0
        continue
    end
    %
    % Get local copies of the connectivity arrays.
    %
    Faces_i = NodeFaceConnect(i,1:nFaces_i);
    Edges_i = NodeEdgeConnect(i,1:nEdges_i);
    Edges_i_Faces = EdgeFaceConnect(Edges_i,:);
    %
    % Make sure that all edges are oriented outward, such that
    % Edges_i_Faces(1) is always the Face to the left and Edges_i_Faces(2)
    % the Face to the right, i.e. when leaving Edges_i_Faces(1) in
    % clockwise direction Edges_i_Faces(2) is the next Face.
    %
    reversed = EdgeNodeConnect(Edges_i,2)==i;
    Edges_i_Faces(reversed,:) = Edges_i_Faces(reversed,[2 1]);
    %
    % Create new arrays for the clockwise list of Faces and Edges.
    %
    newFaces_i = zeros(size(Faces_i));
    newEdges_i = zeros(size(Edges_i));
    %
    % Determine starting point.
    %
    if nEdges_i>nFaces_i
        %
        % If not interested in boundary nodes, skip them.
        %
        if ~includeBoundaryNodes
            continue
        end
        %
        % For a boundary node, start with an Edge that has a Face on the
        % right side: clockwise the first Face when coming from outside the
        % domain.
        %
        jE = find(isnan(Edges_i_Faces(:,1)));
        if length(jE)>1
            complexNodes(i) = true;
            jE = jE(1);
        end
    else
        jE = 1;
    end
    newEdges_i(1) = jE;
    %
    % Loop over the number of connected Faces.
    %
    k = 1;
    while k <= nFaces_i
        %
        % Identify the next Face.
        %
        jF = Edges_i_Faces(jE,2);
        newFaces_i(k) = jF;
        if complexNodes(i) && isnan(jF)
            nFaces_i = nFaces_i+1;
            %
            % Identify the next Edge.
            %
            jE = setdiff(find(isnan(Edges_i_Faces(:,1))),newEdges_i);
            if length(jE)>1
                jE = jE(1);
            end
        else
            %
            % Identify the next Edge.
            %
            jE = find(Edges_i_Faces(:,1)==jF);
        end
        if k<nEdges_i
            newEdges_i(k+1) = jE;
        end
        k = k+1;
    end
    %
    % Copy new Face and Edge lists into the connectivity arrays.
    %
    NodeFaceConnect(i,1:nFaces_i) = newFaces_i;
    NodeEdgeConnect(i,1:nEdges_i) = Edges_i(newEdges_i);
end
%
if includeBoundaryNodes
    %
    % Get boundary Node coordinates.
    %
    nBoundNodes = length(iBoundaryNodes);
    xBoundNode = X(iBoundaryNodes);
    yBoundNode = Y(iBoundaryNodes);
    %
    % Derive boundary Edge coordinates.
    %
    nBoundEdges = length(iBoundaryEdges);
    xBoundEdge = sum(X(EdgeNodeConnect(iBoundaryEdges,:)),2)/2;
    yBoundEdge = sum(Y(EdgeNodeConnect(iBoundaryEdges,:)),2)/2;
    %
    % Concatenate the Face, boundary Edge, and boundary Node coordinates.
    %
    data.X = [xFace;xBoundEdge;xBoundNode];
    data.Y = [yFace;yBoundEdge;yBoundNode];
else
    data.X = xFace;
    data.Y = yFace;
end
%
% Create the Node Face connectivity array for the dual mesh.
% This array is equal to the NodeFaceConnect array extended with boundary
% Edge and Node indices for boundary Nodes.
%
FaceNodeDual = NodeFaceConnect;
if includeBoundaryNodes
    %
    % Determine the indices of the boundary Edges associated with first and
    % last Edge.
    %
    iBE(iBoundaryEdges,1) = 1:length(iBoundaryEdges);
    iBoundEdge1 = iBE(NodeEdgeConnect(iBoundaryNodes,1));
    iBoundEdge2 = iBE(NodeEdgeConnect(iBoundaryNodes + nNodes*(nEdgesPerNode(iBoundaryNodes,:)-1)));
    %
    % NOTE: The boundary faces will not be convex if the model domain at
    % the boundary node is not convex! However this is not a major problem
    % since in qp_scalarfield we will subdivide the face into triangles for
    % contours and continuous shades plotting and those triangles will be
    % generated based by combining node 1 with nodes j and j+1. Since we
    % use the central boundary node as node 1 all triangles generated will
    % be located inside the domain.
    %
    nFacesPerBoundNode_max = max(nFacesPerNode(iBoundaryNodes));
    FaceNodeDual(:,end+1:nFacesPerBoundNode_max+3) = NaN;
    for i = 1:nBoundNodes
        iB = iBoundaryNodes(i);
        nFpN = nFacesPerNode(iB);
        if complexNodes(iB)
            lastNode = max(find(~isnan(FaceNodeDual(iB,:))));
            nParts = sum(isnan(FaceNodeDual(iB,1:lastNode)))+1;
            nNodesDual = nFpN + 3*nParts;
            Face = zeros(1,nNodesDual);
            iBN = nFaces+nBoundEdges+i;
            Face(1) = iBN;
            Face(2) = nFaces+iBoundEdge1(i);
            k = 3;
            for j = 1:lastNode
                if isnan(FaceNodeDual(iB,j))
                    Face(k:k+2) = [nFaces+iBE(NodeEdgeConnect(iB,j)) iBN nFaces+iBE(NodeEdgeConnect(iB,j+1))];
                    k = k+3;
                else
                    Face(k) = FaceNodeDual(iB,j);
                    k = k+1;
                end
            end
            Face(k) = nFaces+iBoundEdge2(i);
            if nNodesDual>size(FaceNodeDual,2)
                FaceNodeDual(:,end+1:nNodesDual) = NaN;
            end
            FaceNodeDual(iB,1:nNodesDual) = Face;
        else
            %                            boundary Node        boundary Edge 1       internal Faces             boundary Edge 2
            FaceNodeDual(iB,1:nFpN+3) = [nFaces+nBoundEdges+i nFaces+iBoundEdge1(i) NodeFaceConnect(iB,1:nFpN) nFaces+iBoundEdge2(i)];
        end
    end
else
    FaceNodeDual(iBoundaryNodes,:)=[];
end
data.FaceNodeConnect = FaceNodeDual;
%
if includeBoundaryNodes
    bEdgeVal = data.Val(max(EdgeFaceConnect(iBoundaryEdges,:),[],2));
    bNodeVal = (bEdgeVal(iBoundEdge1) + bEdgeVal(iBoundEdge2))/2;
    if size(data.Val,1)>1
        data.Val = cat(1,data.Val,bEdgeVal,bNodeVal);
    else
        data.Val = cat(2,data.Val,bEdgeVal,bNodeVal);
    end
end
data.ValLocation = 'NODE';