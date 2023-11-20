function h = qp_colormap(MapName,M)
%QP_COLORMAP QuickPlot colormap repository.
%
%   CMap = QP_COLORMAP(MName,M)
%   returns an M-by-3 matrix containing colormap MName. The input argument
%   M is optional. If it is not provided the length of the colormap is
%   taken equal to the length of the colormap of the current figure.
%
%   CMapStruct = QP_COLORMAP(':getstruct',MName)
%   returns a structure of the indicated colormap which is compatible with
%   the CLRMAP function.
%
%   Maps = QP_COLORMAP
%   returns the names of all available maps on file, this list loaded upon
%   the first call of QP_COLORMAP.
%
%   Maps = QP_COLORMAP(':reload')
%   Forces reloading all colormaps from file and returns the new list.

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

persistent COLORMAPS

NArg = nargin;
if NArg == 0
    %
    % Make sure that MapName is defined
    %
    MapName = ':getmaps';
elseif strcmp(MapName,':reload')
    %
    % A reload request should be treated as a case without input arguments
    % (i.e. a request for a list of all colormaps).
    %
    NArg = 0;
end
%
% If COLORMAPS have not been loaded before, or if the user requests a
% reload of the colormaps, load the colormaps.
%
if isempty(COLORMAPS) || strcmp(MapName,':reload')
    %
    % Retreive directory information from QP_BASEDIR.
    %
    qp_path = qp_basedir('base');
    clrmap_path = [qp_path filesep 'colormaps'];
    %
    % Reset list of loaded colormaps.
    %
    COLORMAPS = [];
    %
    % If the colormap directory exists.
    %
    if exist(clrmap_path,'dir')
        %
        % Get list of colormap files.
        %
        clrmap_path=[clrmap_path filesep];
        d = dir([clrmap_path '*.clrmap']);
        nfiles = length(d);
        %
        % Process colormap files.
        %
        [COLORMAPS(1:nfiles).Name] = deal('');
        for i = nfiles:-1:1
            try
                S = clrmap('read',[clrmap_path d(i).name]);
                %
                COLORMAPS(i).Space = S.Space;
                if isfield(S,'Index')
                    COLORMAPS(i).Index = S.Index;
                else
                    COLORMAPS(i).Index = [];
                end
                COLORMAPS(i).Colors = S.Colors;
                if isfield(S,'AlternatingColors')
                    COLORMAPS(i).AlternatingColors = S.AlternatingColors;
                else
                    COLORMAPS(i).AlternatingColors = 0;
                end
                % finish by setting the Name, such that failed transfers
                % end up with blank name
                if isfield(S,'Name')
                    COLORMAPS(i).Name = S.Name;
                else
                    COLORMAPS(i).Name = d(i).Name;
                end
            catch
                % skip files that fail reading
            end
        end
        %
        empty_names = cellfun(@isempty, {COLORMAPS.Name});
        COLORMAPS(empty_names) = [];
    end
    %
    % fallback in case all are empty
    %
    if isempty(COLORMAPS)
        %
        % Default list contains only jet.
        %
        COLORMAPS.Name = 'jet';
        COLORMAPS.Space = 'RGB';
        COLORMAPS.Index = [0 1/8 3/8 5/8 7/8 1];
        COLORMAPS.Colors = [0 0 0.5; 0 0 1; 0 1 1; 1 1 0; 1 0 0; 0.5 0 0];
        COLORMAPS.AlternatingColors = 0;
    else
        %
        % Sort colormaps in alphabetical order (neglecting upper/lower case).
        %
        cmaps = lower({COLORMAPS.Name});
        [~,i] = sort(cmaps);
        COLORMAPS = COLORMAPS(i);
    end
end

cmaps = {COLORMAPS.Name};
if NArg == 0
    %
    % if no input argument provided, return the list of colormaps.
    %
    h = cmaps;
    
elseif isequal(MapName,':getstruct')
    %
    % if :getstruct command was passed, then check for the colormap stored
    % in the second argument M, and return the clrmap structure
    %
    j = ustrcmpi(M,cmaps);
    h = COLORMAPS(max(j,1));
    
else
    %
    % Find requested colormap.
    %
    j = ustrcmpi(MapName,cmaps);
    %
    % if colormap name could not be identified, return first colormap.
    %
    j = max(j,1);
    %
    % Retrieve colormap of requested length.
    %
    if COLORMAPS(j).AlternatingColors
        %
        % alternating colormaps always use their own length. Why?
        %
        M = size(COLORMAPS(j).Colors,1);
        
    elseif nargin < 2
        %
        % if M is not explicitly specified, then return a colormap of a
        % "default" length.
        %
        cf = get(0,'CurrentFigure');
        if ~isempty(cf)
            M = size(get(cf,'colormap'),1);
        else
            M = size(get(0,'defaultfigurecolormap'),1);
        end
    end
    %
    % if M == 0, then colormap length will always be empty. Why?
    %
    if M == 0
        h = zeros(0,3);
    else
        h = clrmap(COLORMAPS(j),M);
    end
end