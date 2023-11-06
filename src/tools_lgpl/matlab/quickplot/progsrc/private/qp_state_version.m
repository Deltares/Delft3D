function State=qp_state_version(OldState)
%QP_STATE_VERSION Check state.

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

State=OldState;
if ~isfield(OldState,'version')
    State=mvopt(State,'PresentType','presentationtype'); %,'dummy'
    %State=setopt(State,'MNK',0);
    %
    State=mvopt(State,'CLim','colourlimits'); %,[]
    State=mvopt(State,'CLimSymm','symmetriccolourlimits'); %,0
    %
    State=mvopt(State,'Thresholds','thresholds'); %,'none'
    State=mvopt(State,'ThreshDistr','thresholddistribution'); %,'linear'
    %
    State=mvopt(State,'DamColor','colourdams'); %,0
    %
    State=mvopt(State,'VecSMode','verticalscalingmode'); %,''
    State=mvopt(State,'VecSFactor','verticalscalefactor'); %,1
    %
    State=mvopt(State,'VecPlotType','vectorcomponent'); %,''
    State=mvopt(State,'VecColor','vectorcolour'); %,''
    %
    State=mvopt(State,'LineStyle','linestyle'); %,'-'
    State=mvopt(State,'LineWidth','linewidth'); %,0.5
    State=mvopt(State,'Marker','marker'); %,'+'
    State=mvopt(State,'MarkerColor','markercolour');
    State=mvopt(State,'MarkerFillColor','markerfillcolour');
    State=mvopt(State,'Color','colour');
    State=mvopt(State,'FaceColor','facecolour'); %,'none'
    %
    State=mvopt(State,'NumFormat','numformat'); %,'%.2f'
    State=mvopt(State,'FontSize','fontsize'); %,6
    State=mvopt(State,'HorizontalAlign','horizontalalignment'); %,'centre'
    State=mvopt(State,'VerticalAlign','verticalalignment'); %,'middle'
    if isfield(State,'ExtraTextParams')
        if isempty(State.ExtraTextParams)
            %State.textboxfacecolour='none';
        else
            State.textboxfacecolour=State.ExtraTextParams{4};
        end
        State=rmfield(State,'ExtraTextParams');
        %else
        %   State.textboxfacecolour='none';
    end
    %
    State=mvopt(State,'VecSMode','vectorscalingmode'); %,''
    if ~isfield(State,'VecScale') || isempty(State.VecScale)
        State.VecScale=1;
    end
    State=mvopt(State,'VecScale','vectorscale'); %,1
    %
    State=mvopt(State,'ThinMth','thinningmode'); %,'none'
    State=mvopt(State,'ThinFac','thinningfactors'); %,[1 1 1]
    State=mvopt(State,'ThinDist','thinningdistance'); %,50
    %
    State=mvopt(State,'Colormap','colourmap'); %,[]
    State=mvopt(State,'Colorbar','colourbar'); %,0
    if isfield(State,'colourbar') && ischar(State.colourbar)
        cbdir={'none' 'vert' 'horiz'};
        State.colourbar=cbdir{State.colourbar+1};
    end
    %
    State=mvopt(State,'Clippingvals','clippingvalues'); %,[]
    %
    State.version=1;
end
if State.version<1.1
    State=setopt(State,'units','');
    State.version=1.1;
end
if State.version<1.2
    State=setopt(State,'plotcoordinate','path distance');
    State.version=1.2;
end
if State.version<1.3
    State=setopt(State,'vectorstyle','rooted arrow');
    State.version=1.3;
end
%
% default settings
%
State=setopt(State,'presentationtype','');
State=setopt(State,'units','');
%State=setopt(State,'thinningmode','none');
%State=setopt(State,'thresholds','none');
%State=setopt(State,'thresholddistribution','linear');
%State=setopt(State,'horizontalalignment','centre');
%State=setopt(State,'verticalalignment','middle');
%State=setopt(State,'fontsize',6);
State=setopt(State,'textboxfacecolour','none');
State=setopt(State,'colourlimits',[]);
State=setopt(State,'symmetriccolourlimits',0);
State=setopt(State,'colourbar','none');
State=setopt(State,'colourmap',[]);
State=setopt(State,'vectorcolour','');
State=setopt(State,'colourdams',0);
State=setopt(State,'vectorcomponent','');
State=setopt(State,'facecolour','none');
State=setopt(State,'colour',[1 0 0]);
%State=setopt(State,'verticalscalingmode','unrestricted');
%State=setopt(State,'clippingvalues',[]);
State=setopt(State,'extend2edge',0);
if isfield(State,'axestype') && strcmp(State.axestype,'Distance-Val')
    State=setopt(State,'plotcoordinate','path distance');
end
if isfield(State,'marker')
    State=setopt(State,'markersize',6);
end
if strcmp(State.presentationtype,'values')
    State=setopt(State,'clipnans',1);
end


function State=setopt(State,Field,Val)
if ~isfield(State,Field)
    State.(Field) = Val;
end


function State=mvopt(State,oldField,newField,defaultVal)
if isfield(State,oldField)
    Val = State.(oldField);
    State = rmfield(State,oldField);
    State.(newField) = Val;
elseif nargin>3
    Val = defaultVal;
    State.(newField) = Val;
end
