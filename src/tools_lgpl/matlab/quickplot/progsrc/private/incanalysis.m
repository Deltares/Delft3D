function Vals = incanalysis(D,valtype,q,t1,t2,iclass)
%INCANALYSIS Analyse incremental data.
%    VALS = INCANALYSIS(INCDATA,VALTYPE,QUANT,TIME1,TIME2,CLASS) analyses
%    the selected quantity QUANT of the incremental data stored in INCDATA
%    in one of the following ways indicated by the string VALTYPE:
%
%        min               minimum class number during period TIME1 to TIME2
%        max               maximum class number during period TIME1 to TIME2
%        last              class number at TIME1
%        t_of_min          first time of minimum during period TIME1 to TIME2
%        t_of_max          first time of maximum during period TIME1 to TIME2
%        t_first_dry       first time during period TIME1 to TIME2 at which
%                          the point is dry. Similarly for t_first_wet,
%                          t_last_dry and t_last_wet.
%        t_first_eq_class  first time at which class number equals CLASS
%                          during period TIME1 to TIME2. Similarly for
%                          t_first_ge_class, t_first_gt_class,
%                          t_first_le_class, t_first_lt_class,
%                          t_last_eq_class, t_last_ge_class,
%                          t_last_gt_class, t_last_le_class and
%                          t_last_lt_class.
%        t_dry             total time during period TIME1 to TIME2 at which
%                          the point is dry. Similarly for t_wet.
%        t_eq_class        total time during period TIME1 to TIME2 that
%                          class number equals CLASS. Similarly for
%                          t_ge_class, t_gt_class, t_le_class, t_lt_class.
%
%    TIME2 and ICLASS don't have to be specified for VALTYPE='last'. ICLASS
%    does not have to be specified for VALTYPE='min', 'max', 't_of_min',
%    't_of_max', 't_first_dry', 't_last_dry', 't_first_wet', 't_last_wet',
%    't_dry' and 't_wet'.
%
%    See also FLS.

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

if strcmp(valtype,'last')
    ndom = length(D.Domain);
    Vals = cell(1,ndom);
    for d = 1:ndom
        Dom = D.Domain(d);
        Vals{d} = repmat(NaN,Dom.Size);
        Time = Dom.Quant(q).Time;
        Data = Dom.Quant(q).Data;
        Idx = Time(Data(1,:))<=t1;
        Vals{d}(Data(2,Idx)) = Data(3,Idx);
    end
    return
end

switch valtype
    case 't_first_dry'
        valtype = 't_first_eq_class';
        j = 1;
    case 't_first_wet'
        valtype = 't_first_gt_class';
        j = 1;
    case 't_dry'
        valtype = 't_eq_class';
        j = 1;
    case 't_wet'
        valtype = 't_gt_class';
        j = 1;
    case 't_last_dry'
        valtype = 't_last_eq_class';
        j = 1;
    case 't_last_wet'
        valtype = 't_last_gt_class';
        j = 1;
    case {'t_first_eq_class','t_first_ge_class','t_first_gt_class', ...
            't_first_le_class','t_first_lt_class', ...
            't_last_eq_class','t_last_ge_class','t_last_gt_class', ...
            't_last_le_class','t_last_lt_class', ...}
            't_eq_class','t_ge_class','t_gt_class', ...
            't_le_class','t_lt_class'}
        j = iclass+1;
end
ndom = length(D.Domain);
Vals = cell(1,ndom);
for d = 1:ndom
    Dom = D.Domain(d);
    Time = Dom.Quant(q).Time;
    Data = Dom.Quant(q).Data;
    %
    t1d = max(t1,min(Time));
    t2d = min(t2,max(Time));
    %
    switch valtype
        case {'min','max','last'}
            dVal = repmat(NaN,Dom.Size);
            Idx = Time(Data(1,:))<=t1d;
            dVal(Data(2,Idx)) = Data(3,Idx);
        case {'t_of_min','t_of_max'}
            dVal = repmat(t1,Dom.Size);
        case {'t_first_eq_class','t_first_ge_class','t_first_gt_class', ...
                't_first_le_class','t_first_lt_class', ...
                't_last_eq_class','t_last_ge_class','t_last_gt_class', ...
                't_last_le_class','t_last_lt_class'}
            CLASS = repmat(NaN,Dom.Size);
            Idx = Time(Data(1,:))<=t1d;
            CLASS(Data(2,Idx)) = Data(3,Idx);
            %
            dVal = repmat(NaN,Dom.Size);
            switch valtype
                case {'t_first_eq_class','t_last_eq_class'}
                    ix = CLASS==j;
                case {'t_first_ge_class','t_last_ge_class'}
                    ix = CLASS>=j;
                case {'t_first_gt_class','t_last_gt_class'}
                    ix = CLASS>j;
                case {'t_first_le_class','t_last_le_class'}
                    ix = CLASS<=j;
                case {'t_first_lt_class','t_last_lt_class'}
                    ix = CLASS<j;
            end
            dVal(ix) = t1d;
        case {'t_eq_class','t_ge_class','t_gt_class','t_le_class','t_lt_class'}
            CLASS = repmat(NaN,Dom.Size);
            Idx = Time(Data(1,:))<=t1d;
            CLASS(Data(2,Idx)) = Data(3,Idx);
            %
            dVal = repmat(0,Dom.Size);
        otherwise
            error('Unknown value type: %s.',valtype)
    end
    %
    Idx = find(diff([Data(1,:) inf]));
    %Data(1,Idx); % 1:length(Idx)
    %
    Times = Time(Data(1,Idx));
    t1i = sum(Times<=t1d);
    t2i = sum(Times<=t2d);
    %
    tprev = t1d;
    %
    for ti = t1i+1:t2i
        i = Idx(ti-1)+1:Idx(ti);
        switch valtype
            case {'t_last_eq_class','t_last_ge_class','t_last_gt_class', ...
                    't_last_le_class','t_last_lt_class'}
                switch valtype
                    case 't_last_eq_class'
                        mask = CLASS(Data(2,i))==j;
                        if ti==t2i
                            mask = mask | Data(3,i)==j;
                        end
                    case 't_last_ge_class'
                        mask = CLASS(Data(2,i))>=j;
                        if ti==t2i
                            mask = mask | Data(3,i)>=j;
                        end
                    case 't_last_gt_class'
                        mask = CLASS(Data(2,i))>j;
                        if ti==t2i
                            mask = mask | Data(3,i)>j;
                        end
                    case 't_last_le_class'
                        mask = CLASS(Data(2,i))<=j;
                        if ti==t2i
                            mask = mask | Data(3,i)<=j;
                        end
                    case 't_last_lt_class'
                        mask = CLASS(Data(2,i))<j;
                        if ti==t2i
                            mask = mask | Data(3,i)<j;
                        end
                end
                dVal(Data(2,i(mask))) = Time(ti);
                CLASS(Data(2,i)) = Data(3,i);
            case {'t_first_eq_class','t_first_ge_class','t_first_gt_class', ...
                    't_first_le_class','t_first_lt_class'}
                switch valtype
                    case 't_first_eq_class'
                        mask = Data(3,i)==j;
                    case 't_first_ge_class'
                        mask = Data(3,i)>=j;
                    case 't_first_gt_class'
                        mask = Data(3,i)>j;
                    case 't_first_le_class'
                        mask = Data(3,i)<=j;
                    case 't_first_lt_class'
                        mask = Data(3,i)<j;
                end
                v = dVal(Data(2,i(mask)));
                dVal(Data(2,i(mask))) = min(v,Time(ti));
                CLASS(Data(2,i)) = Data(3,i);
            case 'last'
                dVal(Data(2,i)) = Data(3,i);
            case 't_of_min'
                newex = dVal(Data(2,i))>Data(3,i);
                dVal(Data(2,i(newex))) = Time(ti);
            case 'min'
                v = dVal(Data(2,i));
                dVal(Data(2,i)) = min(Data(3,i),v);
            case 't_of_max'
                newex = dVal(Data(2,i))<Data(3,i);
                dVal(Data(2,i(newex))) = Time(ti);
            case 'max'
                v = dVal(Data(2,i));
                dVal(Data(2,i)) = max(Data(3,i),v);
            otherwise
                dt = Time(ti)-tprev;
                switch valtype
                    case 't_eq_class'
                        ix = CLASS==j;
                    case 't_ge_class'
                        ix = CLASS>=j;
                    case 't_gt_class'
                        ix = CLASS>j;
                    case 't_le_class'
                        ix = CLASS<=j;
                    case 't_lt_class'
                        ix = CLASS<j;
                end
                dVal(ix) = dVal(ix) + dt;
                CLASS(Data(2,i)) = Data(3,i);
                tprev = Time(ti);
        end
    end
    %
    switch valtype
        case {'min','max'}
            dVal = dVal-1;
        case {'t_last_eq_class','t_last_ge_class','t_last_gt_class', ...
                't_last_le_class','t_last_lt_class'}
            switch valtype
                case {'t_first_eq_class','t_last_eq_class'}
                    ix = CLASS==j;
                case {'t_first_ge_class','t_last_ge_class'}
                    ix = CLASS>=j;
                case {'t_first_gt_class','t_last_gt_class'}
                    ix = CLASS>j;
                case {'t_first_le_class','t_last_le_class'}
                    ix = CLASS<=j;
                case {'t_first_lt_class','t_last_lt_class'}
                    ix = CLASS<j;
            end
            dVal(ix) = t2d;
        case {'t_eq_class','t_ge_class','t_gt_class','t_le_class','t_lt_class'}
            dt = t2d-tprev;
            switch valtype
                case 't_eq_class'
                    ix = CLASS==j;
                case 't_ge_class'
                    ix = CLASS>=j;
                case 't_gt_class'
                    ix = CLASS>j;
                case 't_le_class'
                    ix = CLASS<=j;
                case 't_lt_class'
                    ix = CLASS<j;
            end
            dVal(ix) = dVal(ix) + dt;
            CLASS(Data(2,i)) = Data(3,i);
    end
    Vals{d} = dVal;
end

