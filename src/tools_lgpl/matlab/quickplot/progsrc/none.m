function L = none(A,dim)
%NONE   True if all elements of a vector are zero.
%   For vectors, NONE(V) returns logical 1 (TRUE) if all of the elements 
%   of the vector are zero.  Otherwise it returns logical 0 (FALSE).  For 
%   matrices, NONE(X) operates on the columns of X, returning a row vector
%   of logical 1's and 0's. For N-D arrays, NONE(X) operates on the first
%   non-singleton dimension.
%
%   NONE(X,DIM) works down the dimension DIM.  For example, NONE(X,1)
%   works down the first dimension (the rows) of X.
%
%   NONE(...) is equivalent to NOT(ANY(...)).
%
%   See also ANY, ALL.

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

if nargin==2
    L = ~any(A,dim);
else
    L = ~any(A);
end