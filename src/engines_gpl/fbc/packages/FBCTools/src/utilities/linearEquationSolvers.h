// Copyright (C) 2013 Deltares, University of Duisburg-Essen
// Institute of Hydraulic Engineering and Water Resources Management
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License version 2 as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

/**
 * @file
 * @brief xxx
 * @author Dirk Schwanenberg
 * @version 1.0
 * @date 2013
 */

#ifndef LINEAREQUATIONSOLVERS_H
#define LINEAREQUATIONSOLVERS_H


namespace rtctools
{
namespace utilities
{

void gj(int n, double** A, double* b, double* x);
void gj(int n, double** Ab);

} // end namespace utilities
} // end namespace rtctools

#endif /* LINEAREQUATIONSOLVERS_H */

