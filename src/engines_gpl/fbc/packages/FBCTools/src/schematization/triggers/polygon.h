// Copyright (C) 2010 Deltares
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
 * @date 2010
 */

#ifndef POLYGON_H
#define POLYGON_H

#include <vector>

namespace rtctools
{
namespace schematization
{
namespace triggers
{

class polygon
{
public:
	polygon(void) {};
	polygon(int nEdges, std::vector<double>& x, std::vector<double>& y, double value);
	~polygon(void) {};

	bool contains(double tx, double ty);
	double getNEdges() { return nEdges; };
	double getValue() { return value; };

private:
	int nEdges;
	std::vector<double> x;
	std::vector<double> y;
    double value;

	/**
	 * Classifies the polygon specified by the constructor. Returns false if the polygon
	 * is complex (has intersecting edges). Returns true if it is simple and convex.
	 * Returns true if it is simple and concave.
	 *
	 * @returns False if the polygon is complex. True if the polygon is simple and 
	 *			either convex or concave.
	 **/
	bool isSimple();
};

} // end namespace triggers
} // end namespace schematization
} // end namespace rtctools

#endif /* POLYGON_H */
