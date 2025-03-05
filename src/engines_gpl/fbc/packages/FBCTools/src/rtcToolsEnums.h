// Copyright (C) 2012 Deltares
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
 * @brief rtcRuntimeConfigSettings.h
 * @author
 * @version 1.0
 * @date 2012
 */

#ifndef RTCTOOLSENUMS_H
#define RTCTOOLSENUMS_H

enum runModeEnum
{
    SIMULATE,
    FIRSTORDERSENSITIVITY,
    OPTIMIZE,
    CLOSEDLOOP,
    POSTPROCESSING
};

enum simPeriodEnum
{
	COMPLETE,
    UPDATE,
    FORECAST
};

enum optimizerEnum
{
	GAMS,
    IPOPT,
    SA
};

enum optimizerExecutionModeEnum
{
    SEQUENTIAL,
    PARALLEL
};

enum fileTypeEnum
{
    TREEVECTOR,
    PIMODELPARAMETERS
};

enum interpolationOption
{
	NONE,
	BLOCK,
	LINEAR,
	PERIODIC
};

enum prefixEnum
{
	PREFIX_NONE,
	PREFIX_LOCATIONID
};

#endif //RTCTOOLSENUMS_H