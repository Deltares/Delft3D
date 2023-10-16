!    Copyright 2011 Davide Cesari <dcesari69 at gmail dot com>
!
!    This file is part of FortranGIS.
!
!    FortranGIS is free software: you can redistribute it and/or modify
!    it under the terms of the GNU Lesser General Public License as
!    published by the Free Software Foundation, either version 3 of the
!    License, or (at your option) any later version.
!
!    FortranGIS is distributed in the hope that it will be useful, but
!    WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!    Lesser General Public License for more details.
!
!    You should have received a copy of the GNU Lesser General Public
!    License along with FortranGIS.  If not, see
!    <http://www.gnu.org/licenses/>.

!> Fortran 2003 interface to the proj https://proj.org/ library, API version 6.
!! The following functions and subroutines are directly interfaced to
!! their corresponding C version, so they are undocumented here,
!! please refer to the original proj C API documentation, e.g. at the
!! address https://proj.org/development/reference/index.html , for their
!! use:
!!  - proj_context_create()
!!  - proj_context_destroy(ctx)
!!  - proj_context_clone(ctx)
!!  - proj_create(ctx, definition)
!!  - proj_create_crs_to_crs(ctx, source_crs, target_crs, area)
!!  - proj_create_crs_to_crs_from_pj(ctx, source_crs, target_crs, area, options)
!!  - proj_normalize_for_visualization(ctx, object)
!!  - proj_destroy(pj)
!!  - proj_trans(p, direction, coord)
!!  - proj_trans_array(p, direction, n, coord)
!!  - proj_info()
!!  - proj_pj_info(p)
!!  - proj_get_type(obj)
!!  - proj_torad(angle_in_degrees)
!!  - proj_todeg(angle_in_radians)
!!  - proj_area_set_bbox(area, west_lon_degree, south_lat_degree, east_lon_degree, north_lat_degree)
!!  - proj_area_destroy(area)
!!
!! Some of these functions have also a more Fortran-friendly interface
!! explicitely documented here, with an \a _f appended to the name.
!!
!! For an example of application of the \a proj6 module, please refer
!! to the following test program, which performs a forward and
!! backward transformation:
!! \include proj6_test.F90
!!
!! \ingroup libfortrangis
MODULE proj6
USE,INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

!> Object describing a projection or a transformation.
!! It is the equivalent of the PJ C type. Its components are private
!! so they should not be manipulated directly.
TYPE,BIND(C) :: pj_object
  PRIVATE
  TYPE(c_ptr) :: ptr = C_NULL_PTR
END TYPE pj_object

!> Object representing a null projection or transformation.
!! It should be used for comparison of function results
!! with the \a == operator for error checking.
TYPE(pj_object),PARAMETER :: pj_object_null=pj_object(C_NULL_PTR)

!> Object describing a proj multithread context.
!! It is the equivalent of the PJ_CONTEXT C type. Its components are
!! private so they should not be manipulated directly.
TYPE,BIND(C) :: pj_context_object
  PRIVATE
  TYPE(c_ptr) :: ptr = C_NULL_PTR
END TYPE pj_context_object

!> Object representing the default context.
!! It should be used in single threaded programs.
TYPE(pj_context_object),PARAMETER :: pj_default_ctx=pj_context_object(C_NULL_PTR)

!> Object describing a general-purpose georeferenced coordinate.
!! It is the equivalent of the PJ_COORD C type. Depending on the
!! context, the 4 or less coordinate values may be intepreted in
!! different ways. By default the first 2 values (usually x and y) are
!! initialised to HUGE (invalid), while the following (usually z and
!! t) are initialised to 0.
TYPE,BIND(C) :: pj_coord_object
  REAL(kind=c_double) :: x=HUGE(1.0_c_double) !< x, 1st axis, longitude, easting...
  REAL(kind=c_double) :: y=HUGE(1.0_c_double) !< y, 2nd axis, latitude, northing...
  REAL(kind=c_double) :: z=0.0_c_double !< z
  REAL(kind=c_double) :: t=0.0_c_double !< time
END TYPE pj_coord_object

!> Object describing a proj area.
!! Its components are private so they should not be manipulated
!! directly.
TYPE,BIND(C) :: pj_area_object
  PRIVATE
  TYPE(c_ptr) :: ptr = C_NULL_PTR
END TYPE pj_area_object

!> Object strictly reflecting C PJ_INFO structure.
TYPE,BIND(C) :: pj_info_object
 INTEGER(kind=c_int) :: major !< Major release number
 INTEGER(kind=c_int) :: minor !< Minor release number
 INTEGER(kind=c_int) :: patch !< Patch level
 TYPE(c_ptr) :: release !< Release info. Version + date
 TYPE(c_ptr) :: version !< Full version number
 TYPE(c_ptr) :: searchpath !< Paths where init and grid files are looked for
 TYPE(c_ptr) :: paths !< C char**, use c_ptr_ptr for decoding in fortran
 INTEGER(kind=c_size_t) :: path_count
END TYPE pj_info_object

!> Object strictly reflecting C PJ_PROJ_INFO structure.
TYPE,BIND(C) :: pj_proj_info_object
  TYPE(c_ptr) :: id !< Name of the projection in question
  TYPE(c_ptr) :: description !< Description of the projection
  TYPE(c_ptr) :: definition !< Projection definition
  INTEGER(kind=c_int) :: has_inverse !< 1 if an inverse mapping exists, 0 otherwise
  REAL(kind=c_double) :: accuracy !< Expected accuracy of the transformation. -1 if unknown
END TYPE pj_proj_info_object

!> Enumerator indicating the type of projection. It is the equivalent of
!! PJ_TYPE in the C API.
ENUM, BIND(c)
  ENUMERATOR :: PJ_TYPE_UNKNOWN, &
   PJ_TYPE_ELLIPSOID, &
   PJ_TYPE_PRIME_MERIDIAN, &
   PJ_TYPE_GEODETIC_REFERENCE_FRAME, PJ_TYPE_DYNAMIC_GEODETIC_REFERENCE_FRAME, &
   PJ_TYPE_VERTICAL_REFERENCE_FRAME, PJ_TYPE_DYNAMIC_VERTICAL_REFERENCE_FRAME, &
   PJ_TYPE_DATUM_ENSEMBLE, &
   PJ_TYPE_CRS, &
   PJ_TYPE_GEODETIC_CRS, PJ_TYPE_GEOCENTRIC_CRS, &
   PJ_TYPE_GEOGRAPHIC_CRS, PJ_TYPE_GEOGRAPHIC_2D_CRS, PJ_TYPE_GEOGRAPHIC_3D_CRS, &
    PJ_TYPE_VERTICAL_CRS, PJ_TYPE_PROJECTED_CRS, PJ_TYPE_COMPOUND_CRS, &
    PJ_TYPE_TEMPORAL_CRS, PJ_TYPE_ENGINEERING_CRS, PJ_TYPE_BOUND_CRS, PJ_TYPE_OTHER_CRS, &
    PJ_TYPE_CONVERSION, PJ_TYPE_TRANSFORMATION, PJ_TYPE_CONCATENATED_OPERATION, &
    PJ_TYPE_OTHER_COORDINATE_OPERATION, &
    PJ_TYPE_TEMPORAL_DATUM, PJ_TYPE_ENGINEERING_DATUM, PJ_TYPE_PARAMETRIC_DATUM
END ENUM

!> Enumerator indicating projection direction. It is the equivalent of
!! PROJ_DIRECTION in the C API.
ENUM, BIND(c)
  ENUMERATOR :: &
   PJ_FWD = 1, &
   PJ_IDENT= 0, &
   PJ_INV = -1
END ENUM

INTERFACE
  FUNCTION proj_context_create() BIND(C,name='proj_context_create')
  IMPORT
  TYPE(pj_context_object) :: proj_context_create
  END FUNCTION proj_context_create
END INTERFACE

INTERFACE
  FUNCTION proj_context_destroy(ctx) BIND(C,name='proj_context_destroy')
  IMPORT
  TYPE(pj_context_object),VALUE :: ctx
  TYPE(pj_context_object) :: proj_context_destroy
  END FUNCTION proj_context_destroy
END INTERFACE

INTERFACE
  FUNCTION proj_context_clone(ctx) BIND(C,name='proj_context_clone')
  IMPORT
  TYPE(pj_context_object),VALUE :: ctx
  TYPE(pj_context_object) :: proj_context_clone
  END FUNCTION proj_context_clone
END INTERFACE

INTERFACE
  FUNCTION proj_create(ctx, definition) BIND(C,name='proj_create')
  IMPORT
  TYPE(pj_context_object),VALUE :: ctx
  CHARACTER(kind=c_char) :: definition(*) !< definition string, must be terminated by //CHAR(0)
  TYPE(pj_object) :: proj_create
  END FUNCTION proj_create
END INTERFACE

INTERFACE
  FUNCTION proj_create_crs_to_crs(ctx, source_crs, target_crs, area) &
   BIND(C,name='proj_create_crs_to_crs')
  IMPORT
  TYPE(pj_context_object),VALUE :: ctx
  CHARACTER(kind=c_char) :: source_crs(*) !< Source projection string, must be terminated by //CHAR(0)
  CHARACTER(kind=c_char) :: target_crs(*) !< Target projection string, must be terminated by //CHAR(0)
  TYPE(pj_area_object),VALUE :: area
  TYPE(pj_object) :: proj_create_crs_to_crs
  END FUNCTION proj_create_crs_to_crs
END INTERFACE

INTERFACE
  FUNCTION proj_create_crs_to_crs_from_pj(ctx, source_crs, target_crs, area, options) &
   BIND(C,name='proj_create_crs_to_crs_from_pj')
  IMPORT
  TYPE(pj_context_object),VALUE :: ctx
  TYPE(pj_object),VALUE :: source_crs
  TYPE(pj_object),VALUE :: target_crs
  TYPE(pj_area_object),VALUE :: area
  TYPE(c_ptr),VALUE :: options !< char**, use c_ptr_ptr from libfortranc to create it in fortran
  TYPE(pj_object) :: proj_create_crs_to_crs_from_pj
  END FUNCTION proj_create_crs_to_crs_from_pj
END INTERFACE

INTERFACE
  FUNCTION proj_normalize_for_visualization(ctx, object) BIND(C,name='proj_normalize_for_visualization')
  IMPORT
  TYPE(pj_context_object),VALUE :: ctx
  TYPE(pj_object),VALUE :: object
  TYPE(pj_object) :: proj_normalize_for_visualization
  END FUNCTION proj_normalize_for_visualization
END INTERFACE

INTERFACE
  FUNCTION proj_destroy(pj) BIND(C,name='proj_destroy')
  IMPORT
  TYPE(pj_object),VALUE :: pj
  TYPE(pj_object) :: proj_destroy
  END FUNCTION proj_destroy
END INTERFACE

INTERFACE
  SUBROUTINE proj_area_set_bbox(area, west_lon_degree, south_lat_degree, &
   east_lon_degree, north_lat_degree) BIND(C,name='proj_area_set_bbox')
  IMPORT
  TYPE(pj_area_object),VALUE :: area
  REAL(kind=c_double),VALUE :: west_lon_degree
  REAL(kind=c_double),VALUE :: south_lat_degree
  REAL(kind=c_double),VALUE :: east_lon_degree
  REAL(kind=c_double),VALUE :: north_lat_degree
  END SUBROUTINE proj_area_set_bbox
END INTERFACE

INTERFACE
  SUBROUTINE proj_area_destroy(area) BIND(C,name='proj_area_destroy')
  IMPORT
  TYPE(pj_area_object),VALUE :: area
  END SUBROUTINE proj_area_destroy
END INTERFACE

INTERFACE
  FUNCTION proj_trans(p, direction, coord) BIND(C,name='proj_trans')
  IMPORT
  TYPE(pj_object),VALUE :: p
  INTEGER(kind=kind(PJ_FWD)),VALUE :: direction ! warning this is an enum
  TYPE(pj_coord_object),VALUE :: coord
  TYPE(pj_coord_object) :: proj_trans
  END FUNCTION proj_trans
END INTERFACE

! TODO implement a fortran-style array function with assumed shape arrays
INTERFACE
  FUNCTION proj_trans_array(p, direction, n, coord) BIND(C,name='proj_trans_array')
  IMPORT
  TYPE(pj_object),VALUE :: p
  INTEGER(kind=KIND(PJ_FWD)),VALUE :: direction ! warning this is an enum
  INTEGER(kind=c_size_t),VALUE :: n
  TYPE(pj_coord_object) :: coord(*)
  INTEGER(kind=c_int) :: proj_trans_array
  END FUNCTION proj_trans_array
END INTERFACE

INTERFACE
  FUNCTION proj_info() BIND(C,name='proj_info')
  IMPORT
  TYPE(pj_info_object) :: proj_info
  END FUNCTION proj_info
END INTERFACE

INTERFACE
  FUNCTION proj_pj_info(p) BIND(C,name='proj_pj_info')
  IMPORT
  TYPE(pj_object),VALUE :: p
  TYPE(pj_proj_info_object) :: proj_pj_info
  END FUNCTION proj_pj_info
END INTERFACE

INTERFACE
  FUNCTION proj_get_type(obj) BIND(C,name='proj_get_type')
  IMPORT
  TYPE(pj_object),VALUE :: obj
  INTEGER(kind=kind(PJ_TYPE_UNKNOWN)) :: proj_get_type
  END FUNCTION proj_get_type
END INTERFACE

INTERFACE
  FUNCTION proj_torad(angle_in_degrees) BIND(C,name='proj_torad')
  IMPORT
  REAL(kind=c_double) :: angle_in_degrees
  REAL(kind=c_double) :: proj_torad
  END FUNCTION proj_torad
END INTERFACE

INTERFACE
  FUNCTION proj_todeg(angle_in_radians) BIND(C,name='proj_todeg')
  IMPORT
  REAL(kind=c_double) :: angle_in_radians
  REAL(kind=c_double) :: proj_todeg
  END FUNCTION proj_todeg
END INTERFACE

INTERFACE
  FUNCTION proj_errno(p) BIND(C,name='proj_errno')
  IMPORT
  TYPE(pj_object),VALUE :: p
  INTEGER(kind=c_int) :: proj_errno
  END FUNCTION proj_errno
END INTERFACE

INTERFACE
  FUNCTION proj_context_errno(ctx) BIND(C,name='proj_context_errno')
  IMPORT
  TYPE(pj_context_object),VALUE :: ctx
  INTEGER(kind=c_int) :: proj_context_errno
  END FUNCTION proj_context_errno
END INTERFACE

INTERFACE
  FUNCTION proj_errno_string(err) BIND(C,name='proj_errno_string')
  IMPORT
  INTEGER(kind=c_int),VALUE :: err
  TYPE(c_ptr) :: proj_errno_string
  END FUNCTION proj_errno_string
END INTERFACE

INTERFACE
  FUNCTION proj_context_errno_string(ctx, err) BIND(C,name='proj_context_errno_string')
  IMPORT
  TYPE(pj_context_object),VALUE :: ctx
  INTEGER(kind=c_int),VALUE :: err
  TYPE(c_ptr) :: proj_context_errno_string
  END FUNCTION proj_context_errno_string
END INTERFACE


!> Test whether an opaque object is valid.
!! This is a generic interface, it can be called with objects of type
!! pj_object, pj_context_object, pj_area_object. It returns .TRUE. if
!! the object has been associated or correctly initialised.
!!
!! \param object the object to be tested
INTERFACE proj_associated
  MODULE PROCEDURE proj_associated_pj, proj_associated_context, &
   proj_associated_area
END INTERFACE proj_associated

! TODO
! pj_latlong_from_proj: proj_crs_get_horizontal_datum() and proj_create_geographic_crs_from_datum()?

CONTAINS

FUNCTION proj_associated_pj(object) RESULT(associated_)
TYPE(pj_object),INTENT(in) :: object
LOGICAL :: associated_
associated_ = C_ASSOCIATED(object%ptr)
END FUNCTION proj_associated_pj

FUNCTION proj_associated_context(object) RESULT(associated_)
TYPE(pj_context_object),INTENT(in) :: object
LOGICAL :: associated_
associated_ = C_ASSOCIATED(object%ptr)
END FUNCTION proj_associated_context

FUNCTION proj_associated_area(object) RESULT(associated_)
TYPE(pj_area_object),INTENT(in) :: object
LOGICAL :: associated_
associated_ = C_ASSOCIATED(object%ptr)
END FUNCTION proj_associated_area

!> Fortran-style interface to proj_trans_array
FUNCTION proj_trans_f(p, direction, coord)
TYPE(pj_object),VALUE :: p !< transfor
INTEGER(kind=KIND(PJ_FWD)),VALUE :: direction ! warning this is an enum
TYPE(pj_coord_object) :: coord(:)
INTEGER :: proj_trans_f

INTEGER(kind=c_size_t) :: n

n = SIZE(coord)
proj_trans_f = proj_trans_array(p, direction, n, coord)

END FUNCTION proj_trans_f


END MODULE proj6
