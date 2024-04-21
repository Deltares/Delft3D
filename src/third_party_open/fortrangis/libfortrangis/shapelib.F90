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

!> Fortran 2003 interface to the shapelib http://shapelib.maptools.org/
!! library.
!! This module defines an API which reflects the original shapelib C
!! API with some modifications:
!!
!!  - \a shpopen, \a shpcreate and \a shpclose functions act also on
!!    .dbf files
!!  - \a shpgetinfo accesses also .dbf information
!!  - the \a DBFRead*Attribute and \a DBWrite*Attribute are converted
!!    into two f90 interfaces called \a dbfreadattribute and \a
!!    dbwriteattribute respectively.
!!
!! The module defines two derived types: \a shpfileobject associated
!! to a shapefile dataset, and \a shpobject associated to a single
!! shape within a dataset. Access to database (.dbf) information is
!! done by accessing the file object only.
!!
!! For an example of application of the \a shapelib module, please
!! refer to the following test program, which creates a shapefile and
!! successively reads it:
!! \include shapelib_test.F90
!!
!! \ingroup libfortrangis
MODULE shapelib
USE,INTRINSIC :: ISO_C_BINDING
USE fortranc
IMPLICIT NONE

INTEGER,PARAMETER :: shpt_null = 0 !< Series of constants for specifying type of new shape datasets with \a shpcreate, null shape
INTEGER,PARAMETER :: shpt_point = 1 !< points
INTEGER,PARAMETER :: shpt_arc = 3 !< arcs (Polylines, possible in parts)
INTEGER,PARAMETER :: shpt_polygon = 5 !< polygons (possible in parts)
INTEGER,PARAMETER :: shpt_multipoint = 8 !< multiPoint (related points)
INTEGER,PARAMETER :: shpt_pointz = 11 !< 3D (+ measure) points
INTEGER,PARAMETER :: shpt_arcz = 13 !< 3D (+ measure) arcs
INTEGER,PARAMETER :: shpt_polygonz = 15 !< 3D (+ measure) polygons
INTEGER,PARAMETER :: shpt_multipointz = 18 !< 3D (+ measure) multiPoint
INTEGER,PARAMETER :: shpt_pointm = 21 !< 2D + measure points
INTEGER,PARAMETER :: shpt_arcm = 23 !< 2D + measure arcs
INTEGER,PARAMETER :: shpt_polygonm = 25 !< 2D + measure polygons
INTEGER,PARAMETER :: shpt_multipointm = 28 !< 2D + measure multiPoint

INTEGER,PARAMETER :: shpt_multipatch = 31 !< complex (TIN-like) with Z, and Measure

INTEGER,PARAMETER :: ftstring = 0 !< Series of constants for specifying dbf field type, fixed length string field
INTEGER,PARAMETER :: ftinteger = 1 !< numeric field with no decimals
INTEGER,PARAMETER :: ftdouble = 2 !< numeric field with decimals
INTEGER,PARAMETER :: ftlogical = 3 !< LOGICAL field
INTEGER,PARAMETER :: ftinvalid = 4 !< not a recognised field TYPE


!> Object describing a shapefile dataset.
!! Its components are private so they should not be manipulated
!! directly.
TYPE shpfileobject
  PRIVATE
  TYPE(c_ptr) :: shpfile_orig=c_null_ptr
  TYPE(c_ptr) :: dbffile_orig=c_null_ptr
END TYPE shpfileobject


!> Object describing the geometrical properties of a shape.
!! It is used for reading a shape, or for manipulating a newly created
!! shape; in the latter case the single values can be changed, but not
!! the size of the arrays.
TYPE shpobject
  TYPE(c_ptr) :: shpobject_orig=c_null_ptr !< pointer to C information, it should not be used
  INTEGER :: nshptype=0 !< shape type, one of the \a shpt_* constants defined
  INTEGER :: nshapeid=-1 !< shape number (-1 is unknown/unassigned)
  INTEGER :: nparts=0 !< number of parts (0 implies single part with no info)
  INTEGER,POINTER :: panpartstart(:)=>NULL() !< starting vertex of each part
  INTEGER,POINTER :: panparttype(:)=>NULL() !< part type (SHPP_RING if not SHPT_MULTIPATCH)
  INTEGER :: nvertices !< number of vertices
  REAL(kind=c_double),POINTER :: padfx(:)=>NULL() !< x coordinates of vertices
  REAL(kind=c_double),POINTER :: padfy(:)=>NULL() !< y coordinates of vertices
  REAL(kind=c_double),POINTER :: padfz(:)=>NULL() !< z coordinates of vertices
  REAL(kind=c_double),POINTER :: padfm(:)=>NULL() !< measure of vertices
  REAL(kind=c_double) :: dfxmin=0.0_c_double !< lower bound in x dimension
  REAL(kind=c_double) :: dfymin=0.0_c_double !< lower bound in y dimension
  REAL(kind=c_double) :: dfzmin=0.0_c_double !< lower bound in z dimension
  REAL(kind=c_double) :: dfmmin=0.0_c_double !< lower bound in measure dimension
  REAL(kind=c_double) :: dfxmax=0.0_c_double !< upper bound in x dimension
  REAL(kind=c_double) :: dfymax=0.0_c_double !< upper bound in y dimension
  REAL(kind=c_double) :: dfzmax=0.0_c_double !< upper bound in z dimension
  REAL(kind=c_double) :: dfmmax=0.0_c_double !< upper bound in measure dimension
END TYPE shpobject

!TYPE(shpfileobject),PARAMETER :: shpfileobject_null = shpfileobject(0, 0)
TYPE(shpobject),PARAMETER :: shpobject_null = shpobject(c_null_ptr, &
 0, -1, 0, &
 NULL(), NULL(), 0, NULL(), NULL(), NULL(), NULL(), &
 0.0_c_double, 0.0_c_double, 0.0_c_double, 0.0_c_double, &
 0.0_c_double, 0.0_c_double, 0.0_c_double, 0.0_c_double)

!> Interface to SUBROUTINEs for reading dbf attributes.
!! The type of the attribute can be either INTEGER,
!! REAL(kind=c_double) (double) or CHARACTER. In case of CHARACTER
!! attributes it is important that the length of the string passed is
!! big enough to contain the attribute. The maximum length for each
!! field can be obtained with the \a dbfgetfieldinfo function, but it
!! is limited anyway to a maximum of 512 characters. The type of the
!! attribute requested may not coincide with the native type of the
!! field, if possible a conversion will be performed.
!!
!! \param hshp TYPE(shpfileobject),INTENT(inout) shapefile object to query
!! \param ishape INTEGER,INTENT(in) the number of shape to query
!! \param ifield INTEGER,INTENT(in) the number of field to query
!! \param attr INTEGER, CHARACTER or REAL(kind=c_double), INTENT(out) the value of the attribute
INTERFACE dbfreadattribute
  MODULE PROCEDURE dbfreadintegerattribute_f, dbfreaddoubleattribute_f, &
   dbfreadstringattribute_f
END INTERFACE


!> Interface to FUNCTIONs for setting dbf attributes.
!! The type of the attribute can be either INTEGER,
!! REAL(kind=c_double) (double) or CHARACTER. The type of the
!! attribute provided may not coincide with the native type of the
!! field, if possible a conversion will be performed. If the \a attr
!! parameter is not provided the attribute will be set to a null
!! value.
!!
!! \param hshp TYPE(shpfileobject),INTENT(inout) shapefile object to set
!! \param ishape INTEGER,INTENT(in) the number of shape to set
!! \param ifield INTEGER,INTENT(in) the number of field to set
!! \param attr INTEGER, CHARACTER or REAL(kind=c_double), INTENT(in) the value of the attribute to set
INTERFACE dbfwriteattribute
  MODULE PROCEDURE dbfwriteintegerattribute_f, dbfwritedoubleattribute_f, &
   dbfwritestringattribute_f, dbfwritenullattribute_f
END INTERFACE


INTERFACE
  FUNCTION shpopen_orig(pszlayer, pszaccess) BIND(C,name='SHPOpen')
  IMPORT
  CHARACTER(kind=c_char) :: pszlayer(*)
  CHARACTER(kind=c_char) :: pszaccess(*)
  TYPE(c_ptr) :: shpopen_orig
  END FUNCTION shpopen_orig

  SUBROUTINE shpclose_orig(psshp) BIND(C,name='SHPClose')
  IMPORT
  TYPE(c_ptr),VALUE :: psshp
  END SUBROUTINE shpclose_orig

  SUBROUTINE shpgetinfo_orig(psshp, pnentities, pnshapetype, padfminbound, padfmaxbound) BIND(C,name='SHPGetInfo')
  IMPORT
  TYPE(c_ptr),VALUE :: psshp
  INTEGER(kind=c_int) :: pnentities
  INTEGER(kind=c_int) :: pnshapetype
  REAL(kind=c_double) :: padfminbound(*)
  REAL(kind=c_double) :: padfmaxbound(*)
  END SUBROUTINE shpgetinfo_orig

  FUNCTION shpcreate_orig(pszlayer, nshapetype) BIND(C,name='SHPCreate')
  IMPORT
  CHARACTER(kind=c_char) :: pszlayer(*)
  INTEGER(kind=c_int),VALUE :: nshapetype
  TYPE(c_ptr) :: shpcreate_orig
  END FUNCTION shpcreate_orig

  SUBROUTINE shpcomputeextents_int(psobject, ftnobject) BIND(C,name='SHPComputeExtentsInt')
  IMPORT
  TYPE(c_ptr),VALUE :: psobject
  TYPE(c_ptr),VALUE :: ftnobject
  END SUBROUTINE shpcomputeextents_int

  FUNCTION shpcreateobject_int(nshptype, nshapeid, nparts, panpartstart, panparttype, &
   nvertices, padfx, padfy, padfz, padfm, ftnobject) BIND(C,name='SHPCreateObjectInt')
  IMPORT
  INTEGER(kind=c_int),VALUE :: nshptype
  INTEGER(kind=c_int),VALUE :: nshapeid
  INTEGER(kind=c_int),VALUE :: nparts
  INTEGER(kind=c_int) :: panpartstart(*)
  INTEGER(kind=c_int) :: panparttype(*)
  INTEGER(kind=c_int),VALUE :: nvertices
  REAL(kind=c_double) :: padfx(*)
  REAL(kind=c_double) :: padfy(*)
  REAL(kind=c_double) :: padfz(*)
  REAL(kind=c_double) :: padfm(*)
  TYPE(c_ptr),VALUE :: ftnobject
  INTEGER(kind=c_int) :: shpcreateobject_int
  END FUNCTION shpcreateobject_int

  FUNCTION shpcreatesimpleobject_int(nshptype, nvertices, padfx, padfy, padfz, ftnobject) BIND(C,name='SHPCreateSimpleObjectInt')
  IMPORT
  INTEGER(kind=c_int),VALUE :: nshptype
  INTEGER(kind=c_int),VALUE :: nvertices
  REAL(kind=c_double) :: padfx(*)
  REAL(kind=c_double) :: padfy(*)
  REAL(kind=c_double) :: padfz(*)
  TYPE(c_ptr),VALUE :: ftnobject
  INTEGER(kind=c_int) :: shpcreatesimpleobject_int
  END FUNCTION shpcreatesimpleobject_int

  FUNCTION shpwriteobject_orig(psshp, nshapeid, psobject) BIND(C,name='SHPWriteObject')
  IMPORT
  TYPE(c_ptr),VALUE :: psshp
  INTEGER(kind=c_int),VALUE :: nshapeid
  TYPE(c_ptr),VALUE :: psobject
  INTEGER(kind=c_int) :: shpwriteobject_orig
  END FUNCTION shpwriteobject_orig

  FUNCTION shpreadobject_int(psshp, hentity, ftnobject) BIND(C,name='SHPReadObjectInt')
  IMPORT
  TYPE(c_ptr),VALUE :: psshp
  INTEGER(kind=c_int),VALUE :: hentity
  TYPE(c_ptr),VALUE :: ftnobject
  INTEGER(kind=c_int) :: shpreadobject_int
  END FUNCTION shpreadobject_int

  SUBROUTINE shpdestroyobject_orig(psshape) BIND(C,name='SHPDestroyObject')
  IMPORT
  TYPE(c_ptr),VALUE :: psshape
  END SUBROUTINE shpdestroyobject_orig

#ifndef SHAPELIB_PRE10
  FUNCTION shprewindobject_int(hshp, psobject, ftnobject) BIND(C,name='SHPRewindObjectInt')
  IMPORT
  TYPE(c_ptr),VALUE :: hshp
  TYPE(c_ptr),VALUE :: psobject
  TYPE(c_ptr),VALUE :: ftnobject
  INTEGER(kind=c_int) :: shprewindobject_int
  END FUNCTION shprewindobject_int
#endif
END INTERFACE

INTERFACE
  FUNCTION dbfopen(pszfilename, pszaccess) BIND(C,name='DBFOpen')
  IMPORT
  CHARACTER(kind=c_char) :: pszfilename(*)
  CHARACTER(kind=c_char) :: pszaccess(*)
  TYPE(c_ptr) :: dbfopen
  END FUNCTION dbfopen

  SUBROUTINE dbfclose(psdbf) BIND(C,name='DBFClose')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  END SUBROUTINE dbfclose

  FUNCTION dbfcreate(pszfilename) BIND(C,name='DBFCreate')
  IMPORT
  CHARACTER(kind=c_char) :: pszfilename(*)
  TYPE(c_ptr) :: dbfcreate
  END FUNCTION dbfcreate

  FUNCTION dbfaddfield_orig(psdbf, pszfieldname, etype, nwidth, ndecimals) BIND(C,name='DBFAddField')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  CHARACTER(kind=c_char) :: pszfieldname(*)
  INTEGER(kind=c_int),VALUE :: etype
  INTEGER(kind=c_int),VALUE :: nwidth
  INTEGER(kind=c_int),VALUE :: ndecimals
  INTEGER(kind=c_int) :: dbfaddfield_orig
  END FUNCTION dbfaddfield_orig

  FUNCTION dbfreadintegerattribute_orig(psdbf, irecord, ifield) BIND(C,name='DBFReadIntegerAttribute')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  INTEGER(kind=c_int),VALUE :: irecord
  INTEGER(kind=c_int),VALUE :: ifield
  INTEGER(kind=c_int) :: dbfreadintegerattribute_orig
  END FUNCTION dbfreadintegerattribute_orig

  FUNCTION dbfreaddoubleattribute_orig(psdbf, irecord, ifield) BIND(C,name='DBFReadDoubleAttribute')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  INTEGER(kind=c_int),VALUE :: irecord
  INTEGER(kind=c_int),VALUE :: ifield
  REAL(kind=c_double) :: dbfreaddoubleattribute_orig
  END FUNCTION dbfreaddoubleattribute_orig

  FUNCTION dbfreadstringattribute_orig(psdbf, irecord, ifield) BIND(C,name='DBFReadStringAttribute')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  INTEGER(kind=c_int),VALUE :: irecord
  INTEGER(kind=c_int),VALUE :: ifield
  TYPE(c_ptr) :: dbfreadstringattribute_orig
  END FUNCTION dbfreadstringattribute_orig

  SUBROUTINE dbfreadstringattribute_int(psdbf, irecord, ifield, attr, lattr) BIND(C,name='DBFReadStringAttributeInt')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  INTEGER(kind=c_int),VALUE :: irecord
  INTEGER(kind=c_int),VALUE :: ifield
  CHARACTER(kind=c_char) :: attr(*)
  INTEGER(kind=c_int),VALUE :: lattr
  END SUBROUTINE dbfreadstringattribute_int

  FUNCTION dbfreadlogicalattribute(psdbf, irecord, ifield) BIND(C,name='DBFReadLogicalAttribute')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  INTEGER(kind=c_int),VALUE :: irecord
  INTEGER(kind=c_int),VALUE :: ifield
  CHARACTER(kind=c_char) :: dbfreadlogicalattribute
  END FUNCTION dbfreadlogicalattribute

#ifndef SHAPELIB_PRE10
  FUNCTION dbfisattributenull_orig(psdbf, irecord, ifield) BIND(C,name='DBFIsAttributeNULL')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  INTEGER(kind=c_int),VALUE :: irecord
  INTEGER(kind=c_int),VALUE :: ifield
  INTEGER(kind=c_int) :: dbfisattributenull_orig
  END FUNCTION dbfisattributenull_orig
#endif

  FUNCTION dbfgetfieldcount(psdbf) BIND(C,name='DBFGetFieldCount')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  INTEGER(kind=c_int) :: dbfgetfieldcount
  END FUNCTION dbfgetfieldcount

  FUNCTION dbfgetrecordcount(psdbf) BIND(C,name='DBFGetRecordCount')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  INTEGER(kind=c_int) :: dbfgetrecordcount
  END FUNCTION dbfgetrecordcount

  FUNCTION dbfgetfieldinfo_orig(psdbf, ifield, pszfieldname, pnwidth, pndecimals) BIND(C,name='DBFGetFieldInfo')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  INTEGER(kind=c_int),VALUE :: ifield
  CHARACTER(kind=c_char) :: pszfieldname(*)
  INTEGER(kind=c_int) :: pnwidth
  INTEGER(kind=c_int) :: pndecimals
  INTEGER(kind=c_int) :: dbfgetfieldinfo_orig
  END FUNCTION dbfgetfieldinfo_orig

  FUNCTION dbfwritedoubleattribute(psdbf, irecord, ifield, dvalue) BIND(C,name='DBFWriteDoubleAttribute')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  INTEGER(kind=c_int),VALUE :: irecord
  INTEGER(kind=c_int),VALUE :: ifield
  REAL(kind=c_double),VALUE :: dvalue
  INTEGER(kind=c_int) :: dbfwritedoubleattribute
  END FUNCTION dbfwritedoubleattribute

  FUNCTION dbfwriteintegerattribute(psdbf, irecord, ifield, nvalue) BIND(C,name='DBFWriteIntegerAttribute')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  INTEGER(kind=c_int),VALUE :: irecord
  INTEGER(kind=c_int),VALUE :: ifield
  INTEGER(kind=c_int),VALUE :: nvalue
  INTEGER(kind=c_int) :: dbfwriteintegerattribute
  END FUNCTION dbfwriteintegerattribute

  FUNCTION dbfwritestringattribute(psdbf, irecord, ifield, pszvalue) BIND(C,name='DBFWriteStringAttribute')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  INTEGER(kind=c_int),VALUE :: irecord
  INTEGER(kind=c_int),VALUE :: ifield
  CHARACTER(kind=c_char) :: pszvalue(*)
  INTEGER(kind=c_int) :: dbfwritestringattribute
  END FUNCTION dbfwritestringattribute

  FUNCTION dbfwritenullattribute(psdbf, irecord, ifield) BIND(C,name='DBFWriteNULLAttribute')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  INTEGER(kind=c_int),VALUE :: irecord
  INTEGER(kind=c_int),VALUE :: ifield
  INTEGER(kind=c_int) :: dbfwritenullattribute
  END FUNCTION dbfwritenullattribute

  FUNCTION dbfwritelogicalattribute(psdbf, irecord, ifield, lvalue) BIND(C,name='DBFWriteLogicalAttribute')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  INTEGER(kind=c_int),VALUE :: irecord
  INTEGER(kind=c_int),VALUE :: ifield
  CHARACTER(kind=c_char),VALUE :: lvalue
  INTEGER(kind=c_int) :: dbfwritelogicalattribute
  END FUNCTION dbfwritelogicalattribute

#ifndef SHAPELIB_PRE10
  FUNCTION dbfgetnativefieldtype_orig(psdbf, ifield) BIND(C,name='DBFGetNativeFieldType')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  INTEGER(kind=c_int),VALUE :: ifield
  INTEGER(kind=c_signed_char) :: dbfgetnativefieldtype_orig
  END FUNCTION dbfgetnativefieldtype_orig

  FUNCTION dbfgetfieldindex_orig(psdbf, pszfieldname) BIND(C,name='DBFGetFieldIndex')
  IMPORT
  TYPE(c_ptr),VALUE :: psdbf
  CHARACTER(kind=c_char) :: pszfieldname(*)
  INTEGER(kind=c_int) :: dbfgetfieldindex_orig
  END FUNCTION dbfgetfieldindex_orig
#endif

END INTERFACE

PRIVATE
PUBLIC shpt_null, shpt_point, shpt_arc, shpt_polygon, shpt_multipoint, &
 shpt_pointz, shpt_arcz, shpt_polygonz, shpt_multipointz, shpt_pointm, &
 shpt_arcm, shpt_polygonm, shpt_multipointm, shpt_multipatch, &
 ftstring, ftinteger, ftdouble, ftlogical, ftinvalid
PUBLIC shpfileobject, shpobject
PUBLIC dbfreadattribute, dbfwriteattribute
PUBLIC shpopen, shpfileisnull, dbffileisnull, shpcreate, shpgetinfo, &
 shpreadobject, shpisnull, shpclose, shpcreatesimpleobject, shpcreateobject, &
 shpcomputeextents, shpwriteobject, shpdestroyobject, &
 dbfgetfieldindex, dbfgetfieldinfo, dbfaddfield, dbfisattributenull, &
 dbfgetnativefieldtype

CONTAINS


!> It tries to open the files composing a shapefile dataset.
!! The filename should be provided without the extension
!! (.shp/.shx/.dbf). It tries to open all files associated, but it
!! does not fail if part of the files are missing, so that it is
!! possible to work on datasets not including .shp/.shx or .dbf parts.
!! The access mode should be "rb" for reading or "rb+" for updating a
!! file.  It returns an object of type \a shpfileobject to be used in
!! the subsequent calls for obtaining both shp and dbf information
!! from the dataset.  The function should not be used for creating a
!! shapefile dataset from scratch, in that case \a shpcreate should be
!! used.
FUNCTION shpopen(pszshapefile, pszaccess)
CHARACTER(len=*),INTENT(in) :: pszshapefile !< filename without extension
CHARACTER(len=*),INTENT(in) :: pszaccess !< file access mode
TYPE(shpfileobject) :: shpopen

shpopen%shpfile_orig = shpopen_orig(fchartrimtostr(pszshapefile), fchartrimtostr(pszaccess))
shpopen%dbffile_orig = dbfopen(fchartrimtostr(pszshapefile), fchartrimtostr(pszaccess))

END FUNCTION shpopen


!> It returns \c .TRUE. if the provided shapefile object is correctly
!! associated with .shp/.shx file pair.  It can be called after \a
!! shpopen to test whether .shp/.shx files have been successfully
!! opened.
FUNCTION shpfileisnull(hshp) RESULT(isnull)
TYPE(shpfileobject),INTENT(in) :: hshp !< shapefile object to test
LOGICAL :: isnull

isnull = .NOT.c_associated(hshp%shpfile_orig)

END FUNCTION shpfileisnull


!> It returns \c .TRUE. if the provided shapefile object is correctly
!! associated with a .dbf file.  It can be called after \a shpopen to
!! test whether .dbf file has been successfully opened.
FUNCTION dbffileisnull(hshp) RESULT(isnull)
TYPE(shpfileobject),INTENT(in) :: hshp !< shapefile object to test
LOGICAL :: isnull

isnull = .NOT.c_associated(hshp%dbffile_orig)

END FUNCTION dbffileisnull


!> It creates a new, empty set of files composing a shapefile dataset.
!! The filename should be provided without the extension
!! (.shp/.shx/.dbf). If the files already exist, they will be
!! overwritten.  The type of shapes should be specified using one of
!! the constants \a shpt_*.  It returns an object of type \a
!! shpfileobject to be used in the subsequent calls for populating the
!! dataset both with shp and dbf information.
FUNCTION shpcreate(pszshapefile, nshapetype)
CHARACTER(len=*),INTENT(in) :: pszshapefile !< filename without extension
INTEGER,INTENT(in) :: nshapetype !< type of shapes in the dataset
TYPE(shpfileobject) :: shpcreate

shpcreate%shpfile_orig = shpcreate_orig(fchartrimtostr(pszshapefile), nshapetype)
shpcreate%dbffile_orig = dbfcreate(fchartrimtostr(pszshapefile))

END FUNCTION shpcreate


!> It gets information about the shapefile database, including dbf.
!! If a part of the dataset has not been correctly opened
!! (e.g. .shp/.shx or .dbf files), the corresponding information, in
!! particular \a nentities or \a dbfrecordcount will be zero.
SUBROUTINE shpgetinfo(hshp, nentities, shapetype, minbound, maxbound, &
 dbffieldcount, dbfrecordcount)
TYPE(shpfileobject),INTENT(in) :: hshp !< shapefile object to query
INTEGER,INTENT(out) :: nentities !< number of shapes
INTEGER,INTENT(out) :: shapetype !< type of shapes in the file, one of the \a shpt_* constants
REAL(kind=c_double),INTENT(out) :: minbound(4) !< lower bounds of shape values
REAL(kind=c_double),INTENT(out) :: maxbound(4) !< upper bounds of shape values
INTEGER,INTENT(out) :: dbffieldcount !< number of dbf fields
INTEGER,INTENT(out) :: dbfrecordcount !< number of dbf records, it should be equal to \a nentities, but it is not guaranteed

IF (.NOT.shpfileisnull(hshp)) THEN
  CALL shpgetinfo_orig(hshp%shpfile_orig, nentities, shapetype, minbound, maxbound)
ELSE
  nentities = 0
  shapetype = 0
  minbound(:) = 0.0D0
  maxbound(:) = 0.0D0
ENDIF
IF (.NOT.dbffileisnull(hshp)) THEN
  dbffieldcount = dbfgetfieldcount(hshp%dbffile_orig)
  dbfrecordcount = dbfgetrecordcount(hshp%dbffile_orig)
ELSE
  dbffieldcount = 0
  dbfrecordcount = 0
ENDIF

END SUBROUTINE shpgetinfo


!> It reads a single shape from a shapefile.
!! This function reads a single shape from a shapefile dataset (only
!! .shp/.shx part) and it returns an object of type \a shpobject
!! containing all the information read. The value of \a ishape should
!! be in the range 0, \a nentites (as returned from \a shpgetinfo).
!! The shape object returned should be destroyed with the \a
!! shpdestroyobject subroutine before being reused, in order to avoid
!! memory leaks.
FUNCTION shpreadobject(hshp, ishape)
TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object to read from
INTEGER :: ishape !< number of shape to be read
TYPE(shpobject),TARGET :: shpreadobject

TYPE(shpobject) :: lshpobject

INTEGER :: ier

IF (.NOT.shpfileisnull(hshp)) THEN
  ier = shpreadobject_int(hshp%shpfile_orig, ishape, C_LOC(shpreadobject))
ELSE ! initialize to empty
  shpreadobject = shpobject_null
ENDIF

END FUNCTION shpreadobject


!> It returns \c .TRUE. if the provided shape object is not valid.
!! It can be called after \a shpreadobject to test whether
!! a valid shape has ben read.
FUNCTION shpisnull(psobject) RESULT(isnull)
TYPE(shpobject),INTENT(in) :: psobject !< shape object to test
LOGICAL :: isnull

isnull = .NOT.c_associated(psobject%shpobject_orig)

END FUNCTION shpisnull


!> It closes all the files associated with the shapefile dataset.
SUBROUTINE shpclose(hshp)
TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object to be closed

IF (.NOT.shpfileisnull(hshp)) THEN
  CALL shpclose_orig(hshp%shpfile_orig)
  hshp%shpfile_orig = c_null_ptr
ENDIF
IF (.NOT.dbffileisnull(hshp)) THEN
  CALL dbfclose(hshp%dbffile_orig)
  hshp%dbffile_orig = c_null_ptr
ENDIF

END SUBROUTINE shpclose


!> It creates a new shape object, simple version.
!! It creates a new shape object and returns it as a variable of type
!! \a shpobject; the object has x,y,z coordinates with no measure and
!! a single part. The successful creation can be checked with the
!! function \a shpisnull.
FUNCTION shpcreatesimpleobject(nshptype, nvertices, padfx, padfy, padfz)
INTEGER :: nshptype !< type of shape, one of the \a shpt_* constants
INTEGER :: nvertices !< number of vertices
REAL(kind=c_double) :: padfx(nvertices) !< x coordinates
REAL(kind=c_double) :: padfy(nvertices) !< y coordinates
REAL(kind=c_double),OPTIONAL :: padfz(nvertices) !< z coordinates, it can be skipped
TYPE(shpobject),TARGET :: shpcreatesimpleobject

TYPE(shpobject) :: lshpobject

IF (shpcreatesimpleobject_int(nshptype, nvertices, padfx, padfy, padfz, &
 C_LOC(shpcreatesimpleobject)) /= 0) THEN
  shpcreatesimpleobject = shpobject_null
ENDIF

END FUNCTION shpcreatesimpleobject


!> It creates a new shape object, full version.
!! It creates a new shape object and returns it as a variable of type
!! \a shpobject; the object has x,y,z coordinates with measure and
!! possibly multiple parts. The successful creation can be checked
!! with the function \a shpisnull.
FUNCTION shpcreateobject(nshptype, ishape, nparts, panpartstart, panparttype, &
 nvertices, padfx, padfy, padfz, padfm)
INTEGER :: nshptype !< type of shape, one of the \a shpt_* constants
INTEGER :: ishape !< shapeid to be recorded with this shape
INTEGER :: nparts !< number of parts
INTEGER :: nvertices !< number of vertices
INTEGER :: panpartstart(nparts) !< start indices of each part
INTEGER :: panparttype(nparts) !< type of each of the parts, this is only meaningful for \a MULTIPATCH files, for all other cases it will be assumed to be \a SHPP_RING 
REAL(kind=c_double) :: padfx(nvertices) !< x coordinates
REAL(kind=c_double) :: padfy(nvertices) !< y coordinates
REAL(kind=c_double),OPTIONAL :: padfz(nvertices) !< z coordinates, it can be skipped
REAL(kind=c_double),OPTIONAL :: padfm(nvertices) !< measure, it can be skipped
TYPE(shpobject),TARGET :: shpcreateobject

TYPE(shpobject) :: lshpobject

IF (shpcreateobject_int(nshptype, ishape, nparts, panpartstart, panparttype, &
 nvertices, padfx, padfy, padfz, padfm, C_LOC(shpcreateobject)) /= 0) THEN
  shpcreateobject = shpobject_null
ENDIF

END FUNCTION shpcreateobject


!> It recomputes the extents of a shape.
!! This subroutine replaces the existing values of the dfXMin, dfYMin,
!! dfZMin, dfMMin, dfXMax, dfYMax, dfZMax, and dfMMax with updated
!! values based on the current set of vertices for the shape. It is
!! automatically called by shpcreateobject, but if the vertices of an
!! existing object are altered it should be called again to fix up the
!! extents.
SUBROUTINE shpcomputeextents(psobject)
TYPE(shpobject),TARGET :: psobject !< shape object to update

CALL shpcomputeextents_int(psobject%shpobject_orig, C_LOC(psobject))

END SUBROUTINE shpcomputeextents


!> It writes a shape object to a file.
!! It returns the number of shape written, starting from 0, or -1 in
!! case of failure.
FUNCTION shpwriteobject(hshp, ishape, psobject)
TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object where to write
INTEGER :: ishape !< number of shape to write in the file, starting from 0, -1 to append to existing shapes
TYPE(shpobject) :: psobject !< shape object to be written
INTEGER :: shpwriteobject

IF (.NOT.shpfileisnull(hshp)) THEN
  shpwriteobject = shpwriteobject_orig(hshp%shpfile_orig, ishape, psobject%shpobject_orig)
ELSE
  shpwriteobject = 0
ENDIF

END FUNCTION shpwriteobject


!> It destroys a shape object for subsequent reuse or when not used anymore.
SUBROUTINE shpdestroyobject(psobject)
TYPE(shpobject) :: psobject !< shape object to be destroyed

IF (c_associated(psobject%shpobject_orig)) THEN
  CALL shpdestroyobject_orig(psobject%shpobject_orig)
ENDIF
psobject = shpobject_null

END SUBROUTINE shpdestroyobject


#ifndef SHAPELIB_PRE10
!> It sets the correct ring order.
!! This function will reverse any ring necessary in order to enforce
!! the shapefile restrictions on the required order of inner and outer
!! rings in the Shapefile specification. It returns TRUE if a change
!! is made and FALSE if no change is made. Only polygon objects will
!! be affected though any object may be passed.
!! This procedure is available only with shapelib version 1.2.10 or
!! later.
FUNCTION shprewindobject(hshp, psobject)
TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object (not used)
TYPE(shpobject),INTENT(inout),TARGET :: psobject !< shape object to be rewound
LOGICAL :: shprewindobject

shprewindobject = shprewindobject_int(hshp%shpfile_orig, psobject%shpobject_orig, &
 C_LOC(psobject)) /= 0

END FUNCTION shprewindobject


!> It returns the index of the field matching the name.
!! The comparison is case insensitive, however lengths must match
!! exactly. It returns -1 if the field is not found or if the shape
!! object is not valid.
!! This procedure is available only with shapelib version 1.2.10 or
!! later.
FUNCTION dbfgetfieldindex(hshp, pszfieldname)
TYPE(shpfileobject),INTENT(in) :: hshp !< shape object to query
CHARACTER(len=*),INTENT(in) :: pszfieldname !< field name to search
INTEGER :: dbfgetfieldindex

IF (.NOT.dbffileisnull(hshp)) THEN
  dbfgetfieldindex = dbfgetfieldindex_orig(hshp%dbffile_orig, fchartrimtostr(pszfieldname))
ELSE
  dbfgetfieldindex = -1
ENDIF

END FUNCTION dbfgetfieldindex
#endif


!> It returns information about a dbf field.
!! The return value is the type of the requested field, which is one
!! of the \a ft* constants. The field type returned does not
!! correspond one to one with the xBase field types. For instance the
!! xBase field type for Date will just be returned as being
!! ftinteger. It returns -1 if the shape object is not valid.
FUNCTION dbfgetfieldinfo(hshp, ifield, pszfieldname, pnwidth, pndecimals)
TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object to query
INTEGER,INTENT(in) :: ifield !< number of field to query, in the interval 0, nfield - 1
CHARACTER(len=*),INTENT(out) :: pszfieldname !< the name of the field, it can be up to 11 characters long
INTEGER,INTENT(out) :: pnwidth !< the width of the field in characters
INTEGER,INTENT(out) :: pndecimals !< the number of decimals in a floating point representation, nonzero only for fields of type \a ftdouble
INTEGER :: dbfgetfieldinfo

CHARACTER(len=11) :: lpszfieldname

IF (.NOT.dbffileisnull(hshp)) THEN
  dbfgetfieldinfo = dbfgetfieldinfo_orig(hshp%dbffile_orig, ifield, &
   lpszfieldname, pnwidth, pndecimals)
  pszfieldname = lpszfieldname ! must strip null here!
ELSE
  dbfgetfieldinfo = -1
ENDIF

END FUNCTION dbfgetfieldinfo


!> It adds a new field to an existing dataset.
!! Note that fields can only be added to datasets with no dbf records,
!! though this is limitation of this API, not of the file format. This
!! function returns the number of the new field, starting from 0, or
!! -1 in case of error.
FUNCTION dbfaddfield(hshp, pszfieldname, etype, nwidth, ndecimals)
TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object to update
CHARACTER(len=*),INTENT(in) :: pszfieldname !< the name of the new field, at most 11 characters will be used
INTEGER,INTENT(in) :: etype !< the type of the new field, one of the \a ft* constants
INTEGER,INTENT(in) :: nwidth !< the width of the field to be created in characters
INTEGER,INTENT(in) :: ndecimals !< the number of decimals in a floating point representation for fields of type \a ftdouble, for the other types it should be 0
INTEGER :: dbfaddfield

IF (.NOT.dbffileisnull(hshp)) THEN
  dbfaddfield = dbfaddfield_orig(hshp%dbffile_orig, fchartrimtostr(pszfieldname), &
   etype, nwidth, ndecimals)
ELSE
  dbfaddfield = -1
ENDIF

END FUNCTION dbfaddfield


SUBROUTINE dbfreadintegerattribute_f(hshp, ishape, ifield, attr)
TYPE(shpfileobject),INTENT(inout) :: hshp
INTEGER,INTENT(in) :: ishape, ifield
INTEGER,INTENT(out) :: attr

IF (.NOT.dbffileisnull(hshp)) THEN
  attr = dbfreadintegerattribute_orig(hshp%dbffile_orig, ishape, ifield)
ELSE
  attr = 0
ENDIF

END SUBROUTINE dbfreadintegerattribute_f


SUBROUTINE dbfreaddoubleattribute_f(hshp, ishape, ifield, attr)
TYPE(shpfileobject),INTENT(inout) :: hshp
INTEGER,INTENT(in) :: ishape, ifield
REAL(kind=c_double),INTENT(out) :: attr

IF (.NOT.dbffileisnull(hshp)) THEN
  attr = dbfreaddoubleattribute_orig(hshp%dbffile_orig, ishape, ifield)
ELSE
  attr = 0.0_c_double
ENDIF

END SUBROUTINE dbfreaddoubleattribute_f


SUBROUTINE dbfreadstringattribute_f(hshp, ishape, ifield, attr)
TYPE(shpfileobject),INTENT(inout) :: hshp
INTEGER,INTENT(in) :: ishape, ifield
CHARACTER(len=*),INTENT(out) :: attr

IF (.NOT.dbffileisnull(hshp)) THEN
  attr = strtofchar(dbfreadstringattribute_orig(hshp%dbffile_orig, ishape, ifield), LEN(attr))
ELSE
  attr = ''
ENDIF

END SUBROUTINE dbfreadstringattribute_f


#ifndef SHAPELIB_PRE10
!> It returns \c .TRUE. if the requested attribute is NULL valued.
!! Note that NULL fields are represented in the .dbf file as having
!! all spaces in the field. Reading NULL fields will result in a value
!! of 0.0 or an empty string with the dbfreadattribute interface.
!! It returns \c .TRUE. in case of error as well.
!! This procedure is available only with shapelib version 1.2.10 or
!! later.
FUNCTION dbfisattributenull(hshp, ishape, ifield)
TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object to query
INTEGER,INTENT(in) :: ishape !< number of shape (record) to query
INTEGER,INTENT(in) :: ifield !< number of field to query
LOGICAL :: dbfisattributenull

IF (.NOT.dbffileisnull(hshp)) THEN
  dbfisattributenull = dbfisattributenull_orig(hshp%dbffile_orig, ishape, ifield) /= 0
ELSE ! force to null
  dbfisattributenull = .FALSE.
ENDIF

END FUNCTION dbfisattributenull
#endif


FUNCTION dbfwriteintegerattribute_f(hshp, ishape, ifield, attr) RESULT(dbfwriteattribute)
TYPE(shpfileobject),INTENT(inout) :: hshp
INTEGER,INTENT(in) :: ishape, ifield
INTEGER,INTENT(in) :: attr
INTEGER :: dbfwriteattribute

IF (.NOT.dbffileisnull(hshp)) THEN
  dbfwriteattribute = dbfwriteintegerattribute(hshp%dbffile_orig, ishape, ifield, attr)
ELSE
  dbfwriteattribute = 0
ENDIF

END FUNCTION dbfwriteintegerattribute_f


FUNCTION dbfwritedoubleattribute_f(hshp, ishape, ifield, attr) RESULT(dbfwriteattribute)
TYPE(shpfileobject),INTENT(inout) :: hshp
INTEGER,INTENT(in) :: ishape, ifield
REAL(kind=c_double),INTENT(in) :: attr
INTEGER :: dbfwriteattribute

IF (.NOT.dbffileisnull(hshp)) THEN
  dbfwriteattribute = dbfwritedoubleattribute(hshp%dbffile_orig, ishape, ifield, attr)
ELSE
  dbfwriteattribute = 0
ENDIF

END FUNCTION dbfwritedoubleattribute_f


FUNCTION dbfwritestringattribute_f(hshp, ishape, ifield, attr) RESULT(dbfwriteattribute)
TYPE(shpfileobject),INTENT(inout) :: hshp
INTEGER,INTENT(in) :: ishape, ifield
CHARACTER(len=*),INTENT(in) :: attr
INTEGER :: dbfwriteattribute

IF (.NOT.dbffileisnull(hshp)) THEN
  dbfwriteattribute = dbfwritestringattribute(hshp%dbffile_orig, ishape, ifield, fchartostr(attr))
ELSE
  dbfwriteattribute = 0
ENDIF

END FUNCTION dbfwritestringattribute_f


FUNCTION dbfwritenullattribute_f(hshp, ishape, ifield) RESULT(dbfwriteattribute)
TYPE(shpfileobject),INTENT(inout) :: hshp
INTEGER,INTENT(in) :: ishape, ifield
INTEGER :: dbfwriteattribute

IF (.NOT.dbffileisnull(hshp)) THEN
  dbfwriteattribute = dbfwritenullattribute(hshp%dbffile_orig, ishape, ifield)
ELSE
  dbfwriteattribute = 0
ENDIF

END FUNCTION dbfwritenullattribute_f


#ifndef SHAPELIB_PRE10
!> It returns the dbf type code of the requested field.
!! The return value is a single character and it can assume the
!! following values:
!!
!! - 'C' (String)
!! - 'D' (Date)
!! - 'F' (Float)
!! - 'N' (Numeric, with or without decimal)
!! - 'L' (Logical)
!! - 'M' (Memo: 10 digits .DBT block ptr)
!! - ' ' (field out of range or other error)
!!
!! This procedure is available only with shapelib version 1.2.10 or
!! later.
FUNCTION dbfgetnativefieldtype(hshp, ifield)
TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object to query
INTEGER,INTENT(in) :: ifield !< number of field to query
CHARACTER(len=1) :: dbfgetnativefieldtype

IF (.NOT.dbffileisnull(hshp)) THEN
  dbfgetnativefieldtype = CHAR(dbfgetnativefieldtype_orig(hshp%dbffile_orig, ifield))
ELSE ! force to null
  dbfgetnativefieldtype = ' '
ENDIF

END FUNCTION dbfgetnativefieldtype
#endif


SUBROUTINE shpsetobjectfortran(ftnobject, cobject, nshptype, nshapeid, &
 nparts, panpartstart, panparttype, &
 nvertices, padfx, padfy, padfz, padfm, &
 dfxmin, dfymin, dfzmin, dfmmin, dfxmax, dfymax, dfzmax, dfmmax) &
 BIND(C,name='SHPSetObjectFortran')
TYPE(c_ptr),VALUE :: ftnobject
TYPE(c_ptr),VALUE :: cobject
INTEGER(kind=c_int) :: nshptype ! Shape Type (SHPT_* - see list above)
INTEGER(kind=c_int) :: nshapeid ! Shape Number (-1 is unknown/unassigned)
INTEGER(kind=c_int) :: nparts ! # of Parts (0 implies single part with no info)
INTEGER(kind=c_int),TARGET :: panpartstart(nparts), & ! Start Vertex of part
 panparttype(nparts) ! Part Type (SHPP_RING if not SHPT_MULTIPATCH)
INTEGER(kind=c_int) :: nvertices ! Vertex list 
REAL(kind=c_double),TARGET ::  padfx(nvertices), padfy(nvertices), &
 padfz(nvertices), padfm(nvertices) ! (all zero if not provided)
REAL(kind=c_double) :: & ! Bounds in X, Y, Z and M dimensions
 dfxmin, dfymin, dfzmin, dfmmin, dfxmax, dfymax, dfzmax, dfmmax

TYPE(shpobject),POINTER :: obj

CALL C_F_POINTER(ftnobject, obj)

obj%shpobject_orig = cobject
obj%nshptype = nshptype
obj%nshapeid = nshapeid
obj%nparts = nparts
obj%panpartstart => panpartstart
obj%panparttype => panparttype
obj%nvertices = nvertices
obj%padfx => padfx
obj%padfy => padfy
obj%padfz => padfz
obj%padfm => padfm
obj%dfxmin = dfxmin
obj%dfymin = dfymin
obj%dfzmin = dfzmin
obj%dfmmin = dfmmin
obj%dfxmax = dfxmax
obj%dfymax = dfymax
obj%dfzmax = dfzmax
obj%dfmmax = dfmmax

END SUBROUTINE shpsetobjectfortran

END MODULE shapelib

