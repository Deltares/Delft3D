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

!> Fortran 2003 interface to the readosm
!! https://www.gaia-gis.it/fossil/readosm/index library.
!! ReadOSM is an open source library which is able to extract valid
!! data from within an Open Street Map input file. It can read files
!! in the .osm and .osm.pbf formats.  ReadOSM is developed and
!! maintained by Alessandro Furieri.  This module defines an API which
!! reflects the original readosm C API, plus some additional objects
!! and methods to simplify the use from Fortran.
!!
!! The reading of the file is callback-based, i.e. a user-defined
!! function is called whenever a node, way or relation is encounterd
!! in the file; the callback functions receive a copy of the entity
!! read from the file in the form of a C-interoperable derived type
!! which matches exactly the structures defined in the original
!! readosm C library. The derived types can be converted on the fly to
!! a more Fortran-friendly version, where pointers are replaced with
!! Fortran arrays where possible. It is up to the callbacks to do
!! something useful with the data received.
!!
!! For an example of application of the \a readosm module, please
!! refer to the following test program, which parses an osm file and
!! dumps some information about it:
!! \include readosm_test.F90
!!
!! \ingroup libfortrangis
MODULE readosm
USE,INTRINSIC :: ISO_C_BINDING
USE fortranc
IMPLICIT NONE


INTEGER,PARAMETER :: READOSM_UNDEFINED = -1234567890 !< information is not available
INTEGER,PARAMETER :: READOSM_MEMBER_NODE = 7361 !< MemberType: NODE
INTEGER,PARAMETER :: READOSM_MEMBER_WAY = 6731 !< MemberType: WAY
INTEGER,PARAMETER :: READOSM_MEMBER_RELATION = 3671 !< MemberType: RELATION
INTEGER,PARAMETER :: READOSM_OK = 0 !< No error, success
INTEGER,PARAMETER :: READOSM_INVALID_SUFFIX = -1 !< not .osm or .pbf suffix
INTEGER,PARAMETER :: READOSM_FILE_NOT_FOUND = -2 !< .osm or .pbf file does not exist or is not accessible for reading
INTEGER,PARAMETER :: READOSM_NULL_HANDLE = -3 !< Null OSM_handle argument
INTEGER,PARAMETER :: READOSM_INVALID_HANDLE = -4 !< Invalid OSM_handle argument
INTEGER,PARAMETER :: READOSM_INSUFFICIENT_MEMORY = -5 !< some kind of memory allocation  failure
INTEGER,PARAMETER :: READOSM_CREATE_XML_PARSER_ERROR = -6 !< cannot create the XML Parser
INTEGER,PARAMETER :: READOSM_READ_ERROR = -7 !< read error
INTEGER,PARAMETER :: READOSM_XML_ERROR = -8 !< XML parser error
INTEGER,PARAMETER :: READOSM_INVALID_PBF_HEADER = -9 !< invalid PBF header
INTEGER,PARAMETER :: READOSM_UNZIP_ERROR = -10 !< unZip error
INTEGER,PARAMETER :: READOSM_ABORT = -11 !< user-required parser abort

!> Object describing a TAG structure.
!! A derived type representing a <b>key:value</b> pair.
TYPE,BIND(C) :: readosm_tag
  TYPE(c_ptr) :: key !< the KEY
  TYPE(c_ptr) :: value !< the VALUE
END TYPE readosm_tag

!> A more Fortran-friendly object describing a TAG structure.
TYPE :: readosm_tag_f
  CHARACTER(kind=c_char,len=1),ALLOCATABLE :: key(:) !< the KEY
  CHARACTER(kind=c_char,len=1),ALLOCATABLE :: value(:) !< the VALUE
END TYPE readosm_tag_f


!> Object describing a NODE structure.
!! A derived type representing a NODE object, wrapping a complex
!! XML fragment like the following:
!! \verbatim
!! <node id="12345" lat="6.66666" lon="7.77777" version="1" changeset="54321" user="some-user" uid="66" timestamp="2005-02-28T17:45:15Z">
!!        <tag key="created_by" value="JOSM" />
!!        <tag key="tourism" value="camp_site" />
!! </node>
!! \endverbatim
TYPE,BIND(C) :: readosm_node
  INTEGER(kind=c_long_long) :: id !< NODE-ID (expected to be a unique value)
  REAL(kind=c_double) :: latitude !< geographic latitude
  REAL(kind=c_double) :: longitude !< geographic longitude
  INTEGER(kind=c_int) :: version !< object version
  INTEGER(kind=c_long_long) :: changeset !< ChangeSet ID
  TYPE(c_ptr) :: user !< name of the User defining this NODE
  INTEGER(kind=c_int) :: uid !< corresponding numeric UserID
  TYPE(c_ptr) :: timestamp !< when this NODE was defined
  INTEGER(kind=c_int) :: tag_count !< number of associated TAGs (may be zero)
  TYPE(c_ptr) :: tags !< array of TAG objects (may be NULL)
END TYPE readosm_node

!> A more Fortran-friendly object describing a NODE structure.
TYPE :: readosm_node_f
  INTEGER(kind=c_long_long) :: id=0 !< NODE-ID (expected to be a unique value)
  REAL(kind=c_double) :: latitude !< geographic latitude
  REAL(kind=c_double) :: longitude !< geographic longitude
  INTEGER(kind=c_int) :: version !< object version
  INTEGER(kind=c_long_long) :: changeset !< ChangeSet ID
!  TYPE(c_ptr) :: user !< name of the User defining this NODE
  INTEGER(kind=c_int) :: uid !< corresponding numeric UserID
!  TYPE(c_ptr) :: timestamp !< when this NODE was defined
  TYPE(readosm_tag_f),ALLOCATABLE :: tags(:) !< array of TAG objects (may be NULL)
END TYPE readosm_node_f


!> Object describing a WAY structure.
!! A derived type representing a WAY object, wrapping a complex
!! XML fragment like the following:
!! \verbatim
!! <way id="12345" version="1" changeset="54321" user="some-user" uid="66" timestamp="2005-02-28T17:45:15Z">
!!        <nd ref="12345" />
!!        <nd ref="12346" />
!!        <nd ref="12347" />
!!        <tag key="created_by" value="JOSM" />
!!        <tag key="tourism" value="camp_site" />
!! </way>
!! \endverbatim
TYPE,BIND(C) :: readosm_way
  INTEGER(kind=c_long_long) :: id !< WAY-ID (expected to be a unique value)
  INTEGER(kind=c_int) :: version; !< object version
  INTEGER(kind=c_long_long) :: changeset !< ChangeSet ID
  TYPE(c_ptr) :: user !< name of the User defining this WAY
  INTEGER(kind=c_int) :: uid !< corresponding numeric UserID
  TYPE(c_ptr) :: timestamp; !< when this WAY was defined
  INTEGER(kind=c_int) :: node_ref_count !< number of referenced NODE-IDs (may be zero)
  TYPE(c_ptr) :: node_refs !< array of NODE-IDs (may be NULL)
  INTEGER(kind=c_int) :: tag_count !< number of associated TAGs (may be zero)
  TYPE(c_ptr) :: tags !< array of TAG objects (may be NULL)
END TYPE readosm_way

!> A more Fortran-friendly object describing a WAY structure.
TYPE :: readosm_way_f
  INTEGER(kind=c_long_long) :: id=0 !< WAY-ID (expected to be a unique value)
  INTEGER(kind=c_int) :: version; !< object version
  INTEGER(kind=c_long_long) :: changeset; !< ChangeSet ID
!  TYPE(c_ptr) :: user !< name of the User defining this WAY
  INTEGER(kind=c_int) :: uid !< corresponding numeric UserID
!  TYPE(c_ptr) :: timestamp; !< when this WAY was defined
  INTEGER(kind=c_long_long),ALLOCATABLE :: node_refs(:) !< array of NODE-IDs (may be NULL)
  TYPE(readosm_tag_f),ALLOCATABLE :: tags(:) !< array of TAG objects (may be NULL)
END TYPE readosm_way_f


!> Object describing a RELATION-MEMBER structure.
!! A derived type representing a RELATION-MEMBER object, and wrapping
!! an XML fragment like the following:
!! \verbatim
!! <member type="some-type" ref="12345" role="some-role" />
!! \endverbatim
TYPE,BIND(C) :: readosm_member
  INTEGER(kind=c_int) :: member_type !< can be one of: READOSM_MEMBER_NODE, READOSM_MEMBER_WAY or READOSM_MEMBER_RELATION
  INTEGER(kind=c_long_long) :: id !< ID-value identifying the referenced object
  TYPE(c_ptr) :: role !< intended role for this reference
END TYPE readosm_member

!> A more Fortran-friendly object describing a RELATION-MEMEBER structure.
TYPE :: readosm_member_f
  INTEGER(kind=c_int) :: member_type=READOSM_UNDEFINED !< can be one of: READOSM_MEMBER_NODE, READOSM_MEMBER_WAY or READOSM_MEMBER_RELATION
  INTEGER(kind=c_long_long) :: id !< ID-value identifying the referenced object
  CHARACTER(kind=c_char,len=1),ALLOCATABLE :: role(:) !< intended role for this reference
END TYPE readosm_member_f


!> Object describing a RELATION structure.
!! A derived type representing a RELATION object, and wrapping a
!! complex XML fragment like the following:
!! \verbatim
!! <relation id="12345" version="1" changeset="54321" user="some-user" uid="66" timestamp="2005-02-28T17:45:15Z">
!!         <member type="way" ref="12345" role="outer" />
!!         <member type="way" ref="12346" role="inner" />
!!         <tag key="created_by" value="JOSM" />
!!         <tag key="tourism" value="camp_site" />
!! </relation>
!! \endverbatim
TYPE,BIND(C) :: readosm_relation
  INTEGER(kind=c_long_long) :: id !< RELATION-ID (expected to be a unique value)
  INTEGER(kind=c_int) :: version; !< object version
  INTEGER(kind=c_long_long) :: changeset; !< ChangeSet ID
  TYPE(c_ptr) :: user !< name of the User defining this RELATION
  INTEGER(kind=c_int) :: uid !< corresponding numeric UserID
  TYPE(c_ptr) :: timestamp; !< when this RELATION was defined
  INTEGER(kind=c_int) :: member_count !< number of associated MEMBERs (may be zero)
  TYPE(c_ptr) :: members !< array of MEMBER objects (may be NULL)
  INTEGER(kind=c_int) :: tag_count; !< number of associated TAGs (may be zero)
  TYPE(c_ptr) :: tags !< array of TAG objects (may be NULL)
END TYPE readosm_relation

!> A more Fortran-friendly object describing a RELATION structure.
TYPE :: readosm_relation_f
  INTEGER(kind=c_long_long) :: id !< RELATION-ID (expected to be a unique value)
  INTEGER(kind=c_int) :: version; !< object version
  INTEGER(kind=c_long_long) :: changeset; !< ChangeSet ID
!  TYPE(c_ptr) :: user !< name of the User defining this RELATION
  INTEGER(kind=c_int) :: uid !< corresponding numeric UserID
!  TYPE(c_ptr) :: timestamp; !< when this RELATION was defined
  TYPE(readosm_member_f),ALLOCATABLE :: members(:) !< array of MEMBER objects (may be NULL)
  TYPE(readosm_tag_f),ALLOCATABLE :: tags(:) !< array of TAG objects (may be NULL)
END TYPE readosm_relation_f

! define dynamically extensible arrays of the _f types, first part
#undef ARRAYOF_ORIGEQ

#undef ARRAYOF_ORIGTYPE
#undef ARRAYOF_TYPE
#define ARRAYOF_ORIGTYPE TYPE(readosm_node_f)
#define ARRAYOF_TYPE arrayof_readosm_node_f
#include "arrayof_pre.F90"

#undef ARRAYOF_ORIGTYPE
#undef ARRAYOF_TYPE
#define ARRAYOF_ORIGTYPE TYPE(readosm_way_f)
#define ARRAYOF_TYPE arrayof_readosm_way_f
#include "arrayof_pre.F90"

#undef ARRAYOF_ORIGTYPE
#undef ARRAYOF_TYPE
#define ARRAYOF_ORIGTYPE TYPE(readosm_relation_f)
#define ARRAYOF_TYPE arrayof_readosm_relation_f
#include "arrayof_pre.F90"


!> Derived type for performing a prepackaged full parsing of an osm file.
!! An object of this type can contain all the information from an osm
!! file and it can be used in association with the simpified \a
!! readosm_parse_full_f method for importing the full information from
!! a file in a DOM (Document Object Model) style.
TYPE readosm_full_f
  TYPE(arrayof_readosm_node_f) :: nodes !< array containing all the nodes
  TYPE(arrayof_readosm_way_f) :: ways !< array containing all the ways
  TYPE(arrayof_readosm_relation_f) :: relations !< array containing all the relations
END TYPE readosm_full_f


!> Open the .osm or .pbf file, preparing for future functions.
!! \return READOSM_OK will be returned on success, otherwise any
!! appropriate error code on failure.
!! \note You are expected to readosm_close() even on failure, so as to
!! correctly release any dynamic memory allocation.
INTERFACE
  FUNCTION readosm_open(path, osm_handle) BIND(C,name='readosm_open')
  IMPORT
  CHARACTER(kind=c_char),INTENT(in) :: path(*) !< full or relative pathname of the input file
  TYPE(c_ptr),INTENT(out) :: osm_handle !< an opaque reference (handle) to be used in each subsequent function
  INTEGER(kind=c_int) :: readosm_open
  END FUNCTION readosm_open
END INTERFACE


!> Close the .osm or .pbf file and release any allocated resource.
!! \return READOSM_OK will be returned on success, otherwise any
!! appropriate error code on failure.
!! \note After calling readosm_close() any related resource will be
!! released, and the handle will no longer be valid.
INTERFACE
  FUNCTION readosm_close(osm_handle) BIND(C,name='readosm_close')
  IMPORT
  TYPE(c_ptr),VALUE :: osm_handle !< the handle previously returned by readosm_open()
  INTEGER(kind=c_int) :: readosm_close
  END FUNCTION readosm_close
END INTERFACE


!> Parse the corresponding file calling the selected callbacks for
!! every entity encountered.
!! \return READOSM_OK will be returned on success, otherwise any
!! appropriate error code on failure.
INTERFACE readosm_parse
!> Parse the corresponding file calling the selected callbacks for
!! every entity encountered.
!! This is the original C interface, where callback functions are
!! interoperable C function pointers, \a c_null_funptr can be passed
!! in order to disable the corresponding callback.
!! \return READOSM_OK will be returned on success, otherwise any
!! appropriate error code on failure.
  FUNCTION readosm_parse(osm_handle, user_data, node_fnct, way_fnct, &
   relation_fnct) BIND(C,name='readosm_parse')
  IMPORT
  TYPE(c_ptr),VALUE :: osm_handle !< the handle previously returned by readosm_open()
  TYPE(c_ptr),VALUE :: user_data !< user_data pointer to some user-supplied data struct
  TYPE(c_funptr),VALUE :: node_fnct !< pointer to callback function intended to consume NODE objects (may be NULL if processing NODEs is not an interesting option)
  TYPE(c_funptr),VALUE :: way_fnct !< pointer to callback function intended to consume WAY objects (may be NULL if processing WAYs is not an interesting option)
  TYPE(c_funptr),VALUE :: relation_fnct !< pointer to callback function intended to consume RELATION objects (may be NULL if processing RELATIONs is not an interesting option)
  INTEGER(kind=c_int) :: readosm_parse
  END FUNCTION readosm_parse

  MODULE PROCEDURE readosm_parse_f
END INTERFACE readosm_parse


INTERFACE readosm_object_f
  MODULE PROCEDURE readosm_object_f_node, readosm_object_f_way, &
   readosm_object_f_relation
END INTERFACE readosm_object_f


INTERFACE readosm_parse
END INTERFACE readosm_parse

PRIVATE readosm_object_f_node, readosm_object_f_way, &
 readosm_object_f_relation

CONTAINS

!> Parse the corresponding file calling the selected callbacks for
!! every entity encountered.
!! This is the Fortran-friendly interface where callback functions are
!! optional arguments (thus the keyword form is preferred if any of
!! them is missing) and represent interfaced Fortran FUNCTIONS.
!! \return READOSM_OK will be returned on success, otherwise any
!! appropriate error code on failure.
FUNCTION readosm_parse_f(osm_handle, user_data, node_fnct, way_fnct, &
 relation_fnct)
TYPE(c_ptr),VALUE :: osm_handle !< the handle previously returned by readosm_open()
TYPE(c_ptr),VALUE :: user_data !< user_data pointer to some user-supplied data struct
INTERFACE !< callback function intended to consume NODE objects (may be NULL if processing NODEs is not an interesting option)
  FUNCTION node_fnct(user_data, node) BIND(C)
  IMPORT
  TYPE(c_ptr),VALUE :: user_data
  TYPE(readosm_node) :: node
  INTEGER(kind=c_int) :: node_fnct
  END FUNCTION node_fnct
END INTERFACE

!> callback function intended to consume WAY objects (may be NULL if processing WAYs is not an interesting option)
INTERFACE
  FUNCTION way_fnct(user_data, way) BIND(C)
  IMPORT
  TYPE(c_ptr),VALUE :: user_data
  TYPE(readosm_way) :: way
  INTEGER(kind=c_int) :: way_fnct
  END FUNCTION way_fnct
END INTERFACE

!> callback function intended to consume RELATION objects (may be NULL if processing RELATIONs is not an interesting option)
INTERFACE
  FUNCTION relation_fnct(user_data, relation) BIND(C)
  IMPORT
  TYPE(c_ptr),VALUE :: user_data
  TYPE(readosm_relation) :: relation
  INTEGER(kind=c_int) :: relation_fnct
  END FUNCTION relation_fnct
END INTERFACE

OPTIONAL :: node_fnct
OPTIONAL :: way_fnct
OPTIONAL :: relation_fnct !< callback function intended to consume RELATION objects (may be NULL if processing RELATIONs is not an interesting option)
INTEGER(kind=c_int) :: readosm_parse_f

TYPE(c_funptr) :: nf, wf, rf

IF (present(node_fnct)) THEN
  nf = C_FUNLOC(node_fnct)
ELSE
  nf = c_null_funptr
ENDIF
IF (present(way_fnct)) THEN
  wf = C_FUNLOC(way_fnct)
ELSE
  wf = c_null_funptr
ENDIF
IF (present(relation_fnct)) THEN
  rf = C_FUNLOC(relation_fnct)
ELSE
  rf = c_null_funptr
ENDIF

readosm_parse_f = readosm_parse(osm_handle, user_data, nf, wf, rf)

END FUNCTION readosm_parse_f


! private function for "fortranizing" tags array, it has been
! temporarily converted to a subroutine because of (de)allocations
! problems with gfortran 4.6.3
SUBROUTINE readosm_object_f_tags(tags, tag_count, f_type) ! RESULT(f_type)
TYPE(c_ptr) :: tags ! array of TAG objects (may be NULL)
INTEGER(kind=c_int) :: tag_count; ! number of associated TAGs (may be zero)

TYPE(readosm_tag_f),INTENT(out),ALLOCATABLE :: f_type(:)

TYPE(readosm_tag),POINTER :: tmptags(:)
INTEGER :: i

IF (tag_count > 0 .AND. C_ASSOCIATED(tags)) THEN
  CALL C_F_POINTER(tags, tmptags, (/tag_count/))
  ALLOCATE(f_type(tag_count))
  DO i = 1, tag_count
    f_type(i)%key = tmptags(i)%key
    f_type(i)%value = tmptags(i)%value
  ENDDO
ELSE
  ALLOCATE(f_type(0))
ENDIF

END SUBROUTINE readosm_object_f_tags


! private function for "fortranizing" members array, it has been
! temporarily converted to a subroutine because of (de)allocations
! problems with gfortran 4.6.3
SUBROUTINE readosm_object_f_members(members, member_count, f_type) ! RESULT(f_type)
TYPE(c_ptr) :: members ! array of MEMBER objects (may be NULL)
INTEGER(kind=c_int) :: member_count; ! number of associated MEMBERs (may be zero)

TYPE(readosm_member_f),INTENT(out),ALLOCATABLE :: f_type(:)

TYPE(readosm_member),POINTER :: tmpmembers(:)
INTEGER :: i

IF (member_count > 0 .AND. C_ASSOCIATED(members)) THEN
  CALL C_F_POINTER(members, tmpmembers, (/member_count/))
  ALLOCATE(f_type(member_count))
  DO i = 1, member_count
    f_type(i)%member_type = tmpmembers(i)%member_type
    f_type(i)%id = tmpmembers(i)%id
    f_type(i)%role = tmpmembers(i)%role
  ENDDO
ELSE
  ALLOCATE(f_type(0))
ENDIF

END SUBROUTINE readosm_object_f_members


FUNCTION readosm_object_f_node(c_type) RESULT(f_type)
TYPE(readosm_node),INTENT(in) :: c_type

TYPE(readosm_node_f) :: f_type

f_type%id = c_type%id
f_type%latitude = c_type%latitude
f_type%longitude = c_type%longitude
f_type%version = c_type%version
f_type%changeset = c_type%changeset
!f_type%user = c_type%user
f_type%uid = c_type%uid
!f_type%timestamp = c_type%timestamp
CALL readosm_object_f_tags(c_type%tags, c_type%tag_count, f_type%tags)
!f_type%tags = readosm_object_f_tags(c_type%tags, c_type%tag_count)

END FUNCTION readosm_object_f_node


FUNCTION readosm_object_f_way(c_type) RESULT(f_type)
TYPE(readosm_way),INTENT(in) :: c_type

TYPE(readosm_way_f) :: f_type

INTEGER(kind=c_long_long),POINTER :: node_refs(:)

f_type%id = c_type%id
f_type%version = c_type%version
f_type%changeset = c_type%changeset
!f_type%user = c_type%user
f_type%uid = c_type%uid
!f_type%timestamp = c_type%timestamp
IF (c_type%node_ref_count > 0 .AND. C_ASSOCIATED(c_type%node_refs)) THEN
  CALL C_F_POINTER(c_type%node_refs, node_refs, (/c_type%node_ref_count/))
  f_type%node_refs = node_refs
ELSE
  ALLOCATE(f_type%node_refs(0))
ENDIF
CALL readosm_object_f_tags(c_type%tags, c_type%tag_count, f_type%tags)
!f_type%tags = readosm_object_f_tags(c_type%tags, c_type%tag_count)

END FUNCTION readosm_object_f_way


FUNCTION readosm_object_f_relation(c_type) RESULT(f_type)
TYPE(readosm_relation),INTENT(in) :: c_type

TYPE(readosm_relation_f) :: f_type

f_type%id = c_type%id
f_type%version = c_type%version
f_type%changeset = c_type%changeset
!f_type%user = c_type%user
f_type%uid = c_type%uid
!f_type%timestamp = c_type%timestamp
CALL readosm_object_f_members(c_type%members, c_type%member_count, f_type%members)
CALL readosm_object_f_tags(c_type%tags, c_type%tag_count, f_type%tags)
!f_type%tags = readosm_object_f_tags(c_type%tags, c_type%tag_count)

END FUNCTION readosm_object_f_relation


! define dynamically extendible arrays of the _f types, second part
#undef ARRAYOF_ORIGEQ

#undef ARRAYOF_ORIGTYPE
#undef ARRAYOF_TYPE
#define ARRAYOF_ORIGTYPE TYPE(readosm_node_f)
#define ARRAYOF_TYPE arrayof_readosm_node_f
#include "arrayof_post.F90"

#undef ARRAYOF_ORIGTYPE
#undef ARRAYOF_TYPE
#define ARRAYOF_ORIGTYPE TYPE(readosm_way_f)
#define ARRAYOF_TYPE arrayof_readosm_way_f
#include "arrayof_post.F90"

#undef ARRAYOF_ORIGTYPE
#undef ARRAYOF_TYPE
#define ARRAYOF_ORIGTYPE TYPE(readosm_relation_f)
#define ARRAYOF_TYPE arrayof_readosm_relation_f
#include "arrayof_post.F90"


!> Simplified parsing method for quickly retrieving all the
!! information from a osm file.  This method can be called in place of
!! \a readosm_parse, thus avoiding the need to define own
!! callbacks. After successful execution, the \a fulldata argument
!! will contain the full information from the file in a DOM (Document
!! Object Model) style. It must be used with care on big datasets
!! since it can consume a lot of memory. It uses the \a
!! readosm_full_node, \a readosm_full_way and \a readosm_full_relation
!! predefined callbacks.
FUNCTION readosm_parse_full_f(osm_handle, fulldata)
TYPE(c_ptr),VALUE :: osm_handle !< the handle previously returned by readosm_open()
TYPE(readosm_full_f),INTENT(inout),TARGET :: fulldata !< an object which, on exit, will contain the full information read from the file
INTEGER :: readosm_parse_full_f

! parse using the predefined callbacks
readosm_parse_full_f = readosm_parse(osm_handle, C_LOC(fulldata), &
 readosm_full_node, readosm_full_way, readosm_full_relation)

CALL packarray(fulldata%nodes)
CALL packarray(fulldata%ways)
CALL packarray(fulldata%relations)

END FUNCTION readosm_parse_full_f


!> Predefined callback for parsing nodes.
!! It receives an object of the type \a readosm_full_f as \a user_data
!! and adds to it the node entity received.
FUNCTION readosm_full_node(user_data, node) BIND(C)
TYPE(c_ptr),VALUE :: user_data
TYPE(readosm_node) :: node
INTEGER(kind=c_int) :: readosm_full_node

TYPE(readosm_full_f),POINTER :: fulldata

! cast user_data to the desired fortran object and insert the entity
! in "Fortran-friendly" format
CALL C_F_POINTER(user_data, fulldata)
CALL insert(fulldata%nodes, readosm_object_f(node))
! set the return code to OK, otherwise parsing will stop
readosm_full_node = READOSM_OK

END FUNCTION readosm_full_node


!> Predefined callback for parsing ways.
!! It receives an object of the type \a readosm_full_f as \a user_data
!! and adds to it the way entity received.
FUNCTION readosm_full_way(user_data, way) BIND(C)
TYPE(c_ptr),VALUE :: user_data
TYPE(readosm_way) :: way
INTEGER(kind=c_int) :: readosm_full_way

TYPE(readosm_full_f),POINTER :: fulldata

! cast user_data to the desired fortran object and insert the entity
! in "Fortran-friendly" format
CALL C_F_POINTER(user_data, fulldata)
CALL insert(fulldata%ways, readosm_object_f(way))
! set the return code to OK, otherwise parsing will stop
readosm_full_way = READOSM_OK

END FUNCTION readosm_full_way


!> Predefined callback for parsing relations.
!! It receives an object of the type \a readosm_full_f as \a user_data
!! and adds to it the relation entity received.
FUNCTION readosm_full_relation(user_data, relation) BIND(C)
TYPE(c_ptr),VALUE :: user_data
TYPE(readosm_relation) :: relation
INTEGER(kind=c_int) :: readosm_full_relation

TYPE(readosm_full_f),POINTER :: fulldata

! cast user_data to the desired fortran object and insert the entity
! in "Fortran-friendly" format
CALL C_F_POINTER(user_data, fulldata)
CALL insert(fulldata%relations, readosm_object_f(relation))
! set the return code to OK, otherwise parsing will stop
readosm_full_relation = READOSM_OK

END FUNCTION readosm_full_relation


END MODULE readosm
