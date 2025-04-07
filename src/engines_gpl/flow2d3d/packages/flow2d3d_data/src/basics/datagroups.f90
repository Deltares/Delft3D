module datagroups
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
!   NONE
!!--declarations----------------------------------------------------------------

integer, parameter :: ATP_INT  = 1
integer, parameter :: ATP_REAL = 2
integer, parameter :: ATP_CHAR = 3

integer, parameter :: ERRLOG_NONE = 0
integer, parameter :: ERRLOG_CELL_ERRORS = 1
integer, parameter :: ERRLOG_ALL = 2

interface addatt
    module procedure addatt_char
    module procedure addatt_int
    module procedure addatt_fp
end interface addatt

interface defnewgrp
    module procedure defnewgrp_openclose
    module procedure defnew_allgrps
    module procedure defnewgrp_core
end interface defnewgrp

contains

integer function getfiletype(gdp, ifile)
!!--description-----------------------------------------------------------------
!
!    Function: Return the file type
!
!!--declarations----------------------------------------------------------------
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer                  , intent(in)  :: ifile
!
! Local variables
!
!   NONE
!
!! executable statements -------------------------------------------------------
!
    getfiletype = gdp%iofiles(ifile)%filetype
end function getfiletype


subroutine getdatagroup(gdp, ifile, gname, group)
!!--description-----------------------------------------------------------------
!
!    Function: Get pointer to data group (create group if it does not yet exist)
!
!!--declarations----------------------------------------------------------------
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                  , intent(in)  :: ifile
    character(*)             , intent(in)  :: gname
    type(datagroup)          , pointer     :: group
!
! Local variables
!
    integer  :: ig
!
!! executable statements -------------------------------------------------------
!
    do ig = 1, MAXNR_GROUP
       if (gdp%iofiles(ifile)%group(ig)%name == gname) then
           exit
       elseif (gdp%iofiles(ifile)%group(ig)%name == ' ') then
           gdp%iofiles(ifile)%group(ig)%name = gname
           exit
       endif
    enddo
    group => gdp%iofiles(ifile)%group(ig)
end subroutine getdatagroup


function adddim(gdp, lundia, ifile, dname, length) result (id)
!!--description-----------------------------------------------------------------
!
!    Function: Store dimension information in io data structure
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use m_alloc
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    character(64), dimension(:), pointer :: dim_name
    integer      , dimension(:), pointer :: dim_length
!
! Global variables
!
    integer                  , intent(in)  :: lundia
    integer                  , intent(in)  :: ifile
    character(*)             , intent(in)  :: dname
    integer                  , intent(in)  :: length
    integer                                :: id
!
! Local variables
!
    integer         :: ndim
    integer         :: istat
    character(1024) :: error_string
!
!! executable statements -------------------------------------------------------
!
    dim_name   => gdp%iofiles(ifile)%dim_name
    dim_length => gdp%iofiles(ifile)%dim_length
    ndim = size(dim_name)
    do id = 1, ndim
       if (dim_name(id) == dname) then
          if (dim_length(id) == length) then
             return
          else
             write(error_string,'(a,i0,a,i0)') 'Adddim: inconsistent definition of dimension '//trim(dname)//' Old value: ',dim_length(id),' New value: ',length
             call prterr(lundia, 'U021', trim(error_string))
             call d3stop(1, gdp)
          endif
       endif
    enddo
    !
    id = ndim+1
    call reallocP(gdp%iofiles(ifile)%dim_name, id, stat=istat)
    call reallocP(gdp%iofiles(ifile)%dim_length, id, stat=istat)
    call reallocP(gdp%iofiles(ifile)%dim_id, id, stat=istat)
    gdp%iofiles(ifile)%dim_name(id) = dname
    gdp%iofiles(ifile)%dim_length(id) = length
    gdp%iofiles(ifile)%dim_id(id) = -999
end function adddim


function addatt_char(gdp, lundia, ifile, aname, aval) result (id)
!!--description-----------------------------------------------------------------
!
!    Function: Store dimension information in io data structure
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use m_alloc
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                  , intent(in)  :: lundia
    integer                  , intent(in)  :: ifile
    character(*)             , intent(in)  :: aname
    character(*)             , intent(in)  :: aval
    integer                                :: id
!
! Local variables
!
    integer  :: istat
!
!! executable statements -------------------------------------------------------
!
    id = size(gdp%iofiles(ifile)%att_name)+1
    call reallocP(gdp%iofiles(ifile)%att_name, id, stat=istat)
    call reallocP(gdp%iofiles(ifile)%att_vtype, id, stat=istat)
    call reallocP(gdp%iofiles(ifile)%att_ival, id, stat=istat, fill=0)
    call reallocP(gdp%iofiles(ifile)%att_rval, id, stat=istat, fill=0.0_fp)
    call reallocP(gdp%iofiles(ifile)%att_cval, id, stat=istat, fill='')
    gdp%iofiles(ifile)%att_name(id) = aname
    gdp%iofiles(ifile)%att_vtype(id) = ATP_CHAR
    gdp%iofiles(ifile)%att_cval(id) = aval
end function addatt_char


function addatt_int(gdp, lundia, ifile, aname, aval) result (id)
!!--description-----------------------------------------------------------------
!
!    Function: Store dimension information in io data structure
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use m_alloc
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                  , intent(in)  :: lundia
    integer                  , intent(in)  :: ifile
    character(*)             , intent(in)  :: aname
    integer             , intent(in)  :: aval
    integer                                :: id
!
! Local variables
!
    integer  :: istat
!
!! executable statements -------------------------------------------------------
!
    id = size(gdp%iofiles(ifile)%att_name)+1
    call reallocP(gdp%iofiles(ifile)%att_name, id, stat=istat)
    call reallocP(gdp%iofiles(ifile)%att_vtype, id, stat=istat)
    call reallocP(gdp%iofiles(ifile)%att_ival, id, stat=istat, fill=0)
    call reallocP(gdp%iofiles(ifile)%att_rval, id, stat=istat, fill=0.0_fp)
    call reallocP(gdp%iofiles(ifile)%att_cval, id, stat=istat, fill='')
    gdp%iofiles(ifile)%att_name(id) = aname
    gdp%iofiles(ifile)%att_vtype(id) = ATP_INT
    gdp%iofiles(ifile)%att_ival(id) = aval
end function addatt_int


function addatt_fp(gdp, lundia, ifile, aname, aval) result (id)
!!--description-----------------------------------------------------------------
!
!    Function: Store dimension information in io data structure
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use m_alloc
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                  , intent(in)  :: lundia
    integer                  , intent(in)  :: ifile
    character(*)             , intent(in)  :: aname
    real(fp)                 , intent(in)  :: aval
    integer                                :: id
!
! Local variables
!
    integer  :: istat
!
!! executable statements -------------------------------------------------------
!
    id = size(gdp%iofiles(ifile)%att_name)+1
    call reallocP(gdp%iofiles(ifile)%att_name, id, stat=istat)
    call reallocP(gdp%iofiles(ifile)%att_vtype, id, stat=istat)
    call reallocP(gdp%iofiles(ifile)%att_ival, id, stat=istat, fill=0)
    call reallocP(gdp%iofiles(ifile)%att_rval, id, stat=istat, fill=0.0_fp)
    call reallocP(gdp%iofiles(ifile)%att_cval, id, stat=istat, fill='')
    gdp%iofiles(ifile)%att_name(id) = aname
    gdp%iofiles(ifile)%att_vtype(id) = ATP_REAL
    gdp%iofiles(ifile)%att_rval(id) = aval
end function addatt_fp


subroutine addatt_class(gdp, lundia, ifile, label, attribs)
!!--description-----------------------------------------------------------------
!
!    Function: Store dimension information in io data structure
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use m_alloc
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    type(iofile)   , pointer :: file
!
! Global variables
!
    integer                  , intent(in)  :: lundia
    integer                  , intent(in)  :: ifile
    character                , intent(in)  :: label
    integer, dimension(:)    , intent(in)  :: attribs
!
! Local variables
!
    integer  :: ia
    integer  :: ic
    integer  :: dim1
    integer  :: dim2
    integer  :: istat
    integer  :: nattribs
!
!! executable statements -------------------------------------------------------
!
    file => gdp%iofiles(ifile)
    nattribs = size(attribs)
    dim1 = size(file%acl_attrib,1)
    dim2 = size(file%acl_attrib,2)
    ic = 1
    do while (ic <= dim2)
       if (file%acl_label(ic) == label) exit
       ic = ic+1
    enddo
    if (ic > dim2) then
       dim1 = max( size(attribs), dim1 )
       call reallocP(file%acl_label, ic, stat=istat)
       call reallocP(file%acl_attrib, (/dim1, ic/), stat=istat, fill=0)
    elseif (nattribs > dim1) then
       call reallocP(file%acl_attrib , (/nattribs, dim2/), stat=istat, fill=0)
    endif
    file%acl_label(ic) = label
    file%acl_attrib(:,ic) = 0
    do ia = 1,nattribs
       file%acl_attrib(ia,ic) = attribs(ia)
       if (attribs(ia)<0 .or. attribs(ia)>size(file%att_name)) then
          call prterr(lundia, 'U021', 'Addelm: invalid attribute id specified for attribute class "'//label//'"')
          call d3stop(1, gdp)
       endif
    enddo
end subroutine addatt_class


subroutine addelm(gdp, lundia, ifile, gname, name, sname, itype, ndims, dimsize, qty, &
                & longname, unit, dimids, attribs, acl)
!!--description-----------------------------------------------------------------
!
!    Function: Store element information in io data structure
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use m_alloc
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer        , pointer :: nelmx
    type(iofile)   , pointer :: file
    type(datagroup), pointer :: group
!
! Global variables
!
    integer                            , intent(in)  :: lundia
    integer                            , intent(in)  :: ifile
    character(*)                       , intent(in)  :: gname
    character(*)                       , intent(in)  :: name
    character(*)                       , intent(in)  :: sname
    integer                            , intent(in)  :: itype
    integer                            , intent(in)  :: ndims
    integer, dimension(ndims), optional, intent(in)  :: dimsize
    character(*)             , optional, intent(in)  :: qty
    character(*)             , optional, intent(in)  :: longname
    character(*)             , optional, intent(in)  :: unit
    integer, dimension(ndims), optional, intent(in)  :: dimids
    integer, dimension(:)    , optional, intent(in)  :: attribs
    character                , optional, intent(in)  :: acl
!
! Local variables
!
    integer  :: ia
    integer  :: iacl
    integer  :: ic
    integer  :: id
    integer  :: ie
    integer  :: istat  ! Help var. memory allocation
    integer  :: nattr
    integer  :: acl_nattr
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, ifile, gname, group)
    nelmx => group%nelmx
    file => gdp%iofiles(ifile)
    !
    ! Allocate memory: Update space allocated and set
    !                  element properties
    !
    istat = 0
    ie    = nelmx + 1
    !
    !  Extend arrays
    !
    iacl = 0
    acl_nattr = 0
    if (present(acl)) then
       do ic = 1, size(file%acl_label)
          if (file%acl_label(ic) == acl) then
             iacl = ic
             exit
          endif
       enddo
       if (iacl == 0) then
          call prterr(lundia, 'U021', 'Addelm: invalid attribute class "'//acl//'" specified for variable "'//trim(name)//'"')
          call d3stop(1, gdp)
       endif
       do ia = 1, size(file%acl_attrib,1)
          if (file%acl_attrib(ia,iacl) == 0) exit
          acl_nattr = acl_nattr+1
       enddo
    endif
    if (present(attribs)) then
       nattr = size(attribs) + acl_nattr
    else
       nattr = acl_nattr
    endif
    if (associated(group%elm_attrib)) then
       nattr = max(nattr, size(group%elm_attrib,1))
    endif
    !
    ! The first variables will always be specified, so no fill value needed
    !
                  call reallocP(group%elm_name  , ie, stat=istat)
    if (istat==0) call reallocP(group%elm_sname , ie, stat=istat)
    if (istat==0) call reallocP(group%elm_type  , ie, stat=istat)
    if (istat==0) call reallocP(group%elm_ndims , ie, stat=istat)
    !
    ! The remaining variables may not be specified, so fill using default
    !
    if (istat==0) call reallocP(group%elm_dims  , (/ 5, ie /), fill=0, stat=istat)
    if (istat==0) call reallocP(group%elm_size  , (/ 5, ie /), fill=0, stat=istat)
    if (istat==0) call reallocP(group%elm_qty   , ie, fill=' ', stat=istat)
    if (istat==0) call reallocP(group%elm_unit  , ie, fill=' ', stat=istat)
    if (istat==0) call reallocP(group%elm_lname , ie, fill=' ', stat=istat)
    if (istat==0) call reallocP(group%elm_attrib, (/ nattr,ie /), fill=0, stat=istat)
    if (istat/=0) then
       !
       !  Memory error
       !
       call prterr(lundia, 'U021', 'Addelm: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    !  Element name, type, number of dimensions
    !
    group%elm_name(ie)  = name
    group%elm_sname(ie) = sname
    group%elm_type(ie)  = itype
    group%elm_ndims(ie) = ndims
    !
    !  Element dimensions and size
    !
    if (ndims==0 .or. present(dimids)) then
       do id = 1,ndims
          group%elm_dims(id,ie) = dimids(id)
          if (dimids(id)<1 .or. dimids(id)>size(gdp%iofiles(ifile)%dim_name)) then
             call prterr(lundia, 'U021', 'Addelm: invalid dimensions id specified for variable "'//trim(name)//'"')
             call d3stop(1, gdp)
          endif
          group%elm_size(id,ie) = gdp%iofiles(ifile)%dim_length(dimids(id))
       enddo
    elseif (present(dimsize)) then
       do id = 1,ndims
          group%elm_dims(id,ie) = -dimsize(id)
          group%elm_size(id,ie) = dimsize(id)
       enddo
    else
       call prterr(lundia, 'U021', 'Addelm: dimids or dimsize needs to be specified.')
       call d3stop(1, gdp)
    endif
    !
    !  Element quantity
    !
    if (present(qty)) then
       group%elm_qty(ie) = qty
    endif
    !
    !  Element unit
    !
    if (present(unit)) then
       group%elm_unit(ie) = unit
    endif
    !
    !  Element description
    !
    if (present(longname)) then
       group%elm_lname(ie) = longname
    endif
    !
    if (iacl>0) then
       do id = 1, acl_nattr
          group%elm_attrib(id,ie) = file%acl_attrib(id,iacl)
       enddo
    endif
    if (present(attribs)) then
       do id = 1, size(attribs)
          group%elm_attrib(acl_nattr+id,ie) = attribs(id)
          if (attribs(id)<0 .or. attribs(id)>size(gdp%iofiles(ifile)%att_name)) then
             call prterr(lundia, 'U021', 'Addelm: invalid attribute id specified for variable "'//trim(name)//'"')
             call d3stop(1, gdp)
          endif
       enddo
    endif
    !
    !  Update number of elements in group
    !
    nelmx = ie
end subroutine addelm

                
subroutine defnewgrp_openclose(filnam, ifile, gname, gdp)
!!--description-----------------------------------------------------------------
!
!    Function: Write group definition metadata to file.
!
!!--declarations----------------------------------------------------------------
    !use precision
    use netcdf
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                      , pointer :: lundia
    type (iofile)                , pointer :: file
!
! Global variables
!
    character(*), intent(in) :: filnam     !!  Name of NEFIS file
    integer     , intent(in) :: ifile      !!  File type
    character(*)             :: gname      !!  Name of data group. This name is
                                           !!  also used as the name for the cell
                                           !!  and group-definition.
!
! Local variables
!
    integer                                :: error
    integer                                :: fds
    !
    integer, external                      :: clsnef
    integer, external                      :: open_datdef
    integer, external                      :: neferr
    character(1024)                        :: error_string
!
!! executable statements -------------------------------------------------------
!
    file      => gdp%iofiles(ifile)
    lundia    => gdp%gdinout%lundia
    if (file%filetype == FTYPE_NEFIS) then
       error = open_datdef(filnam   ,fds      , .false.)
       if (error /= 0) then
          write(error_string,'(2a)') 'While trying to open dat/def-file ',trim(filnam)
          call prtnefiserr(trim(error_string), gdp)
          error = 0
       endif
       !
       call defnewgrp_core(fds, ifile, gname, gdp, filnam)
       !
       error = clsnef(fds)
       if (error /= 0) then
          write(error_string,'(2a)') 'While trying to close def/dat-file ', trim(filnam)
          call prtnefiserr(trim(error_string), gdp)
          write(lundia,*) 'While trying to close file ',trim(filnam)
       endif
    else
       write(lundia,'(2A)') 'Invalid call to DEFNEWGRP for ', trim(filnam)
    endif
end subroutine defnewgrp_openclose


subroutine defnew_allgrps(fds, ifile, gdp, filnam)
!!--description-----------------------------------------------------------------
!
!    Function: Write group definition metadata for all groups to file.
!
!!--declarations----------------------------------------------------------------
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    type (iofile)                , pointer :: file
!
! Global variables
!
    character(*), intent(in)           :: filnam     !!  Name of NEFIS file
    integer     , intent(in)           :: ifile      !!  File type
    integer                            :: fds
!
! Local variables
!
    integer                            :: i
!
!! executable statements -------------------------------------------------------
!
    file      => gdp%iofiles(ifile)
    do i = 1,size(file%group)
       if (file%group(i)%name == ' ') return
       call defnewgrp_core(fds, ifile, file%group(i)%name, gdp, filnam)
    enddo
end subroutine defnew_allgrps


subroutine defnewgrp_core(fds, ifile, gname, gdp, filnam, grpdim, errlog)
!!--description-----------------------------------------------------------------
!
!    Function: Write group definition metadata to file.
!
!!--declarations----------------------------------------------------------------
    use netcdf
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                      , pointer :: lundia
    integer      , dimension(:)  , pointer :: elm_ndims
    integer      , dimension(:)  , pointer :: elm_type
    integer      , dimension(:,:), pointer :: elm_dims
    integer      , dimension(:,:), pointer :: elm_size
    integer      , dimension(:,:), pointer :: elm_attrib
    type (iofile)                , pointer :: file
    type (datagroup)             , pointer :: group
    integer                      , pointer :: nelmx
    character(64), dimension(:)  , pointer :: elm_unit
    character(64), dimension(:)  , pointer :: elm_name
    character(16), dimension(:)  , pointer :: elm_qty
    character(256), dimension(:) , pointer :: elm_lname
    character(64), dimension(:)  , pointer :: elm_sname
!
! Global variables
!
    character(*), intent(in)           :: filnam     !!  Name of NEFIS file
    integer     , intent(in)           :: ifile      !!  File type
    character(*)                       :: gname      !!  Name of data group. This name is
                                                     !!  also used as the name for the cell
                                                     !!  and group-definition.
    integer                            :: fds
    integer     , intent(in), optional :: grpdim
    integer     , intent(in), optional :: errlog
!
! Local variables
!
    integer                                   :: error
    integer                                   :: i
    integer                                   :: id
    integer                                   :: idm
    integer                                   :: idf
    integer                                   :: ie
    integer                                   :: istat
    integer                                   :: varid
    character(10)                             :: strlen
    !
    integer, external                         :: clsnef
    integer, external                         :: credat
    integer, external                         :: open_datdef
    integer, external                         :: defcel
    integer, external                         :: defelm
    integer, external                         :: defgrp
    integer, external                         :: neferr
    !
    integer                                   :: err_log
    integer       , dimension(:), allocatable :: localdim
    integer                                   :: ndims
    integer                                   :: nbytes
    integer                                   :: nctype
    character(8)                              :: nefistyp
    character(10)                             :: nefisunit
    character(16)                             :: localnm
    character(16), dimension(:) , allocatable :: localnms
    character(16)                             :: localqty
    character(64)                             :: localdes
    character(1024)                           :: error_string
!
!! executable statements -------------------------------------------------------
!
    lundia    => gdp%gdinout%lundia
    file      => gdp%iofiles(ifile)
    call getdatagroup(gdp, ifile, gname, group)
    elm_dims   => group%elm_dims
    nelmx      => group%nelmx
    elm_unit   => group%elm_unit
    elm_name   => group%elm_name
    elm_qty    => group%elm_qty
    elm_type   => group%elm_type
    elm_lname  => group%elm_lname
    elm_sname  => group%elm_sname
    elm_ndims  => group%elm_ndims
    elm_size   => group%elm_size
    elm_type   => group%elm_type
    elm_attrib => group%elm_attrib
    !
    if (present(grpdim)) then
       group%grp_dim = grpdim
    endif
    if (present(errlog)) then
       err_log = errlog
    else
       !
       ! Defining an element twice will cause an error code. However, some
       ! groups have the same element, for instance the groups 'map-rol-series'
       ! and 'map-infsed-serie' both have the element ITMAPS. Let's assume
       ! that the element errors are of this type and don't need to be reported.
       ! If it is a more serious error, we will probably get a clear error
       ! during writing. So, only report cell errors at this stage.
       !
       err_log = ERRLOG_CELL_ERRORS
    endif
    !
    ! Create elements
    !
    do ie = 1, nelmx
       select case (elm_type(ie))
       !case (IO_LOGIC2)
       !   nefistyp = 'LOGICAL'
       !   nbytes   = 2
       !   nctype   = N/A
       !case (IO_LOGIC4)
       !   nefistyp = 'LOGICAL'
       !   nbytes   = 4
       !   nctype   = N/A
       case (IO_INT2)
          nefistyp = 'INTEGER'
          nbytes   = 2
          nctype   = nf90_short
       case (IO_INT4)
          nefistyp = 'INTEGER'
          nbytes   = 4
          nctype   = nf90_int
       !case (IO_INT8)
       !   nefistyp = 'INTEGER'
       !   nbytes   = 8
       !   nctype   = N/A
       case (IO_REAL4)
          nefistyp = 'REAL'
          nbytes   = 4
          nctype   = nf90_float
       case (IO_REAL8)
          nefistyp = 'REAL'
          nbytes   = 8
          nctype   = nf90_double
       !case (FILOUT_COMPLEX8)
       !   nefistyp = 'COMPLEX'
       !   nbytes   = 8
       !   nctype   = N/A
       !case (FILOUT_COMPLEX16)
       !   nefistyp = 'COMPLEX'
       !   nbytes   = 16
       !   nctype   = N/A
       case default
          nefistyp = 'CHARACTE'!R
          nbytes   = elm_type(ie)
          nctype   = nf90_char
       end select
       !
       ! Local copies of elementtype/elementquantity should not be necessary
       ! But testcase 17.03-2dred1dump really prefers it
       !
       localqty = elm_qty(ie)(1:16)
       localnm  = elm_name(ie)(1:16)
       localdes = elm_lname(ie)(1:64)
       select case (elm_unit(ie))
       case ('')
          nefisunit = '[   -   ]'
       case ('days')
          nefisunit = '[  DAYS ]'
       case ('degrees_Celsius','arc_degrees','degrees_east','degrees_north')
          nefisunit = '[  DEG  ]'
       case ('h')
          nefisunit = '[ HOUR  ]'
       case ('J/m2')
          nefisunit = '[  J/M2 ]'
       case ('kg')
          nefisunit = '[  KG   ]'
       case ('kg/s', 'kg s-1')
          nefisunit = '[  KG/S ]'
       case ('kg/m2')
          nefisunit = '[ KG/M2 ]'
       case ('kg/m3')
          nefisunit = '[ KG/M3 ]'
       case ('kg/(s m)', 'kg s-1 m-1')
          nefisunit = '[ KG/S/M]'
       case ('kg/(m3 s)')
          nefisunit = '[KG/M3/S]'
       case ('N/m2')
          nefisunit = '[  N/M2 ]'
       case ('m')
          nefisunit = '[   M   ]'
       case ('m0.5/s')
          nefisunit = '[M0.5/S ]'
       case ('m2')
          nefisunit = '[  M2   ]'
       case ('m/s')
          nefisunit = '[  M/S  ]'
       case ('m/s2')
          nefisunit = '[  M/S2 ]'
       case ('m2/s')
          nefisunit = '[  M2/S ]'
       case ('m3')
          nefisunit = '[  M3   ]'
       case ('m3/s', 'm3 s-1')
          nefisunit = '[  M3/S ]'
       case ('m3/(s m)', 'm3 s-1 m-1')
          nefisunit = '[ M3/S/M]'
       case ('mm/h')
          nefisunit = '[  MM/H ]'
       case ('percent')
          nefisunit = '[   %   ]'
       case ('s')
          nefisunit = '[   S   ]'
       case ('1/s')
          nefisunit = '[  1/S  ]'
       case ('1/s2')
          nefisunit = '[  1/S2 ]'
       case ('s/m{1/3}')
          nefisunit = '[S/M{1/3}]'
       case ('W/m2')
          nefisunit = '[ W/M2 ]'
       case ('W/(m2 K)')
          nefisunit = '[W/M2K ]'
       case default
          nefisunit = elm_unit(ie)(1:10)
       end select
       if (file%filetype == FTYPE_NEFIS) then
          if (elm_ndims(ie)==0) then
             elm_ndims(ie) = 1
             elm_size(1,ie) = 1
          endif
          error = defelm(fds   , localnm    , nefistyp  , nbytes    , &
                & localqty     , nefisunit  , localdes  ,             &
                & elm_ndims(ie), elm_size(1, ie))
          if (error /= 0) then
             if (err_log >= ERRLOG_ALL) then
                write(error_string,'(5a)') 'While trying to define the element ', &
                              & trim(localnm), ' in file ', trim(filnam), '.def'
                call prtnefiserr(trim(error_string), gdp)
             endif
             error = 0
          endif
       else ! FTYPE_NETCDF
          !
          ! Count the number of dimensions
          !
          ndims = elm_ndims(ie)
          if (nctype == nf90_char) ndims = ndims+1
          if (group%grp_dim > 0)   ndims = ndims+1
          !
          ! Collect the in-memory indices of the dimensions
          !
          allocate(localdim(ndims))
          i = 1
          if (nctype == nf90_char) then
             strlen = 'strlen'
             if (elm_type(ie)<10) then
                write(strlen(7:7),'(I1)') elm_type(ie)
             elseif (elm_type(ie)<100) then
                write(strlen(7:8),'(I2)') elm_type(ie)
             elseif (elm_type(ie)<1000) then
                write(strlen(7:9),'(I3)') elm_type(ie)
             elseif (elm_type(ie)<10000) then
                write(strlen(7:10),'(I4)') elm_type(ie)
             endif
             localdim(i) = adddim(gdp, lundia, ifile, strlen, elm_type(ie))
             i = i+1
          endif
          do id = 1,elm_ndims(ie)
             localdim(i) = elm_dims(id, ie)
             i = i+1
          enddo
          if (group%grp_dim > 0) then
             localdim(i) = group%grp_dim
          endif
          !
          ! Convert the in-memory indices to the file-based indices
          ! Define the dimension if necessary
          !
          do i = 1,ndims
             idm = localdim(i)
             if (idm<0) then
                write(lundia,'(a,i5,5a)') 'Dimension size (',-idm,') specified for variable ',trim(elm_name(ie)),' on ',trim(filnam),'. A dimension id is needed!'
                call d3stop(1, gdp)
             endif
             idf = file%dim_id(idm)
             if (idf<0) then
                error = nf90_def_dim(fds, file%dim_name(idm), file%dim_length(idm), idf)
                call nc_check_err(lundia, error, 'writing dimensions for '//trim(elm_name(ie)), filnam)
                file%dim_id(idm) = idf
             endif
             localdim(i) = idf
          enddo
          !
          ! Now define the variable
          !
          error = nf90_def_var(fds, elm_name(ie), nctype, localdim, varid, deflate_level = gdp%gdpostpr%nc_deflate)
          call nc_check_err(lundia, error, 'writing '//trim(elm_name(ie)), filnam)
          deallocate(localdim)
          !
          ! Add attributes as needed
          !
          if (elm_sname(ie) /= ' ') then
             error = nf90_put_att(fds, varid, 'standard_name', elm_sname(ie))
             call nc_check_err(lundia, error, 'writing standard_name attribute for '//trim(elm_name(ie)), filnam)
          endif
          if (elm_lname(ie) /= ' ') then
             error = nf90_put_att(fds, varid, 'long_name'    , elm_lname(ie))
             call nc_check_err(lundia, error, 'writing long_name attribute for '//trim(elm_name(ie)), filnam)
          endif
          ! dimensionless numbers get unit "1" in cf/udunits
          if (elm_unit(ie) == ' ') then
             if (nctype /= nf90_char) then
                elm_unit(ie) = '1'
             endif
          endif
          if (elm_unit(ie) /= ' ') then
             error = nf90_put_att(fds, varid, 'units'        , elm_unit(ie))
             call nc_check_err(lundia, error, 'writing units attribute for '//trim(elm_name(ie)), filnam)
          endif
          do i = 1,size(elm_attrib,1)
             idm = elm_attrib(i,ie)
             if (idm > 0) then
                if (file%att_vtype(idm) == ATP_CHAR) then
                   error = nf90_put_att(fds, varid, trim(file%att_name(idm)), trim(file%att_cval(idm)))
                elseif (file%att_vtype(idm) == ATP_INT) then
                   error = nf90_put_att(fds, varid, trim(file%att_name(idm)), file%att_ival(idm))
                elseif (file%att_vtype(idm) == ATP_REAL) then
                   error = nf90_put_att(fds, varid, trim(file%att_name(idm)), file%att_rval(idm))
                else
                   !error
                endif
                call nc_check_err(lundia, error, 'writing '//trim(file%att_name(idm))//' attribute for '//trim(elm_name(ie)), filnam)
             endif
          enddo
       endif
    enddo
    !
    if (file%filetype == FTYPE_NEFIS) then
       !
       ! Create cell
       !
       allocate(localnms(nelmx), stat=istat)
       do ie = 1, nelmx
          localnms(ie) = elm_name(ie)
       enddo
       error = defcel(fds, gname, nelmx, localnms)
       deallocate(localnms, stat=istat)
       !
       if (error /= 0) then
          if (err_log >= ERRLOG_CELL_ERRORS) then
             write(error_string,'(5a)') 'While trying to define a cell for group ', &
                           & trim(gname), ' in file ', trim(filnam), '.def'
             call prtnefiserr(trim(error_string), gdp)
          endif
          error = 0
       endif
       !
       ! Create group on definition file
       !
       error = defgrp(fds, gname, gname, 1, 0, 1)
       if (error /= 0) then
          if (err_log >= ERRLOG_ALL) then
             write(error_string,'(5a)') 'While trying to define group ', &
                           & trim(gname), ' in file ', trim(filnam), '.def'
             call prtnefiserr(trim(error_string), gdp)
          endif
          error = 0
       endif
       !
       ! Create group on data file
       !
       error = credat(fds, gname, gname)
       if (error /= 0) then
          if (err_log >= ERRLOG_ALL) then
             write(error_string,'(5a)') 'While trying to create group ', &
                           & trim(gname), ' in file ', trim(filnam), '.dat'
             call prtnefiserr(trim(error_string), gdp)
          endif
          error = 0
       endif
    else
       !
    endif
end subroutine defnewgrp_core
                
                
end module datagroups
