module table_handles
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!
! Handle wrapper for tables module.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use tables
    use handles
!
!! -----------------------------------------------------------------------------
!
    private
    public handletype
    public readtable
    public cleartable
    public getntables
    public gettable
    public checktable
    public checktableparnames
    public gettablelocation
    public gettablentimes
    public gettabletimes
    public gettabledata
    public getfilename
    public validtable
    !
    public MAXTABLECLENGTH

    public CHKTAB_PARNAME
    public CHKTAB_POSITIVE
    public CHKTAB_BLOCK
    public CHKTAB_LOGICAL

!
!! -----------------------------------------------------------------------------
!
    interface gettabletimes
       module procedure gettabletimes_hp
       module procedure gettabletimes_sp
    end interface gettabletimes

    interface gettable
       module procedure gettable_vector, gettable_scalar
    end interface

    interface gettabledata
       module procedure gettabledata_vector_hp
       module procedure gettabledata_vector_sp
       module procedure gettabledata_scalar_hp
       module procedure gettabledata_scalar_sp
    end interface

    type tablefiletypehandle
       type(tablefiletype), pointer :: this => NULL()
       integer :: htype = -999
    endtype tablefiletypehandle

    integer :: htype = -999

contains

subroutine readtable(handle, filnam, refjulday, errorstring)
!
! Global variables
!
    integer            ,intent(in)  :: refjulday
    character(*)       ,intent(in)  :: filnam
    type(handletype)                :: handle
    character(256)     ,intent(out) :: errorstring
!
! Local variables
!
    integer                         :: istat
    type(tablefiletypehandle)       :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    call newtablehandle(tablehandle,errorstring)
    if (errorstring /= '') return
    !
    if (.not.associated(tablehandle%this)) then
       istat = 0
       allocate(tablehandle%this, stat = istat)
       if (istat /= 0) then
          errorstring = 'GETDATAFILE: Memory allocation error'
          return
       endif
    endif
    call org_readtable(tablehandle%this, filnam, refjulday, errorstring)
    if (errorstring /= '') return
    handle = cast_from_tablehandle(tablehandle)
    !
end subroutine readtable


subroutine cleartable(handle)
!!--description-----------------------------------------------------------------
!
!    Function: Deallocate data object
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(handletype)                     :: handle
!
! Local variables
!
    type(tablefiletypehandle)            :: tablehandle
    integer                              :: istat
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle)) return
    tablehandle = cast_to_tablehandle(handle)
    if (associated(tablehandle%this)) then
       call org_cleartable(tablehandle%this)
       deallocate(tablehandle%this, stat = istat)
    endif
    handle = cast_from_tablehandle(tablehandle)
end subroutine cleartable


subroutine gettable_vector(handle    ,location  ,parname   ,ivec      , &
                         & nparmin   ,errorstring)
!
! Global variables
!
    integer, dimension(4)  ,intent(out) :: ivec
    integer                ,intent(in)  :: nparmin
    character(*)           ,intent(in)  :: location
    character(*)           ,intent(in)  :: parname
    character(256)         ,intent(out) :: errorstring
    type(handletype)       ,intent(in)  :: handle
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    call gettable_scalar(handle    ,location  ,parname   , &
                       & ivec(1)    ,ivec(2)   ,ivec(3)   ,nparmin   , &
                       & errorstring)
    ivec(4) = 1
    !
end subroutine gettable_vector


subroutine gettable_scalar(handle    ,location  ,parname   ,itable    , &
                         & ipar      ,npar      ,nparmin   ,errorstring)
!
! Global variables
!
    integer                ,intent(out) :: itable
    integer                ,intent(out) :: ipar
    integer                ,intent(out) :: npar
    integer                ,intent(in)  :: nparmin
    character(*)           ,intent(in)  :: location
    character(*)           ,intent(in)  :: parname
    character(256)         ,intent(out) :: errorstring
    type(handletype)       ,intent(in)  :: handle
!
! Local variables
!
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'GetTable call before initialisation'
    else
       call org_gettable(tablehandle%this      ,location  ,parname   , &
                       & itable     ,ipar      ,npar      ,nparmin   , &
                       & errorstring)
    endif
    !
end subroutine gettable_scalar

subroutine gettabletimes_sp(handle     ,itable     ,times      ,refjulday  , &
                       & errorstring)
!
! Global variables
!
    integer                ,intent(in)  :: itable
    integer                ,intent(in)  :: refjulday
    real(sp), dimension(:) ,intent(out) :: times
    character(256)         ,intent(out) :: errorstring
    type(handletype)       ,intent(in)  :: handle
!
! Local variables
!
    integer                             :: i
    integer                             :: istat
    real(hp), dimension(:), allocatable :: times_hp
!
!! executable statements -------------------------------------------------------
!
    allocate(times_hp(size(times,1)), stat=istat)
    do i=1,size(times,1)
       times_hp(i) = real(times(i),hp)
    enddo
    if (istat /= 0) then
       errorstring = 'GETDATAFILE: Memory allocation error'
       return
    endif
    call gettabletimes_hp(handle, itable, times_hp, refjulday, &
                       & errorstring)
    do i=1,size(times,1)
       times(i) = real(times_hp(i),sp)
    enddo
    deallocate(times_hp, stat=istat)
end subroutine gettabletimes_sp


subroutine gettabletimes_hp(handle     ,itable     ,times      ,refjulday  , &
                       & errorstring)
!
! Global variables
!
    integer                ,intent(in)  :: itable
    integer                ,intent(in)  :: refjulday
    real(hp), dimension(:) ,intent(out) :: times
    character(256)         ,intent(out) :: errorstring
    type(handletype)       ,intent(in)  :: handle
!
! Local variables
!
    integer                             :: i
    integer                             :: istat
    real(fp), dimension(:), allocatable :: times_fp
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'GetTableTimes call before initialisation'
    else
       allocate(times_fp(size(times)), stat = istat)
       if (istat /= 0) then
          errorstring = 'gettabletimes_hp: Memory allocation error'
          return
       endif
       do i=1,size(times)
          times_fp(i) = real(times(i),fp)
       enddo
       call org_gettabletimes(tablehandle%this       ,itable     ,times_fp   , &
                            & refjulday  ,errorstring)
       do i=1,size(times)
          times(i) = real(times_fp(i),hp)
       enddo
       deallocate(times_fp, stat = istat)
    endif
    !
end subroutine gettabletimes_hp


subroutine gettabledata_vector_sp(handle     ,ivec       ,values     , &
                             & timhr      ,refjulday  ,errorstring, extrapol_in)
!
! Global variables
!
    integer, dimension(4)               :: ivec
    integer                ,intent(in)  :: refjulday
    real(sp), optional     ,intent(in)  :: extrapol_in
    real(sp)               ,intent(in)  :: timhr
    real(sp), dimension(:) ,intent(out) :: values
    character(256)         ,intent(out) :: errorstring
    type(handletype)       ,intent(in)  :: handle
!
! Local variables
!
    real(sp)                  :: extrapol
!
!! executable statements -------------------------------------------------------
!
    if (present(extrapol_in)) then
       extrapol = extrapol_in
    else
       extrapol = 0.0_sp
    endif
    call gettabledata_scalar_sp(handle     ,ivec(1)    ,ivec(2)    , &
                 & ivec(3)    ,ivec(4)    ,values     ,timhr      , &
                 & refjulday  ,errorstring,extrapol   )
end subroutine gettabledata_vector_sp

subroutine gettabledata_vector_hp(handle     ,ivec       ,values     , &
                             & timhr      ,refjulday  ,errorstring, extrapol_in)
!
! Global variables
!
    integer, dimension(4)               :: ivec
    integer                ,intent(in)  :: refjulday
    real(hp), optional     ,intent(in)  :: extrapol_in
    real(hp)               ,intent(in)  :: timhr
    real(hp), dimension(:) ,intent(out) :: values
    character(256)         ,intent(out) :: errorstring
    type(handletype)       ,intent(in)  :: handle
!
! Local variables
!
    real(hp)                  :: extrapol
!
!! executable statements -------------------------------------------------------
!
    if (present(extrapol_in)) then
       extrapol = extrapol_in
    else
       extrapol = 0.0_hp
    endif
    call gettabledata_scalar_hp(handle     ,ivec(1)    ,ivec(2)    , &
                 & ivec(3)    ,ivec(4)    ,values     ,timhr      , &
                 & refjulday  ,errorstring,extrapol   )
end subroutine gettabledata_vector_hp


subroutine gettabledata_scalar_sp(handle     ,itable     ,ipar       , &
                    & npar       ,irec       ,values     ,timhr      , &
                    & refjulday  ,errorstring,extrapol_in)
!
! Global variables
!
    integer                ,intent(in)  :: itable
    integer                ,intent(in)  :: ipar
    integer                             :: irec
    integer                ,intent(in)  :: npar
    integer                ,intent(in)  :: refjulday
    real(sp), optional     ,intent(in)  :: extrapol_in
    real(sp)               ,intent(in)  :: timhr
    real(sp), dimension(:) ,intent(out) :: values
    character(256)         ,intent(out) :: errorstring
    type(handletype)       ,intent(in)  :: handle
!
! Local variables
!
    integer                             :: i
    integer                             :: istat
    real(fp)                            :: extrapol
    real(fp)                            :: timhr_fp
    real(fp), dimension(:), allocatable :: values_fp
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'GetTableData call before initialisation'
    else
       if (present(extrapol_in)) then
          extrapol = real(extrapol_in,fp)
       else
          extrapol = 0.0_fp
       endif
       timhr_fp = real(timhr,fp)
       allocate(values_fp(size(values)), stat = istat)
       if (istat /= 0) then
          errorstring = 'gettabledata_scalar: Memory allocation error'
          return
       endif
       do i=1,size(values)
          values_fp(i) = real(values(i),fp)
       enddo
       call org_gettabledata(tablehandle%this       ,itable     ,ipar       , &
                           & npar       ,irec       ,values_fp  ,timhr_fp   , &
                           & refjulday  ,errorstring,extrapol   )
       do i=1,size(values)
          values(i) = real(values_fp(i),hp)
       enddo
       deallocate(values_fp, stat = istat)
    endif
    !
end subroutine gettabledata_scalar_sp


subroutine gettabledata_scalar_hp(handle     ,itable     ,ipar       , &
                    & npar       ,irec       ,values     ,timhr      , &
                    & refjulday  ,errorstring,extrapol_in)
!
! Global variables
!
    integer                ,intent(in)  :: itable
    integer                ,intent(in)  :: ipar
    integer                             :: irec
    integer                ,intent(in)  :: npar
    integer                ,intent(in)  :: refjulday
    real(hp), optional     ,intent(in)  :: extrapol_in
    real(hp)               ,intent(in)  :: timhr
    real(hp), dimension(:) ,intent(out) :: values
    character(256)         ,intent(out) :: errorstring
    type(handletype)       ,intent(in)  :: handle
!
! Local variables
!
    integer                             :: i
    integer                             :: istat
    real(fp)                            :: extrapol
    real(fp)                            :: timhr_fp
    real(fp), dimension(:), allocatable :: values_fp
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'GetTableData call before initialisation'
    else
       if (present(extrapol_in)) then
          extrapol = real(extrapol_in,fp)
       else
          extrapol = 0.0_fp
       endif
       timhr_fp = real(timhr,fp)
       allocate(values_fp(size(values)), stat = istat)
       if (istat /= 0) then
          errorstring = 'gettabledata_scalar: Memory allocation error'
          return
       endif
       do i=1,size(values)
          values_fp(i) = real(values(i),fp)
       enddo
       call org_gettabledata(tablehandle%this       ,itable     ,ipar       , &
                           & npar       ,irec       ,values_fp  ,timhr_fp   , &
                           & refjulday  ,errorstring,extrapol   )
       do i=1,size(values)
          values(i) = real(values_fp(i),hp)
       enddo
       deallocate(values_fp, stat = istat)
    endif
    !
end subroutine gettabledata_scalar_hp


subroutine checktable(handle    ,itable    ,ipar      , &
                    & npar      ,chktyp    ,errorstring       )
!
! Global variables
!
    integer                      ,intent(in)  :: itable
    integer                      ,intent(in)  :: ipar
    integer                      ,intent(in)  :: npar
    integer                      ,intent(in)  :: chktyp
    character(256)         ,intent(out) :: errorstring
    type(handletype)             ,intent(in)  :: handle
!
! Local variables
!
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'CheckTable call before initialisation'
    else
       call org_checktable(tablehandle%this        ,itable    ,ipar      , &
                         & npar      ,chktyp    ,errorstring)
    endif
    !
end subroutine checktable


subroutine checktableparnames(handle    ,parnames  ,itable    , &
                            & ipar      ,npar      ,errorstring       )
!
! Global variables
!
    integer                      ,intent(in)  :: itable
    integer                      ,intent(in)  :: ipar
    integer                      ,intent(in)  :: npar
    character(*), dimension(npar),intent(in)  :: parnames
    character(256)               ,intent(out) :: errorstring
    type(handletype)             ,intent(in)  :: handle
!
! Local variables
!
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'CheckTableParNames call before initialisation'
    else
       call org_checktableparnames(tablehandle%this        ,parnames  ,itable    , &
                                 & ipar      ,npar      ,errorstring)
    endif
    !
end subroutine checktableparnames


character(256) function getfilename(handle    )
!
! Global variables
!
    type(handletype)             ,intent(in)  :: handle
!
! Local variables
!
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle,getfilename)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       getfilename = 'NOT YET INITIALISED'
    else
       getfilename = org_getfilename(tablehandle%this)
    endif
    !
end function getfilename


integer function getntables(handle  ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Get the number of tables
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(handletype)             ,intent(in)  :: handle
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'GetNTables call before initialisation'
    else
       getntables = org_getntables(tablehandle%this ,errorstring)
    endif
end function getntables


character(MAXTABLECLENGTH) function gettablelocation(handle  ,itable     ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Get the number of tables
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(handletype)             ,intent(in)  :: handle
    integer                      ,intent(in)  :: itable
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'GetTableLocation call before initialisation'
    else
       gettablelocation = org_gettablelocation(tablehandle%this ,itable, errorstring)
    endif
end function gettablelocation


integer function gettablentimes(handle  ,itable     ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Get the number of tables
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(handletype)             ,intent(in)  :: handle
    integer                      ,intent(in)  :: itable
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'GetTableNTimes call before initialisation'
    else
       gettablentimes = org_gettablentimes(tablehandle%this ,itable, errorstring)
    endif
end function gettablentimes


subroutine newtablehandle(tablehandle, errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Create a new table handle object
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(tablefiletypehandle) ,intent(out) :: tablehandle
    character(256)            ,intent(out) :: errorstring
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    errorstring = ''
    if (htype<0) then
       htype = registerhandletype('TableFile')
       if (htype<0) then
          errorstring = 'Unable to register TableFile handle'
          return
       endif
    endif
    nullify(tablehandle%this)
    tablehandle%htype = htype
end subroutine newtablehandle


logical function validtable(handle, errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Verify whether handle is a tablefiletypehandle
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(handletype)                     :: handle
    character(256),intent(out), optional :: errorstring
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    if (htype<0) then
       validtable = .false.
    else
       validtable = handle%htype == htype
    endif
    if (present(errorstring)) then
       if (.not.validtable) then
          errorstring = 'INVALID TableFile handle'
       else
          errorstring = ''
       endif
    endif
end function validtable


function cast_to_tablehandle(handle) result (tablehandle)
!!--description-----------------------------------------------------------------
!
!    Function: Cast from handle to tablefiletypehandle
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(handletype)                     :: handle
    type(tablefiletypehandle)            :: tablehandle
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    tablehandle = transfer(handle,tablehandle)
end function cast_to_tablehandle


function cast_from_tablehandle(tablehandle) result (handle)
!!--description-----------------------------------------------------------------
!
!    Function: Cast from tablefiletypehandle to handle
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(handletype)                     :: handle
    type(tablefiletypehandle)            :: tablehandle
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    handle = transfer(tablehandle,handle)
end function cast_from_tablehandle

end module table_handles
