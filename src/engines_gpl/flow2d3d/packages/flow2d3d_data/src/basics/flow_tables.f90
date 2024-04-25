module flow_tables
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
!
! Delft3D-FLOW interface for tables module.
! This interface includes GDP and error handling.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use table_handles
    use handles
!
!! -----------------------------------------------------------------------------
!
    private
    public flw_readtable
    public flw_gettable
    public flw_gettabledata
    public flw_checktable
    public flw_checktableparnames
    !
    public cleartable
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

contains

subroutine flw_readtable(handle, filnam, refjulday, gdp)
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
    integer            ,intent(in)  :: refjulday
    character(*)       ,intent(in)  :: filnam
    type(handletype)                :: handle
!
! Local variables
!
    integer, external               :: newlun
    character(256)                  :: errorstring
!
!! executable statements -------------------------------------------------------
!
    call readtable(handle, filnam, refjulday, errorstring)
    call errorhandler(errorstring,gdp)
    !
end subroutine flw_readtable


subroutine flw_gettable(handle    ,location  ,parname   ,itable    , &
                      & ipar      ,npar      ,nparmin   ,gdp)
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer                ,intent(out) :: itable
    integer                ,intent(out) :: ipar
    integer                ,intent(out) :: npar
    integer                ,intent(in)  :: nparmin
    character(*)           ,intent(in)  :: location
    character(*)           ,intent(in)  :: parname
    type(handletype)       ,intent(in)  :: handle
!
! Local variables
!
    character(256)                      :: errorstring
!
!! executable statements -------------------------------------------------------
!
    call gettable(handle      ,location  ,parname   , &
                & itable      ,ipar      ,npar      ,nparmin   , &
                & errorstring)
    call errorhandler(errorstring,gdp)
    !
end subroutine flw_gettable


subroutine flw_gettabledata(handle     ,itable     ,ipar       , &
              & npar       ,irec       ,values     ,timhr      , &
              & refjulday  ,gdp        )
    use globaldata
    !
    implicit none
    !
    type(globdat),target   :: gdp
    real(fp)     , pointer :: hdt
!
! Global variables
!
    integer                ,intent(in)  :: itable
    integer                ,intent(in)  :: ipar
    integer                             :: irec
    integer                ,intent(in)  :: npar
    integer                ,intent(in)  :: refjulday
    real(fp)               ,intent(in)  :: timhr
    real(fp), dimension(:) ,intent(out) :: values
    type(handletype)       ,intent(in)  :: handle
!
! Local variables
!
    character(256) :: errorstring
    real(fp)       :: extrapol     ! Time interval after T_end for which extrapolation is used in hours
!
!! executable statements -------------------------------------------------------
!
    hdt => gdp%gdnumeco%hdt
    !
    ! extrapol [hr] = 2 * hdt [sec] / 3600
    !
    extrapol = hdt / 1800.0_fp
    call gettabledata(handle     ,itable     ,ipar       , &
                    & npar       ,irec       ,values     ,timhr      , &
                    & refjulday  ,errorstring,extrapol)
    call errorhandler(errorstring,gdp)
    !
end subroutine flw_gettabledata


subroutine flw_checktable(handle    ,itable    ,ipar      , &
                        & npar      ,chktyp    ,gdp       )
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer                      ,intent(in)  :: itable
    integer                      ,intent(in)  :: ipar
    integer                      ,intent(in)  :: npar
    integer                      ,intent(in)  :: chktyp
    type(handletype)             ,intent(in)  :: handle
!
! Local variables
!
    character(256)                      :: errorstring
!
!! executable statements -------------------------------------------------------
!
    call checktable(handle    ,itable    ,ipar      , &
                  & npar      ,chktyp    ,errorstring)
    call errorhandler(errorstring,gdp)
    !
end subroutine flw_checktable


subroutine flw_checktableparnames(handle    ,parnames  ,itable    , &
                                & ipar      ,npar      ,gdp       )
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer                      ,intent(in)  :: itable
    integer                      ,intent(in)  :: ipar
    integer                      ,intent(in)  :: npar
    character(*), dimension(npar),intent(in)  :: parnames
    type(handletype)             ,intent(in)  :: handle
!
! Local variables
!
    character(256)                      :: errorstring
!
!! executable statements -------------------------------------------------------
!
    call checktableparnames(handle    ,parnames  ,itable     , &
                          & ipar      ,npar      ,errorstring)
    call errorhandler(errorstring,gdp)
    !
end subroutine flw_checktableparnames


subroutine errorhandler(errorstring,gdp)
!!--description-----------------------------------------------------------------
!
!    Function: Deallocate data object when needed
!
!!------------------------------------------------------------------------------
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    integer , pointer :: lundia
!
! Global variables
!
    character(*)                         :: errorstring
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    lundia  => gdp%gdinout%lundia
    if (errorstring /= ' ') then
       call prterr(lundia, 'U021', errorstring)
       call d3stop(1, gdp)
    endif
end subroutine errorhandler

end module flow_tables
