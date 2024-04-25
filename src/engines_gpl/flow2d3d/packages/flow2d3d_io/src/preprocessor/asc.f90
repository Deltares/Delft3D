subroutine asc(w         ,v0u       ,fr        ,kcmp      ,inaam     , &
             & jdate    ,ierrs     ,gdp       )
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
!    Function: Determination of FR and V0+U
!              'stripped' VERSION OF MAIN (ASCON)
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'pardef.igd'
!
! Global variables
!
    integer                         :: ierrs  !!  Number of error messages
    integer                         :: kcmp
    integer     , dimension(6)      :: jdate  !!  Date and time
    character(8), dimension(0:kcmp) :: inaam  !!  Name of the referenced components
    real(hp)    , dimension(kcmp)   :: fr     !!  Amplitude factors for the referenced components
    real(hp)    , dimension(kcmp)   :: v0u    !!  Astronomical arguments of the referenced components
    real(hp)    , dimension(kcmp)   :: w      !!  Angular velocity of the referenced components
!
!
! Local variables
!
    integer                           :: i      ! Help var. 
    integer                           :: ik     ! Help var. 
    integer                           :: il     ! Help var. 
    integer                           :: j      ! Help var. 
    integer                           :: jaar   ! Present year 
    integer      , dimension(16*mxkc) :: jnaam  ! Help var. 
    character(8) , dimension(mxkc)    :: knaam  ! Array with the names of all components 
    character(80), dimension(mxkc)    :: kombes ! Array with tidal components 
    real(hp)                          :: t      ! Time in hours referred to January 1, 00:00 of the year 'JAAR' 
    real(hp)     , dimension(15)      :: v      ! Help var. to calculate V0U() 
    real(hp)     , dimension(25)      :: f      ! Help var. to calculate FR() 
!
!
!! executable statements -------------------------------------------------------
!
    !
    ! Read C0021KOMPBES
    !
    call kompbs(kombes)
    !
    ik = -15
    do i = 1, mxkc
       ik = ik + 16
       il = ik + 15
       read (kombes(i), '(a8,10i3,3(i1,i2))') knaam(i), (jnaam(j), j = ik, il)
    enddo
    !
    ! Loop over the provided times
    !
    jaar = jdate(1)
    !
    call datumi(jaar      ,jdate     ,t         )
    call hulpgr(jaar      ,t         ,v         ,f         )
    call bewvuf(ierrs     ,kcmp      ,mxkc      ,inaam     ,knaam     , &
              & jnaam     ,w         ,v0u       ,fr        ,v         , &
              & f         )
end subroutine asc
