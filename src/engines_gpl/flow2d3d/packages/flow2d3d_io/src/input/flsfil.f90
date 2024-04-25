subroutine flsfil(lundia    ,error     ,filfls    ,mmax      ,nmax      , &
                & nmaxus    ,kmax      ,istruc    ,kspu      ,kspv      , &
                & pship     ,gdp       )
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
!    Function: Reads the floating structure depth values from
!              the attribute file
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
    real(fp) , pointer :: eps
!
! Global variables
!
    integer                                                             , intent(in)  :: istruc !!  =+/-3: local weir
                                                                                                !!  =+/-4: gate
                                                                                                !!  =+/-5: rigid sheets
                                                                                                !!  =+/-6: porous plate
                                                                                                !!  =+/-7: bridge
                                                                                                !!  =+/-9: 2d weir
    integer                                                             , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: lundia !  Description and declaration in inout.igs
    integer                                                             , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), intent(out) :: kspu   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), intent(out) :: kspv   !  Description and declaration in esm_alloc_int.f90
    logical                                                             , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                     :: pship  !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                      :: filfls !!  Name of the relevant file
!
! Local variables
!
    integer           :: iocond   ! Help var. for iostat condition 
    integer           :: lfile    ! Length of file name 
    integer           :: luntmp   ! Unit number for attribute file 
    integer           :: m        ! Help loop var. 
    integer           :: md
    integer           :: n        ! Help loop var. 
    integer           :: nd
    integer, external :: newlun
    character(11)     :: fmttmp   ! Format file ('formatted  ')
    character(300)    :: message
!
!! executable statements -------------------------------------------------------
!
    eps  => gdp%gdconst%eps
    !
    fmttmp = 'formatted'
    !
    call depfil(lundia    ,error     ,trim(filfls)         ,fmttmp    , &
              & pship     ,1         ,1         ,gdp%griddim)
    if (error) then
       write (message,'(3a,i2,2a)') 'error reading file ', trim(filfls)
       call prterr(lundia, 'U021', trim(message))
       call d3stop(1, gdp)          
    endif
    !
    ! Check for ships and fill arrays
    ! Only KSPU/V(NM,0) is essential.
    !
    do n = 1, nmaxus
       do m = 1, mmax
          if (pship(n, m)>eps) then
             md            = max(1,m-1)
             nd            = max(1,n-1)
             kspu(n ,m ,0) = istruc
             kspu(n ,md,0) = istruc
             kspv(n ,m ,0) = istruc
             kspv(nd,m ,0) = istruc
          endif
       enddo
    enddo
end subroutine flsfil
