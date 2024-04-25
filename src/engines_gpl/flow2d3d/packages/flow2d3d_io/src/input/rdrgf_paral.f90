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
module m_rdrgf_paral

private

!
! functions and subroutines
!
public rdrgf_paral
    
contains
    
!> Read the grid point coordinates from a curvi-linear grd-file and distribute.
subroutine rdrgf_paral(filrgf    ,lundia    ,error     ,nmax      ,mmax      , &
                     & xcor      ,ycor      ,sferic    ,gdp       )
    use precision
    use globaldata
    use dfparall, only: inode, master, dfint, dfloat
    use m_rdrgf, only: rdrgf
    ! 
    implicit none 
    ! 
    type(globdat),target :: gdp 
    ! 
    ! The following list of pointer parameters is used to point inside the gdp structure 
    ! 
    integer, pointer :: mfg
    integer, pointer :: mlg
    integer, pointer :: nfg
    integer, pointer :: nlg
    integer, pointer :: mmaxgl
    integer, pointer :: nmaxgl
! 
! Subroutine arguments
! 
    integer                                                                                                             :: lundia !< unit number for diagnostic output
    integer                                                                                               , intent(in)  :: mmax   !< size of partition in M-dir.
    integer                                                                                               , intent(in)  :: nmax   !< size of partition in N-dir.
    logical                                                                                                             :: error  !< flag=TRUE if an error is encountered 
    logical                                                                                               , intent(out) :: sferic !< flag=TRUE if grid in spherical coordinates
    real(fp)    , dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)              :: xcor   !< x-coordinates of the grid points
    real(fp)    , dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)              :: ycor   !< y-coordinates of the grid points
    character(*)                                                                                                        :: filrgf !< name of the grid file
! 
! Local variables 
! 
    integer, dimension(1)                 :: ival     !< temporary array for broadcasting error/spherical flag
    integer                               :: m        !< loop index
    integer                               :: n        !< loop index
    real(fp), dimension(:,:), allocatable :: xtmp     !< temporary array containing xcor of entire domain 
    real(fp), dimension(:,:), allocatable :: ytmp     !< temporary array containing ycor of entire domain 
! 
!! executable statements ------------------------------------------------------- 
! 
    mfg    => gdp%gdparall%mfg 
    mlg    => gdp%gdparall%mlg 
    nfg    => gdp%gdparall%nfg 
    nlg    => gdp%gdparall%nlg 
    nmaxgl => gdp%gdparall%nmaxgl 
    mmaxgl => gdp%gdparall%mmaxgl

    ! initialize local parameters 
    sferic = .false.
    error  = .false.

    ! allocate temporary arrays to store coordinates
    allocate(xtmp(nmaxgl,mmaxgl), ytmp(nmaxgl,mmaxgl)) 
    xtmp(:,:) = 0.0_fp
    ytmp(:,:) = 0.0_fp

    ! the master opens and reads the grid file 
    if ( inode == master ) then
        call rdrgf(filrgf, lundia, error, nmaxgl, mmaxgl, xtmp, ytmp, sferic)

        ! fill ival on master thread
        if (error) then
           ival(1) = -1
        elseif (sferic) then 
           ival(1) = 1
        else 
           ival(1) = 0
        endif 
    endif

    ! scatter integer array to all nodes
    call dfbroadc_gdp( ival, 1, dfint, gdp ) 
    if (ival(1) == -1) then
        ! error signal from master thread
        error = .true.
        if (inode /= master) then
            call prterr(lundia, 'P004', 'Error while reading grid by master thread.')
        endif
        
    else
        ! unpack ival on all threads
        sferic = (ival(1) == 1) 
        if (sferic) then 
           write(lundia, *) 
           write(lundia, '(a)') 'Coordinate System: Spherical' 
           write(lundia, *) 
        endif 
        
        ! scatter arrays xtmp and ytmp to all nodes 
        call dfbroadc_gdp( xtmp, nmaxgl*mmaxgl, dfloat, gdp ) 
        call dfbroadc_gdp( ytmp, nmaxgl*mmaxgl, dfloat, gdp ) 
        call dfsync( gdp ) 
        
        ! copy parts of xcor, ycor for each subdomain 
        do m = mfg, mlg 
           do n = nfg, nlg 
              xcor(n-nfg+1,m-mfg+1) = xtmp(n,m) 
              ycor(n-nfg+1,m-mfg+1) = ytmp(n,m) 
           enddo 
        enddo 
    endif
    
    deallocate(xtmp,ytmp)
end subroutine rdrgf_paral

end module m_rdrgf_paral