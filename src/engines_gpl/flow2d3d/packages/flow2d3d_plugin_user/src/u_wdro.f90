subroutine u_wdro(lundat    ,first     ,header    ,runid     ,itime     , &
                & idate     ,timnow    ,notim     ,idro      ,nodro     , &
                & xydro     ,namdro    )
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
! Example user routine to write drogue data for a
! specific drogue with the sequence number IDRO to a
! user defined output file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer                        , intent(in) :: idate
    integer                        , intent(in) :: idro
    integer                        , intent(in) :: itime
    integer                        , intent(in) :: lundat
    integer                        , intent(in) :: nodro
    integer                        , intent(in) :: notim
    logical                        , intent(in) :: first
    real(fp)                       , intent(in) :: timnow
    real(fp), dimension(2, nodro)      , intent(in) :: xydro !  Description and declaration in esm_alloc_real.f90
    character(*)                   , intent(in) :: runid
    character(131), dimension(10)               :: header !  Description and declaration in postpr.igs
    character(20), dimension(nodro), intent(in) :: namdro !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer :: i
!
!! executable statements -------------------------------------------------------
!
    !
    ! Header; write for the first time
    !
    if (first) then
       header(1) = '* output for drogue: ' // namdro(idro)
       header(2) = '* simulation results from run:' // runid
       header(3) = '* column    1 : x-coor. particle (m)'
       header(4) = '* column    2 : y-coor. particle (m)'
       header(5) = '* column    3 : elapsed time (minutes)'
       header(6) = '* column    4 : date(yyyymmdd)'
       header(7) = '* column    5 : time(dummy)'
       header(8) = 'P001 Drogue coord.  ' // namdro(idro) // ' run: ' // runid
       do i = 1, 8
          write (lundat, '(  a)') header(i)
       enddo
       write (lundat, '(2i8)') notim, 5
    endif
    !
    ! write values to tekal-data file
    !
    write (lundat, '(2(f11.3,1x),i8.8,1x,i6.6,1x,f11.3)') &
        & xydro(1, idro), xydro(2, idro), idate, itime, timnow
end subroutine u_wdro
