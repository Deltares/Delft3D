subroutine init_out_heatfluxes(lundia, ktemp, keva, nostat, gdp)
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp 
    !
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    type (flwoutputtype)                 , pointer :: flwoutput
    real(fp), dimension(:)               , pointer :: qeva_out   
    real(fp), dimension(:)               , pointer :: qco_out   
    real(fp), dimension(:)               , pointer :: qbl_out    
    real(fp), dimension(:)               , pointer :: qin_out    
    real(fp), dimension(:)               , pointer :: qnet_out   
    real(fp), dimension(:)               , pointer :: hlc_out    
    real(fp), dimension(:)               , pointer :: hfree_out  
    real(fp), dimension(:)               , pointer :: efree_out
    real(fp), dimension(:)               , pointer :: qmis_out
    real(fp), dimension(:)               , pointer :: zqeva_out  
    real(fp), dimension(:)               , pointer :: zqco_out  
    real(fp), dimension(:)               , pointer :: zqbl_out   
    real(fp), dimension(:)               , pointer :: zqin_out   
    real(fp), dimension(:)               , pointer :: zqnet_out  
    real(fp), dimension(:)               , pointer :: zhlc_out   
    real(fp), dimension(:)               , pointer :: zhfree_out 
    real(fp), dimension(:)               , pointer :: zefree_out
    real(fp), dimension(:)               , pointer :: zqmis_out
    logical                              , pointer :: free_convec
!
! Global variables
!
    integer                                                     , intent(in)  :: keva   !  Description and declaration in tricom.igs
    integer                                                     , intent(in)  :: ktemp  !  Description and declaration in tricom.igs
    integer                                                     , intent(in)  :: lundia !  Description and declaration in inout.igs
    integer                                                     , intent(in)  :: nostat !  Description and declaration in dimens.igs
!
! Local variables
!
    integer  :: istat
    !
!
!! executable statements -------------------------------------------------------
!
    flwoutput           => gdp%gdflwpar%flwoutput
    free_convec         => gdp%gdheat%free_convec
    qeva_out            => gdp%gdheat%qeva_out
    qco_out             => gdp%gdheat%qco_out
    qbl_out             => gdp%gdheat%qbl_out
    qin_out             => gdp%gdheat%qin_out!
    qnet_out            => gdp%gdheat%qnet_out
    hlc_out             => gdp%gdheat%hlc_out!
    hfree_out           => gdp%gdheat%hfree_out
    efree_out           => gdp%gdheat%efree_out
    qmis_out            => gdp%gdheat%qmis_out
    zqeva_out           => gdp%gdheat%zqeva_out
    zqco_out            => gdp%gdheat%zqco_out
    zqbl_out            => gdp%gdheat%zqbl_out
    zqin_out            => gdp%gdheat%zqin_out!
    zqnet_out           => gdp%gdheat%zqnet_out
    zhlc_out            => gdp%gdheat%zhlc_out
    zhfree_out          => gdp%gdheat%zhfree_out
    zefree_out          => gdp%gdheat%zefree_out
    zqmis_out           => gdp%gdheat%zqmis_out
    !
    ! Only when using a heat flux model with requested output of heat fluxes
    !
    if (ktemp > 0 .and. flwoutput%temperature) then
       !
       istat = 0
       !
       ! Allocate and initialize output arrays needed for writing to output files
       !
       if (.not. associated(gdp%gdheat%qeva_out)) then
          !
          ! Arrays for writing to MAP-file
          !
          allocate (gdp%gdheat%qeva_out (gdp%d%nmlb:gdp%d%nmub) , stat = istat)
          if (istat==0) allocate (gdp%gdheat%qco_out  (gdp%d%nmlb:gdp%d%nmub) , stat = istat)
          if (istat==0) allocate (gdp%gdheat%qbl_out  (gdp%d%nmlb:gdp%d%nmub) , stat = istat)
          if (istat==0) allocate (gdp%gdheat%qin_out  (gdp%d%nmlb:gdp%d%nmub) , stat = istat)
          if (istat==0) allocate (gdp%gdheat%qnet_out (gdp%d%nmlb:gdp%d%nmub) , stat = istat)
          !
          ! Arrays for writing to HIS-file
          !
          if (istat==0) allocate (gdp%gdheat%zqeva_out (nostat) , stat = istat)
          if (istat==0) allocate (gdp%gdheat%zqco_out  (nostat) , stat = istat)
          if (istat==0) allocate (gdp%gdheat%zqbl_out  (nostat) , stat = istat)
          if (istat==0) allocate (gdp%gdheat%zqin_out  (nostat) , stat = istat)
          if (istat==0) allocate (gdp%gdheat%zqnet_out (nostat) , stat = istat)
          !
          if (ktemp == 3) then
             if (istat==0) allocate (gdp%gdheat%hlc_out  (gdp%d%nmlb:gdp%d%nmub) , stat = istat)
             if (istat==0) allocate (gdp%gdheat%zhlc_out (nostat) , stat = istat)
          endif
          if (free_convec) then
             if (istat==0) allocate (gdp%gdheat%hfree_out(gdp%d%nmlb:gdp%d%nmub) , stat = istat)
             if (istat==0) allocate (gdp%gdheat%efree_out(gdp%d%nmlb:gdp%d%nmub) , stat = istat)
             !
             if (istat==0) allocate (gdp%gdheat%zhfree_out(nostat) , stat = istat)
             if (istat==0) allocate (gdp%gdheat%zefree_out(nostat) , stat = istat)
          endif
          if (keva == 3) then
             if (istat==0) allocate (gdp%gdheat%qmis_out  (gdp%d%nmlb:gdp%d%nmub) , stat = istat)
             if (istat==0) allocate (gdp%gdheat%zqmis_out (nostat) , stat = istat)
          endif
          !
          if (istat /= 0) then
             call prterr(lundia, 'U021', 'INI_OUT_HEATFLUXES: memory allocation error')
             call d3stop(1, gdp)
          endif
          !
          ! define pointers again to update references; initialize the arrays
          !
          qeva_out    => gdp%gdheat%qeva_out
          qco_out     => gdp%gdheat%qco_out
          qbl_out     => gdp%gdheat%qbl_out
          qin_out     => gdp%gdheat%qin_out
          qnet_out    => gdp%gdheat%qnet_out
          zqeva_out   => gdp%gdheat%zqeva_out
          zqco_out    => gdp%gdheat%zqco_out
          zqbl_out    => gdp%gdheat%zqbl_out
          zqin_out    => gdp%gdheat%zqin_out
          zqnet_out   => gdp%gdheat%zqnet_out
          qeva_out  = 0.0_fp
          qco_out   = 0.0_fp
          qbl_out   = 0.0_fp
          qin_out   = 0.0_fp
          qnet_out  = 0.0_fp
          zqeva_out  = 0.0_fp
          zqco_out   = 0.0_fp
          zqbl_out   = 0.0_fp
          zqin_out   = 0.0_fp
          zqnet_out  = 0.0_fp
          if (ktemp == 3) then
             hlc_out    => gdp%gdheat%hlc_out
             zhlc_out   => gdp%gdheat%zhlc_out
             hlc_out    = 0.0_fp
             zhlc_out   = 0.0_fp
          endif
          if (free_convec) then
             hfree_out  => gdp%gdheat%hfree_out
             efree_out  => gdp%gdheat%efree_out
             zhfree_out => gdp%gdheat%zhfree_out
             zefree_out => gdp%gdheat%zefree_out
             hfree_out  = 0.0_fp
             efree_out  = 0.0_fp
             zhfree_out = 0.0_fp
             zefree_out = 0.0_fp
          endif
          if (keva == 3) then
             qmis_out   => gdp%gdheat%qmis_out
             zqmis_out  => gdp%gdheat%zqmis_out
             qmis_out   = 0.0_fp
             zqmis_out  = 0.0_fp
          endif
       endif
    endif
    !
end subroutine init_out_heatfluxes
