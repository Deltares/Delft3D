subroutine readPOLY(lunscr, gdp)
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2013.
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
!  $Id: READpoly.f90 6742 2016-11-09 16:50:58Z canestrelli.x $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_banks/READpoly.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: READ polygons for PLIC bank reconstruction (Piecewise Linear Interface Construction) 
!
! Method used:
!
! Written by: Alberto Canestrelli
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer               , pointer :: analyticalPOLY
    integer               , pointer :: NvertTOT
    integer               , pointer :: Npoly
    integer, dimension(:) , pointer :: nvert
    integer, dimension(:) , pointer :: progVERT
    real(fp), dimension(:), pointer :: vertx
    real(fp), dimension(:), pointer :: verty
    character(255)        , pointer :: filcc_in
!
! Global variables
!  
  integer                                                             , intent(in)    :: lunscr   !  Description and declaration in iidim.f90
!
!
! Local variables
!
  integer cont,Np,Nv
  character*100 buttaC
!
! executable statements -------------------------------------------------------
!        
    analyticalPOLY => gdp%gdimbound%analyticalPOLY
    NvertTOT       => gdp%gdimbound%NvertTOT
    Npoly          => gdp%gdimbound%Npoly
    filcc_in       => gdp%gdimbound%filcc_in
    OPEN(16,file = filcc_in)
    READ(16,'(a1,i4)') buttaC,Npoly
    allocate (gdp%gdimbound%progVERT(Npoly+1))
    allocate (gdp%gdimbound%Nvert(Npoly))
    allocate (gdp%gdimbound%vertx(Npoly*100000))
    allocate (gdp%gdimbound%verty(Npoly*100000))
    !
    nvert          => gdp%gdimbound%nvert
    progVERT       => gdp%gdimbound%progVERT
    vertx          => gdp%gdimbound%vertx
    verty          => gdp%gdimbound%verty
    !
    if (Npoly/=0) then
       READ(16,*)  
       READ(16,*)
       READ(16,*)
       cont = 0
    endif
    DO Np=1,Npoly
       READ(16,*)
       READ(16,*) Nvert(Np)
       if (Nvert(Np).le.2) then
          write(lunscr,*) 'The polygon has to be made by more than 2 vertices.' 
          call d3stop(1, gdp)
       endif
       DO Nv=1,Nvert(Np)
          cont = cont + 1
          READ(16,*) vertx(cont),verty(cont)
       ENDDO
    ENDDO
    CLOSE(16)
!
    IF (Npoly.GT.0) THEN
       progVERT(1) = 0
       cont = 1
       DO Np=1,Npoly
         cont = cont+1
         progVERT(cont) = progVERT(cont-1) + nvert(Np) 
       ENDDO
       NvertTOT = progVERT(Npoly) + nvert(Npoly) 
    !ELSE
    !   WRITE(*,*) 'No bank polygon provided in the bank file' 
    !   call d3stop(1, gdp)    
    ENDIF
    if (analyticalPOLY>0) THEN
       SELECT CASE (analyticalPOLY)   
       CASE(1) ! circle
          Npoly = 2
       CASE DEFAULT
          write(*,*) 'analyticalPOLY has a not admitted value'
          call d3stop(1, gdp)
       END SELECT       
    endif
!
RETURN
END
