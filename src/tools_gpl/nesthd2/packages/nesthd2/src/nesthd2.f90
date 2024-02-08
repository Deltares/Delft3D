program nesthd2
    use nesthd2_version_module
    implicit none
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
!***********************************************************************
! Deltares                         marine and coastal management
!
! program            : nesthd2
! version            : v1.4
! date               : January   1998
! version            : 1.50.01 - AVUNDF PC underflow
!                                SECURE without license check
! programmer         : Theo van der Kaaij
!
! function           :    Determine time series boundary conditions
!                         for water levels, velocities and/or
!                         constituents.
!                         This program uses one real, one integer
!                         and one character arry to limit memory
!                         requirements. This main program only
!                         computes pointers and calls the main
!                         module.
! notes              : 1. Assumed is a layer distribution which is
!                         equal in both the overall and the detailed
!                         model.
!                      2. Assumed are identical constituents in both
!                         models
!
! changes v1.3       : Get all the additional information for nesting
!                      at the beginning of a session
!***********************************************************************
      integer       lun   (  5   )
    
      integer, dimension(:,:), pointer :: kfs
      integer, dimension(:,:), pointer :: mcbsp
      integer, dimension(:,:), pointer :: ncbsp
      integer, dimension(:,:), pointer :: mnstat

      double precision, dimension(:)        , pointer :: thick
      double precision, dimension(:,:)      , pointer :: wl
      double precision, dimension(:,:,:)    , pointer :: uu
      double precision, dimension(:,:,:)    , pointer :: vv
      double precision, dimension(:,:,:,:)  , pointer :: conc
      double precision, dimension(:)        , pointer :: angle
      double precision, dimension(:,:,:,:,:), pointer :: bndva
      
      character(len= 1), dimension(:), pointer :: typbn
      character(len=20), dimension(:), pointer :: nambn
      character(len=20), dimension(:), pointer :: namco

      character*(256) extnef
      character*  63  verid
      character*   6  pntnam

      logical       fout
      character*80  CIDENT
      integer       status
!
      integer :: length
      integer :: notims, nostat, kmax, lstci, nobnd, mincon

      cident = ' '
      call getfullversionstring_nesthd2(cident)
!-----------------------------------------------------------------------
!---- 1. Open all files
!-----------------------------------------------------------------------

      length = len_trim(cident)
      write (*     ,'(/, 2a)') ' ', trim(cident)

      call opnfl2(lun   ,extnef)

      write (lun(5),'(/, 2a)') ' ', trim(cident)
      write (lun(5),*) ' '

!-----------------------------------------------------------------------
!---- 2. Get dimensions
!-----------------------------------------------------------------------

      call getdim(lun(5),lun(1),fout  ,extnef,notims,nostat, &
     &            kmax  ,lstci ,nobnd                      )
      if (fout) goto 999
      mincon = max(lstci,1)

!-----------------------------------------------------------------------
!---- 3.1 Determine integer   pointers
!-----------------------------------------------------------------------

      allocate (kfs   (nostat, notims))
      allocate (mcbsp (nobnd ,2))
      allocate (ncbsp (nobnd ,2))
      allocate (mnstat(2,nostat))

!-----------------------------------------------------------------------
!---- 3.2 Determine real pointers
!-----------------------------------------------------------------------

      allocate (thick(kmax))
      allocate (wl   (nostat, notims))
      allocate (uu   (nostat, kmax, notims))
      allocate (vv   (nostat, kmax, notims))
      allocate (conc (nostat, kmax, notims, mincon))
      allocate (angle(nostat))
      allocate (bndva(nobnd, notims, kmax, mincon, 2))

!-----------------------------------------------------------------------
!     3.3 Determine character  pointers
!-----------------------------------------------------------------------

      allocate (typbn(nobnd))
      allocate (nambn(nobnd)) ! each 20 characters long
      allocate (namco(lstci+2)) ! each 20 characters long

!-----------------------------------------------------------------------
!---- 4.  Call main module
!-----------------------------------------------------------------------

      call nest_hd2  (lun   , extnef, nostat , notims, kmax  , &
                      lstci , nobnd , mincon , &
                      thick , wl    , uu     , vv    , conc, &
                      angle , bndva , &
                      kfs   , mcbsp , ncbsp , mnstat , &
                      typbn , nambn , namco                    )

!-----------------------------------------------------------------------
!---- end program
!-----------------------------------------------------------------------

  999 continue

      call clsfil(lun   ,5     )
      write(*,'( a)') 'Normal end of program'
      stop

  900 write(lun(5),'(//a)') 'Fatal error detected - Memory problem'
      write(lun(5),'(  a)') 'Not enough memory for allocating arrays ',pntnam
      write(lun(5),'(  a)') 'Delft3D-NESTHD2 aborted'
      write(*,'(  a)') 'Abnormal end of program'
      endprogram nesthd2
