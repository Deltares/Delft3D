subroutine inimet(lundia    ,error     ,versio    ,wave      ,trasol    , &
                & momsol    ,method    ,dischy    ,solver    ,icreep    , &
                & disctr    ,gdp       )
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
! - Initialisation of numerical parameters which define the numerical methods.
! - The parameters will be either read from local file "method"
!   or set to default values
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
    integer                        , pointer :: nprocs
    integer      , dimension(:)    , pointer :: nread
    integer      , dimension(:, :) , pointer :: nprinp
    character*256, dimension(:)    , pointer :: filusr
    character*20 , dimension(:)    , pointer :: procs
!
! Global variables
!
    integer                    :: icreep !  Description and declaration in tricom.igs
    integer                    :: lundia !  Description and declaration in inout.igs
    logical      , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    logical      , intent(in)  :: wave   !  Description and declaration in procs.igs
    character(6) , intent(in)  :: momsol
    character(13), intent(in)  :: trasol !  Description and declaration in tricom.igs
    character(5) , intent(in)  :: versio !!  Version nr. of the current package
    character(8)               :: dischy !  Description and declaration in tricom.igs
    character(8)               :: disctr !  Description and declaration in tricom.igs
    character(8)               :: method !  Description and declaration in tricom.igs
    character(8)               :: solver !  Description and declaration in tricom.igs
!
! Local variables
!
    integer           :: iocond ! IO status output: > 0 error< 0 end-of-file = 0 ok 
    integer           :: kreep  ! Help variable for reading ICREEP 
    integer           :: lfil
    integer           :: lunmet ! Unit nr. for Method file 
    integer           :: n
    integer           :: nfil
    integer           :: nn
    integer, external :: newlun
    logical           :: ex     ! Logical flag for file existence 
!
!! executable statements -------------------------------------------------------
!
    nprocs  => gdp%gdusrpar%nprocs
    nread   => gdp%gdusrpar%nread
    nprinp  => gdp%gdusrpar%nprinp
    filusr  => gdp%gdusrpar%filusr
    procs   => gdp%gdusrpar%procs
    !
    method = 'adi'
    dischy = 'cn'
    solver = 'jac'
    disctr = 'impl'
    !
    ! Initialize local parameters
    !
    kreep = 0
    !
    ! Check if user defined process 'numerical method' is defined
    ! NOTE: process 'numerical method' contains only one file or
    !       only one integer (for anti creep)
    !
    do n = 1, nprocs
       if (procs(n)=='numerical method    ') then
          if (nread(n)/=0) then
             nfil = 0
             do nn = 1, nread(n)
                nfil = nfil + nprinp(1, nn)
             enddo
             !
             ! Check if process 'numerical method' contains a file
             !
             if (nprinp(1, nread(n))>0) then
                !
                !
                ! File contains character strings that represent the
                ! following (four) characteristics:
                ! a) numerical method (adi only; aoi not supported anymore)
                ! b) time discretisation for hydrodynamical part
                !    (cn or be), Crank Nicolson or Backward Euler
                ! c) solver (jac or gs), Point Jacobi or Gauss Seidel
                ! d) creep (1 or 0), anti creep or no anti creep (to be added)
                ! e) time discretisation for transport solver (expl or impl)
                !
                !
                lfil = index(filusr(nfil), ' ')
                if (lfil==0) lfil = 13
                lfil = lfil - 1
                inquire (file = filusr(nfil)(:lfil), exist = ex)
                !
                ! YES file, read input
                !
                if (ex) then
                   open (newunit=lunmet, file = filusr(nfil)(:lfil), form = 'formatted',&
                       & status = 'old')
                   !
                   ! Read numerical method
                   !
                   read (lunmet, '(a8)', iostat = iocond) method
                   if (iocond/=0) then
                      call prterr(lundia    ,'G007'    ,filusr(nfil)(:lfil)  )
                      !
                      error = .true.
                      goto 130
                   endif
                   !
                   write (lundia, *) ' '
                   write (lundia, *) '******************************'
                   write (lundia, *) '* This is a research version *'
                   write (lundia, *) '* of Delft3D-FLOW, which     *'
                   write (lundia, *) '* has been incorporated      *'
                   write (lundia, *) '* in version ', versio, '    *'
                   write (lundia, *) '******************************'
                   write (lundia, *) ' '
                   write (lundia, *) ' '
                   write (lundia, *) '******************************'
                   if (method=='adi') then
                      write (lundia, *) '*     ADI METHOD            *'
                   elseif (method=='adiwang') then
                      write (lundia, *) '*     ADIWANG METHOD        *'
                   elseif (method=='aoi') then
                      write (lundia, *) '*** ERROR: AOI method not sup',        &
                                       & 'ported anymore'
                      error = .true.
                   else
                      write (lundia, *) '*** ERROR: Wrong method speci',        &
                                       & 'fied (adi): ', method,           &
                                       & ' in file ', filusr(nfil)(:lfil)
                      error = .true.
                   endif
                   !
                   ! Read discretization (cn or be)
                   !
                   read (lunmet, '(a8)', iostat = iocond) dischy
                   if (iocond/=0) then
                      call prterr(lundia    ,'G007'    ,filusr(nfil)(:lfil)  )
                      !
                      error = .true.
                      goto 130
                   endif
                   !
                   if (dischy=='cn') then
                      write (lundia, *) '*     CRANK NICOLSON        *'
                   elseif (dischy=='be') then
                      write (lundia, *) '*** ERROR: Bacward Euler discre',      &
                                       & 'tisation not supported anymore'
                      error = .true.
                   else
                      write (lundia, *) '*** ERROR: Wrong discretixation',      &
                                       & ' (cn or be) specified: ', dischy,      &
                                       & ' in file ', filusr(nfil)(:lfil)
                      error = .true.
                   endif
                   !
                   ! Read solver (jac or gs)
                   !
                   read (lunmet, '(a8)', iostat = iocond) solver
                   if (iocond/=0) then
                      call prterr(lundia    ,'G007'    ,filusr(nfil)(:lfil)  )
                      !
                      error = .true.
                      goto 130
                   endif
                   !
                   if (solver=='jac') then
                      write (lundia, *) '*     JACOBI                *'
                   elseif (solver=='gs') then
                      write (lundia, *) '*     GAUSS SEIDEL          *'
                   else
                      write (lundia, *) '*** ERROR: Wrong solver (jac or ',     &
                                       & 'gs ) specified: ', solver, ' in ',     &
                                       & 'file ', filusr(nfil)(:lfil)
                      error = .true.
                   endif
                   !
                   ! Read creep flag (anti creep is applied (1=yes,0=no))
                   !
                   read (lunmet, *, iostat = iocond) kreep
                   if (iocond/=0) then
                      call prterr(lundia    ,'G007'    ,filusr(nfil)(:lfil)  )
                      !
                      error = .true.
                      goto 130
                   endif
                   !
                   ! Define ICREEP when was not yet read from MD-file (999)
                   !
                   if (icreep==999) then
                      icreep = kreep
                      if (icreep==1) then
                         write (lundia, *) '*     WITH ANTI CREEP       *'
                      elseif (icreep==0) then
                         write (lundia, *) '*     WITHOUT ANTI CREEP    *'
                      else
                         write (lundia, *) &
                             & '*** ERROR: Wrong input for anti', ' creep (1/0) specified: ', icreep,  &
                             & ' in file ', filusr(nfil)(:lfil)
                         error = .true.
                      endif
                   else
                      write (lundia, *) '*     ANTI CREEP skipped    *'
                   endif
                   !
                   ! Read time integration for transport solver (expl or impl)
                   !
                   read (lunmet, '(a8)', iostat = iocond) disctr
                   if (iocond/=0) then
                      call prterr(lundia    ,'G007'    ,filusr(nfil)(:lfil)  )
                      !
                      error = .true.
                      goto 130
                   endif
                   !
                   if (disctr=='expl') then
                      write (lundia, *) '*     EXPLICIT TRANSPORT    *'
                   elseif (disctr=='impl') then
                      write (lundia, *) '*     IMPLICIT TRANSPORT    *'
                   else
                      write (lundia, *) '*** ERROR: Wrong discr. for tra',      &
                                       & 'nsport (expl or impl): ', disctr,      &
                                       & ' in file ', filusr(nfil)(:lfil)
                      error = .true.
                   endif
                   !
                   write (lundia, *) '*****************************'
                   write (lundia, *) ' '
                   !
                   ! Test combinations which are possible (not allowed !!)
                   !
                   if (method=='adi' .or. method=='adiwang') then
                      if (disctr=='expl') then
                         write (lundia, *) &
                             & '*** ERROR: Combination ADI ', 'and EXPL not allowed !!'
                         error = .true.
                      endif
                      !
                      if (solver=='gs') then
                         write (lundia, *) &
                             & '*** ERROR: Combination ADI ', 'and GS not allowed !!'
                         error = .true.
                      endif
                   endif
                   !
  130              continue
                   close (lunmet)
                !
                ! NO file, write warning
                !
                else
                   call prterr(lundia    ,'G004'    ,filusr(nfil)(:lfil)  )
                !
                endif
             endif
          !
          endif
       endif
    enddo
    !
    ! Check value of ICREEP if still 999 reset to 0 (default value)
    !
    if (icreep==999) then
       icreep = 0
    endif
end subroutine inimet
