#include "global_config.inc"
module m_depfil_stm
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2026.                                
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
!-------------------------------------------------------------------------------
!!--description----------------------------------------------------------------- 
! 
!    Function: (Wrapper) Reads the depth values from the attribute file 
!    Either calls the familiar depfil routine, or an interpolator from the
!    EC-module for extended file type and unstructured grid support.
! 
!-------------------------------------------------------------------------------

contains
!
!
!
!==============================================================================
subroutine depfil_stm(lundia    ,error     ,fildep    ,fmttmp    , &
                    & array     ,nfld      ,ifld      ,dims      , &
                    & errmsg    )
   use precision
   use grid_dimens_module
   use m_partitioninfo, only: jampi, my_rank, numranks
! MOR_USE_ECMODULE macro used from global_config.h to enable/disable EC-module for space-varying input in sed/mor.
#if MOR_USE_ECMODULE
   use m_ec_module
   use m_ec_filereader_read, only: ecSampleReadAll
   use m_ec_basic_interpolation, only: triinterp2, nearest_neighbour
#endif
   use system_utils
   ! 
   implicit none 
   ! 
   ! Global variables 
   ! 
   type(griddimtype), target                                          , intent(in)  :: dims   !  grid dimensions
   integer                                                            , intent(in)  :: ifld   !  index of field to be read
   integer                                                                          :: lundia !  unit number for diagnostic file
   integer                                                            , intent(in)  :: nfld   !  number of fields
   logical                                                            , intent(out) :: error  !  Flag=TRUE if an error is encountered 
   real(fp), dimension(nfld, dims%nlb:dims%nub, dims%mlb:dims%mub)    , intent(out) :: array  !  data array to fill
   character(*)                                             , optional, intent(out) :: errmsg !  Error message in case of error
   character(*)                                                       , intent(in)  :: fildep !  Name of the relevant file 
   character(11)                                                      , intent(in)  :: fmttmp !  Format switch for the attribute file 
   ! 
   ! Local variables 
   ! 
   real(fp), allocatable :: array1d(:)
   real(hp), allocatable :: xs(:)
   real(hp), allocatable :: ys(:)
   real(hp), allocatable :: zs(:,:)
   real(hp)              :: xpl(1)
   real(hp)              :: ypl(1)
   real(hp)              :: zpl(1)
   real(hp)              :: transformcoef(25)
   integer               :: minp0
   integer               :: jdla
   integer               :: ibnd
   integer               :: ierror
   integer               :: nm
   integer               :: nm2
   logical               :: success
   real(hp)              :: dmiss    = -999.0_hp
   integer               :: ns
   integer               :: ngrid    
   integer               :: kx
   integer               :: npl
   integer               :: jsferic
   integer               :: jasfer3D
   integer               :: jins
   character(256)        :: path
   character(256)        :: file
   character(256)        :: ext
   character(20)         :: xlocstring
   character(20)         :: ylocstring
   
   integer                                    :: k, n, jamiss
   integer, dimension(:), pointer             :: kcc
   integer, dimension(:), allocatable         :: Mn
   character(20)                              :: nodestr=''  !< keep track of node number in parallel runs for output writing
   logical                                    :: is_open
   integer                                    :: unit_no
   integer                                    :: total_missing
   real(8) :: mem_mb
   logical :: mem_ok
   
   integer :: c0, c1, rate
   real(kind=fp) :: dt
   real :: t0, t1
   ! 
   !! executable statements ------------------------------------------------------- 
   ! 
   transformcoef = 0.0_hp
   error         = .false.
   if (present(errmsg)) errmsg = ' '
   !
   path = ' '
   file = ' '
   ext  = ' ' 
   call split_filename(fildep, path, file, ext)
   if (jampi > 0) then
      write(nodestr, '(a,i0,a,i0,a)') ' [', my_rank, '/', numranks, ']'
   end if

#if MOR_USE_ECMODULE
   if (ext(1:3) == '.xy') then
      ! Assumption: if extension starts with 'xy' (to cover both xyz and xyb), then it is assumed to be an xyz file
      !
      ! TODO: AvD: test code below now works via EC module, but still needs some inconvenient additional 'dummy' arguments. Consider further refactoring.
      call system_clock(c0, rate)
      call cpu_time(t0)
      inquire(file=fildep, opened=is_open, number=unit_no)
      if (is_open) then
         write(lundia,*) '(File ', trim(fildep),' is open on unit ', unit_no, trim(nodestr), ')'
      else
         write(lundia,*) '(File ', trim(fildep),' is not open', trim(nodestr), ')'
      end if
      open (newunit=minp0, file = fildep, form = fmttmp, status = 'old') 
      write(lundia,*) '(File ', trim(fildep),' is open on unit ', minp0, trim(nodestr), ')'
      success = ecSampleReadAll(minp0, fildep, xs, ys, zs, ns, kx)
      write(lundia,*) '(', ns,'samples have been read from file ', trim(fildep),' on unit ', minp0, trim(nodestr), ')'
      close(minp0)
      write(lundia,*) '(File ', trim(fildep),' is closed on unit ', minp0, trim(nodestr), ')'
      call system_clock(c1)
      call cpu_time(t1)
      dt = real(c1 - c0, fp) / real(rate, fp)
      write(lundia, '(a,f6.2,a)') 'Wall time taken to read file '//trim(fildep)//trim(nodestr)//': ', dt, ' seconds'
      write(lundia, '(a,f6.2,a)') 'CPU time taken to read file '//trim(fildep)//trim(nodestr)//': ', t1 - t0, ' seconds'
      call system_clock(c0, rate)
      call cpu_time(t0)
      jdla = 1
      jsferic = 0
      jasfer3D = 0
      jins = 1
      NPL = 0 ! Dummies, since STM is not aware of these yet.
      ngrid = dims%nmmax + size(dims%nmbnd, 1)
      allocate (array1d(ngrid), stat=ierror)
      array1d = dmiss

      CALL triinterp2(dims%xz, dims%yz, array1d, ngrid, jdla, & 
                      XS, YS, ZS(1,:), NS, dmiss, jsferic, jins, jasfer3D, NPL, 0, 0, XPL, YPL, ZPL, transformcoef)
      array(ifld,:,1) = array1d
      deallocate(array1d, stat=ierror)
      write(lundia,*) '(Samples are triangulated to grid with ', dims%nmmax,' points from file ', trim(fildep), ' ', trim(nodestr), ')'
      call system_clock(c1)
      call cpu_time(t1)
      dt = real(c1 - c0, fp) / real(rate, fp)
      write(lundia, '(a,f6.2,a)') 'Wall time taken to triangulate file '//trim(fildep)//trim(nodestr)//': ', dt, ' seconds'
      write(lundia, '(a,f6.2,a)') 'CPU time taken to triangulate file '//trim(fildep)//trim(nodestr)//': ', t1 - t0, ' seconds'
      
      allocate (kcc(ngrid))
      kcc    = 0
      jamiss = 0
      total_missing = 0
      do k = 1,ngrid
         if (array(ifld,k,1) == dmiss) then
            kcc(k) = 1
            jamiss = 1
            total_missing = total_missing + 1
         endif
      enddo
      
      ! For any remaining missing points after regular interpolation, fill them up with nearest neigbour values.
      call system_clock(c0, rate)
      call cpu_time(t0)
      if (jamiss == 1) then
         allocate(Mn(ngrid))
         Mn = 0
         call nearest_neighbour(ngrid, dims%xz, dims%yz, kcc, Mn, dmiss, XS, YS, NS, jsferic, jasfer3D)
         do k = 1,ngrid
            n = Mn(k)
            if (n > 0) then
               array(ifld,k,1) = ZS(1,n)
            endif
         enddo
         deallocate(Mn)
         write(lundia,*) '(Additional missing', total_missing ,'points are filled using nearest neighbour values from file ', trim(fildep), ' ', trim(nodestr), ')'
      end if
      deallocate(kcc)
      call system_clock(c1)
      call cpu_time(t1)
      dt = real(c1 - c0, fp) / real(rate, fp)
      write(lundia, '(a,f6.2,a)') 'Wall time taken to fill missing points using nearest neighbour for file '//trim(fildep)//trim(nodestr)//': ', dt, ' seconds'
      write(lundia, '(a,f6.2,a)') 'CPU time taken to fill missing points using nearest neighbour for file '//trim(fildep)//trim(nodestr)//': ', t1 - t0, ' seconds'
      
      ! mirror boundary cells if undefined if equal to dmiss
      do ibnd = 1, size(dims%nmbnd,1)  ! loop over boundary flow links (TO DO: what about 3D?)
         nm  = dims%nmbnd(ibnd,1)      ! point outside net
         nm2 = dims%nmbnd(ibnd,2)      ! point inside net
         if (array(ifld, nm, 1) == dmiss) then
             array(ifld, nm, 1) = array(ifld, nm2, 1)
         endif 
      enddo   
      
      ! if sample still equal to dmiss (values are not defined on flow nodes) - throw error
      do nm = 1, size(array,2)  ! loop over flow nodes
         if (array(ifld, nm, 1) == dmiss) then
             error = .true.
             write(xlocstring, '(F10.3)') dims%xz(nm)
             write(ylocstring, '(F10.3)') dims%yz(nm)
             if (present(errmsg)) errmsg = 'Error reading samples (not covering full grid) ' // trim(fildep) //' at location (x,y)=('// trim(xlocstring) //','//  trim(ylocstring) //').' 
         endif
      enddo    
      ! success = timespaceinitialfield(dims%xz, dims%yz, array(ifld, :, :), dims%nmmax, fildep, 7, 5,  'O', transformcoef, 1) ! zie meteo module
   else
#endif
      ! No xyz file: depfile
      !
      call depfil(lundia    ,error     ,fildep    ,fmttmp    , &
                & array     ,nfld      ,ifld      ,dims      )
      if (present(errmsg)) errmsg = 'Error reading QUICKIN file '//trim(fildep)
#if MOR_USE_ECMODULE
   endif
#endif
   call get_mem_available_mb(mem_mb, mem_ok)
   if (mem_ok) then
      write(lundia,'(a,f10.1,a)') 'MemAvailable: ', mem_mb, ' MB'
   end if
end subroutine depfil_stm
!
!
!
!==============================================================================
subroutine depfil_stm_double(lundia    ,error     ,fildep    ,fmttmp    , &
                           & array     ,nfld      ,ifld      ,dims      , &
                           & errmsg    )
   use precision 
   use grid_dimens_module
#if MOR_USE_ECMODULE
   use m_ec_module
   use m_ec_basic_interpolation, only: triinterp2, nearest_neighbour
   use m_ec_filereader_read, only: ecSampleReadAll
#endif
   use system_utils
   ! 
   implicit none 
   ! 
   ! Global variables 
   ! 
   type(griddimtype), target                                          , intent(in)  :: dims   !  grid dimensions
   integer                                                            , intent(in)  :: ifld   !  index of field to be read
   integer                                                                          :: lundia !  unit number for diagnostic file
   integer                                                            , intent(in)  :: nfld   !  number of fields
   logical                                                            , intent(out) :: error  !  Flag=TRUE if an error is encountered 
   real(hp), dimension(nfld, dims%nlb:dims%nub, dims%mlb:dims%mub)    , intent(out) :: array  !  data array to fill
   character(*)                                             , optional, intent(out) :: errmsg !  Error message in case of error
   character(*)                                                       , intent(in)  :: fildep !  Name of the relevant file 
   character(11)                                                      , intent(in)  :: fmttmp !  Format switch for the attribute file 
   ! 
   ! Local variables 
   ! 
   real(hp), allocatable :: array1d(:)
   real(hp), allocatable :: xs(:)
   real(hp), allocatable :: ys(:)
   real(hp), allocatable :: zs(:,:)
   real(hp) :: xpl(1)
   real(hp) :: ypl(1)
   real(hp) :: zpl(1)
   real(hp) :: transformcoef(25)
   integer  :: minp0
   integer  :: jdla
   integer  :: ibnd
   integer  :: ierror
   integer  :: nm
   integer  :: nm2
   logical  :: success
   real(hp) :: dmiss    = -999.0_hp
   integer  :: ns, kx
   integer  :: ngrid    
   integer  :: npl
   integer  :: jsferic
   integer  :: jasfer3D
   integer  :: jins
   character(256)        :: path
   character(256)        :: file
   character(256)        :: ext
   character(20)         :: xlocstring
   character(20)         :: ylocstring
   
   integer                                    :: k, n, jamiss
   integer, dimension(:), pointer             :: kcc
   integer, dimension(:), allocatable         :: Mn
   ! 
   !! executable statements ------------------------------------------------------- 
   ! 
   transformcoef = 0.0_hp
   error = .false.
   if (present(errmsg)) errmsg = ' '
   path = ' '
   file = ' '
   ext  = ' ' 
   call split_filename(fildep, path, file, ext)
#if MOR_USE_ECMODULE
   if (ext(1:3) == '.xy') then
      ! Assumption: if extension starts with 'xy' (to cover both xyz and xyb), then it is assumed to be an xyz file
      !
      ! TODO: AvD: test code below now works via EC module, but still needs some inconvenient additional 'dummy' arguments. Consider further refactoring.
      open (newunit=minp0, file = fildep, form = fmttmp, status = 'old') 
      success = ecSampleReadAll(minp0, fildep, xs, ys, zs, ns, kx)

      jdla = 1
      jsferic = 0
      jasfer3D = 0
      jins = 1
      NPL = 0 ! Dummies, since STM is not aware of these yet.

      ngrid = dims%nmmax + size(dims%nmbnd, 1)
      allocate (array1d(ngrid), stat=ierror)
      array1d = dmiss

      CALL triinterp2(dims%xz, dims%yz, array1d, ngrid, jdla, & 
                      XS, YS, ZS(1,:), NS, dmiss, jsferic, jins, jasfer3D, NPL, 0, 0, XPL, YPL, ZPL, transformcoef)
      array(ifld,:,1) = array1d
      deallocate(array1d, stat=ierror)
      
      allocate (kcc(ngrid))
      kcc    = 0
      jamiss = 0
      do k = 1,ngrid
         if (array(ifld,k,1) == dmiss) then
            kcc(k) = 1
            jamiss = 1
         endif
      enddo

      ! For any remaining missing points after regular interpolation, fill them up with nearest neigbour values.
      if (jamiss == 1) then
         allocate(Mn(ngrid))
         Mn = 0
         call nearest_neighbour(ngrid, dims%xz, dims%yz, kcc, Mn, dmiss, XS, YS, NS, jsferic, jasfer3D)
         do k = 1,ngrid
            n = Mn(k)
            if (n > 0) then
               array(ifld,k,1) = ZS(1,n)
            endif
         enddo
         deallocate(Mn)
      end if
      deallocate(kcc)

      ! mirror boundary cells if undefined if equal to dmiss
      do ibnd = 1, size(dims%nmbnd,1)  ! loop over boundary flow links (TO DO: what about 3D?)
         nm  = dims%nmbnd(ibnd,1)      ! point outside net
         nm2 = dims%nmbnd(ibnd,2)      ! point inside net
         if (array(ifld, nm, 1) == dmiss) then
             array(ifld, nm, 1) = array(ifld, nm2, 1)
         endif 
      enddo
      
      ! if sample still equal to dmiss (values are not defined on flow nodes) - throw error
      do nm = 1, size(array,2)  ! loop over flow nodes
         if (array(ifld, nm, 1) == dmiss) then
             error = .true.
             write(xlocstring, '(F10.3)') dims%xz(nm)
             write(ylocstring, '(F10.3)') dims%yz(nm)
             if (present(errmsg)) errmsg = 'Error reading samples (not covering full grid) ' // trim(fildep) //' at location (x,y)=('// trim(xlocstring) //','//  trim(ylocstring) //').' 
         endif    
      enddo    
      close(minp0)

      ! success = timespaceinitialfield(dims%xz, dims%yz, array(ifld, :, :), dims%nmmax, fildep, 7, 5,  'O', transformcoef, 1) ! zie meteo module
   else
#endif
      call depfil_double(lundia    ,error     ,fildep    ,fmttmp    , &
                       & array     ,nfld      ,ifld      ,dims      )
      if (present(errmsg)) errmsg = 'Error reading QUICKIN file '//trim(fildep)
#if MOR_USE_ECMODULE
   endif
#endif
end subroutine depfil_stm_double

                           
subroutine get_mem_available_mb(mem_mb, ok)
  use iso_fortran_env, only: int64
  implicit none
  real(8), intent(out) :: mem_mb
  logical, intent(out) :: ok
  integer :: iu, ios, p
  character(len=256) :: line
  integer(int64) :: kb

  mem_mb = -1.0d0
  ok = .false.
  kb = -1_int64

  open(newunit=iu, file='/proc/meminfo', status='old', action='read', iostat=ios)
  if (ios /= 0) return

  do
     read(iu, '(A)', iostat=ios) line
     if (ios /= 0) exit
     if (index(line, 'MemAvailable:') == 1) then
        p = len('MemAvailable:') + 1
        read(line(p:), *, iostat=ios) kb
        if (ios == 0) then
           mem_mb = real(kb,8) / 1024.0d0
           ok = .true.
        end if
        exit
     end if
  end do
  close(iu)
end subroutine get_mem_available_mb

end module m_depfil_stm
    
    
