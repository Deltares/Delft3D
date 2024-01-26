#include "global_config.inc"
module m_depfil_stm
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
      ! No xyz file: depfile
      !
      call depfil(lundia    ,error     ,fildep    ,fmttmp    , &
                & array     ,nfld      ,ifld      ,dims      )
      if (present(errmsg)) errmsg = 'Error reading QUICKIN file '//trim(fildep)
#if MOR_USE_ECMODULE
   endif
#endif
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

                           
                           
end module m_depfil_stm
    
    
