!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
! 
! 
module m_fm_morstatistics
   use unstruc_netcdf
   use m_sediment
   use m_flowgeom
   use m_flow, only: kmx, ndkx
   use m_flowtimes
   use precision

   implicit none

   public morstats

   interface morstats
      module procedure morstats_simple
      module procedure morstats_full
   end interface morstats

   type t_unc_sedids

      integer                  :: ncid = 0
      type(t_unc_timespace_id) :: id_tsp

      integer                  :: id_time        = -1
      integer                  :: id_interval    = -1
      integer                  :: id_morfac      = -1
      integer                  :: id_hs_mean(4)  = -1
      integer                  :: id_hs_std(4)   = -1
      integer                  :: id_hs_max(4)   = -1
      integer                  :: id_hs_min(4)   = -1

      integer                  :: id_ucx_mean(4)  = -1
      integer                  :: id_ucx_std(4)   = -1
      integer                  :: id_ucx_max(4)   = -1
      integer                  :: id_ucx_min(4)   = -1

      integer                  :: id_ucy_mean(4)  = -1
      integer                  :: id_ucy_std(4)   = -1
      integer                  :: id_ucy_max(4)   = -1
      integer                  :: id_ucy_min(4)   = -1

      integer                  :: id_sbx_mean(4)  = -1
      integer                  :: id_sbx_std(4)   = -1
      integer                  :: id_sbx_max(4)   = -1
      integer                  :: id_sbx_min(4)   = -1

      integer                  :: id_sby_mean(4)  = -1
      integer                  :: id_sby_std(4)   = -1
      integer                  :: id_sby_max(4)   = -1
      integer                  :: id_sby_min(4)   = -1

      integer                  :: id_ssx_mean(4)  = -1
      integer                  :: id_ssx_std(4)   = -1
      integer                  :: id_ssx_max(4)   = -1
      integer                  :: id_ssx_min(4)   = -1

      integer                  :: id_ssy_mean(4)  = -1
      integer                  :: id_ssy_std(4)   = -1
      integer                  :: id_ssy_max(4)   = -1
      integer                  :: id_ssy_min(4)   = -1

      integer                  :: id_dmsedcum(4)  = -1
      integer                  :: id_netsbx(4)    = -1
      integer                  :: id_netsby(4)    = -1
      integer                  :: id_netssx(4)    = -1
      integer                  :: id_netssy(4)    = -1

   end type t_unc_sedids
   !
   !  Set up parallel FM administration to ensure backward compatibility of Delft3Dv4
   !
   integer                                         :: nmorstatqnt
   integer                                         :: morstatflg(10,4)
   double precision                                :: morstatt0
   double precision, dimension(:,:), allocatable   :: morstatqnt

   contains

   subroutine morstats_setflags()
      ! needed because FM approach differs from Delft3Dv4, and we need backward compatibility
      ! so we cannot affect rdmor
      use morphology_data_module, only: MOR_STAT_MIN,MOR_STAT_MAX,MOR_STAT_MEAN,MOR_STAT_STD,MOR_STAT_CUM

      implicit none

      integer     :: i
      integer     :: iqnt
      integer     :: idx(6)    ! weights, min, max, mean, std, cum

      i           = 1+stmpar%lsedtot
      nmorstatqnt = 0
      morstatflg  = 0

      do iqnt = 1, 4      ! waterdepth, velocity, bed, sus
         idx = stmpar%morpar%moroutput%statflg(:,iqnt)
         if (iqnt==1) then  ! waterdepth has single component
             if (iand(idx(1),MOR_STAT_MIN)>0) then
                morstatflg(1,iqnt)=morstatflg(1,iqnt)+MOR_STAT_MIN
                i = i+1
                morstatflg(2,iqnt)=i
             endif
             !
             if (iand(idx(1),MOR_STAT_MAX)>0) then
                morstatflg(1,iqnt)=morstatflg(1,iqnt)+MOR_STAT_MAX
                i = i+1
                morstatflg(3,iqnt)=i
             endif
             !
             if (iand(idx(1),MOR_STAT_MEAN)>0) then
                morstatflg(1,iqnt)=morstatflg(1,iqnt)+MOR_STAT_MEAN
                i= i+1
                morstatflg(4,iqnt)=i
             endif
             !
             if (iand(idx(1),MOR_STAT_STD)>0) then
                if (.not. iand(idx(1),MOR_STAT_MEAN)>0) then            ! needed for std calc
                   morstatflg(1,iqnt)=morstatflg(1,iqnt)+MOR_STAT_MEAN
                   i= i+1
                   morstatflg(4,iqnt)=i
                endif
                morstatflg(1,iqnt)=morstatflg(1,iqnt)+MOR_STAT_STD
                i=i+1
                morstatflg(5,iqnt)=i
             endif
             !
             if (iand(idx(1),MOR_STAT_CUM)>0) then
                morstatflg(1,iqnt)=morstatflg(1,iqnt)+MOR_STAT_CUM
                i= i+1
                morstatflg(6,iqnt)=i
             endif
         else   ! rest are vectors, two horizontal components for mean, min, max
                ! std = std of magnitude
             if (iand(idx(1),MOR_STAT_MIN)>0) then
                morstatflg(1,iqnt)=morstatflg(1,iqnt)+MOR_STAT_MIN
                i = i+2
                morstatflg(2,iqnt)=i-1
                morstatflg(3,iqnt)=i
             endif
             !
             if (iand(idx(1),MOR_STAT_MAX)>0) then
                morstatflg(1,iqnt)=morstatflg(1,iqnt)+MOR_STAT_MAX
                i = i+2
                morstatflg(4,iqnt)=i-1
                morstatflg(5,iqnt)=i
             endif
             !
             if (iand(idx(1),MOR_STAT_MEAN)>0) then
                morstatflg(1,iqnt)=morstatflg(1,iqnt)+MOR_STAT_MEAN
                i= i+2
                morstatflg(6,iqnt)=i-1
                morstatflg(7,iqnt)=i
             endif
             !
             if (iand(idx(1),MOR_STAT_STD)>0) then
               if (.not. iand(idx(1),MOR_STAT_MEAN)>0) then            ! needed for std calc
                   morstatflg(1,iqnt)=morstatflg(1,iqnt)+MOR_STAT_MEAN
                   i= i+2
                   morstatflg(6,iqnt)=i-1
                   morstatflg(7,iqnt)=i
                endif
                morstatflg(1,iqnt)=morstatflg(1,iqnt)+MOR_STAT_STD
                i=i+1                                          ! vector has no meaning in this context, so magnitude
                morstatflg(8,iqnt)=i
             endif
             !
             if (iand(idx(1),MOR_STAT_CUM)>0) then
                morstatflg(1,iqnt)=morstatflg(1,iqnt)+MOR_STAT_CUM
                i= i+2
                morstatflg(9,iqnt)=i-1
                morstatflg(10,iqnt)=i
             endif
         endif
      end do
      !
      nmorstatqnt = i
      if (allocated(morstatqnt)) deallocate(morstatqnt)
      allocate(morstatqnt(ndx,nmorstatqnt))
      morstatqnt=0d0
      !
      ! Set minima and maxima start values
      !
      do iqnt = 1,4
          select case (iqnt)
          case (1)  ! waterdepth
             ! min
             if (stmpar%morpar%moroutput%statflg(2,iqnt)>0) then
                 morstatqnt(:,morstatflg(2,iqnt)) = 1e10
             endif
             ! max
             if (stmpar%morpar%moroutput%statflg(3,iqnt)>0) then
                 morstatqnt(:,morstatflg(3,iqnt)) = -1e10
             endif
          case default  ! vectors
             ! min
             if (stmpar%morpar%moroutput%statflg(2,iqnt)>0) then
                 morstatqnt(:,morstatflg(2,iqnt)) = 1e10
                 morstatqnt(:,morstatflg(3,iqnt)) = 1e10
             endif
             ! max
             if (stmpar%morpar%moroutput%statflg(3,iqnt)>0) then
                 morstatqnt(:,morstatflg(4,iqnt)) = -1e10
                 morstatqnt(:,morstatflg(5,iqnt)) = -1e10
             endif
          end select
      enddo

   end subroutine morstats_setflags

   subroutine morstats_clearstats()

      implicit none

      integer :: iqnt

      morstatqnt(:,1) = 0d0
      morstatqnt(:,2:stmpar%lsedtot+1) = 0d0

      do iqnt = 1,4
          select case (iqnt)
          case (1)  ! waterdepth
             ! min
             if (stmpar%morpar%moroutput%statflg(2,iqnt)>0) then
                 morstatqnt(:,morstatflg(2,iqnt)) = 1d10
             endif
             ! max
             if (stmpar%morpar%moroutput%statflg(3,iqnt)>0) then
                 morstatqnt(:,morstatflg(3,iqnt)) = -1d10
             endif
             !
             if (stmpar%morpar%moroutput%statflg(4,iqnt)>0) then
                 morstatqnt(:,morstatflg(4,iqnt)) = 0d0
             endif
             !
            if (stmpar%morpar%moroutput%statflg(5,iqnt)>0) then
                 morstatqnt(:,morstatflg(5,iqnt)) = 0d0
             endif
             !
             if (stmpar%morpar%moroutput%statflg(6,iqnt)>0) then
                 morstatqnt(:,morstatflg(6,iqnt)) = 0d0
             endif
          case default  ! vectors
             ! min
             if (stmpar%morpar%moroutput%statflg(2,iqnt)>0) then
                 morstatqnt(:,morstatflg(2,iqnt)) = 1d10
                 morstatqnt(:,morstatflg(3,iqnt)) = 1d10
             endif
             ! max
             if (stmpar%morpar%moroutput%statflg(3,iqnt)>0) then
                 morstatqnt(:,morstatflg(4,iqnt)) = 0d0           ! comparison on magnitude
                 morstatqnt(:,morstatflg(5,iqnt)) = 0d0
             endif
             if (stmpar%morpar%moroutput%statflg(4,iqnt)>0) then
                 morstatqnt(:,morstatflg(6,iqnt)) = 0d0
                 morstatqnt(:,morstatflg(7,iqnt)) = 0d0
             endif
             if (stmpar%morpar%moroutput%statflg(5,iqnt)>0) then
                 morstatqnt(:,morstatflg(8,iqnt)) = 0d0
             endif
             if (stmpar%morpar%moroutput%statflg(6,iqnt)>0) then
                 morstatqnt(:,morstatflg(9,iqnt)) = 0d0
                 morstatqnt(:,morstatflg(10,iqnt)) = 0d0
             endif
          end select
      enddo

   end subroutine morstats_clearstats

   subroutine morstats_simple(dbodsd)
      use precision, only: fp
      !
      implicit none
      !
      real(fp), dimension(stmpar%lsedtot, ndx), intent(in)  :: dbodsd !  change in sediment composition, units : kg/m2
      !
      integer                                       :: ll
      integer                                       :: k
      !
      if (nmorstatqnt > 0) then
         do k = 1, ndx
            do ll = 1, stmpar%lsedtot
               morstatqnt(k,1+ll) = morstatqnt(k,1+ll) + dbodsd(ll, k)
            enddo
         enddo
      endif
   end subroutine morstats_simple


   subroutine morstats_full(dbodsd, hs_mor, ucxq_mor, ucyq_mor, sbcx, sbcy, sbwx, sbwy, sscx, sscy, sswx, sswy)
      use morphology_data_module, only: MOR_STAT_BODS
      !
      implicit none
      !
      real(fp), dimension(stmpar%lsedtot, ndx),    intent(in)  :: dbodsd !  change in sediment composition, units : kg/m2
      real(fp), dimension(ndx)         ,           intent(in)  :: hs_mor
      real(fp), dimension(ndkx)        ,           intent(in)  :: ucxq_mor
      real(fp), dimension(ndkx)        ,           intent(in)  :: ucyq_mor
      real(fp), dimension(ndx, stmpar%lsedtot),    intent(in)  :: sbcx
      real(fp), dimension(ndx, stmpar%lsedtot),    intent(in)  :: sbcy
      real(fp), dimension(ndx, stmpar%lsedsus),    intent(in)  :: sbwx
      real(fp), dimension(ndx, stmpar%lsedsus),    intent(in)  :: sbwy
      real(fp), dimension(ndx, stmpar%lsedsus),    intent(in)  :: sscx
      real(fp), dimension(ndx, stmpar%lsedsus),    intent(in)  :: sscy
      real(fp), dimension(ndx, stmpar%lsedsus),    intent(in)  :: sswx
      real(fp), dimension(ndx, stmpar%lsedsus),    intent(in)  :: sswy
      !
      ! Local variables
      !
      integer                                        :: ll
      integer                                        :: k
      integer                                        :: kb
      integer                                        :: kt
      integer                                        :: idx
      real(fp)                                       :: q
      real(fp)                                       :: qu
      real(fp)                                       :: qv
      real(fp)                                       :: rhol
      real(fp)                                       :: wght
      !
      if (nmorstatqnt == 0) return
      !
      wght = max(dts/ti_sed,0d0)                     ! time weighted is default, as opposed to original D3D implementation which used accreted volume
      !
      do k = 1, ndx
         if (stmpar%morpar%moroutput%weightflg==MOR_STAT_BODS) then
            wght = 0d0
            do ll = 1, stmpar%lsedtot
               wght = wght + max(0d0,dbodsd(ll,k))
            enddo
         endif
         !
         morstatqnt(k,1) = morstatqnt(k,1) + wght
         !
         do ll = 1, stmpar%lsedtot
            morstatqnt(k,1+ll) = morstatqnt(k,1+ll) + dbodsd(ll, k)
         enddo
         !
         if (morstatflg(1,1)>0) then
            call local_stats(morstatflg(:,1), k, hs_mor(k), wght)
         endif
         !
         if (morstatflg(1,2)>0) then
            if (kmx>0) then
               call getkbotktop(k, kb, kt)
               call local_stats_vec(morstatflg(:,2), k, ucxq_mor(kb), ucyq_mor(kb), wght)   ! only bed layer
            else
               call local_stats_vec(morstatflg(:,2), k, ucxq_mor(k), ucyq_mor(k), wght)
            endif
         endif
         !
         if (morstatflg(1,3)>0) then
            qu = 0.0_fp
            qv = 0.0_fp
            do ll = 1, stmpar%lsedtot
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1.0_fp
               case (1)
                  rhol = stmpar%sedpar%cdryb(ll)
               case (2)
                  rhol = stmpar%sedpar%rhosol(ll)
               end select
               qu = qu + sbcx(k, ll)/rhol + sbwx(k, ll)/rhol
               qv = qv + sbcy(k, ll)/rhol + sbwy(k, ll)/rhol
            enddo
            call local_stats_vec(morstatflg(:,3), k, qu, qv, wght)
         endif
         !
         if (morstatflg(1,4)>0) then
            qu = 0.0_fp
            qv = 0.0_fp
            do ll = 1, stmpar%lsedsus
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1.0_fp
               case (1)
                  rhol = stmpar%sedpar%cdryb(ll)
               case (2)
                  rhol = stmpar%sedpar%rhosol(ll)
               end select
               qu = qu + sscx(k, ll)/rhol + sswx(k, ll)/rhol
               qv = qv + sscy(k, ll)/rhol + sswy(k, ll)/rhol
            enddo
            call local_stats_vec(morstatflg(:,4), k, qu, qv, wght)
         endif
      enddo

   contains

      subroutine local_stats(idx, nm, q, wght)
         integer, dimension(10):: idx
         integer               :: nm
         real(fp)              :: q
         real(fp)              :: wght
         !
         if (idx(2)>0 .and. wght>0.0_fp) then
            morstatqnt(nm,idx(2)) = min(morstatqnt(nm,idx(2)),q)
         endif
         if (idx(3)>0 .and. wght>0.0_fp) then
            morstatqnt(nm,idx(3)) = max(morstatqnt(nm,idx(3)),q)
         endif
         if (idx(4)>0) then
            morstatqnt(nm,idx(4)) = morstatqnt(nm,idx(4)) + wght*q
         endif
         if (idx(5)>0) then
            morstatqnt(nm,idx(5)) = morstatqnt(nm,idx(5)) + wght*q**2
         endif
         if (idx(6)>0) then
            morstatqnt(nm,idx(6)) = morstatqnt(nm,idx(6)) + dts*q
         endif
      end subroutine local_stats

      subroutine local_stats_vec(idx, nm, q1, q2, wght)
         integer, dimension(10):: idx
         integer               :: nm
         real(fp)              :: q1, q2
         real(fp)              :: wght
         real(fp)              :: presval, newval

         newval     = sqrt(q1**2+q2**2)
         if (idx(2)>0 .and. wght>0.0_fp) then
            presval = sqrt(morstatqnt(nm,idx(2))**2+morstatqnt(nm,idx(3))**2)
            if (presval>newval) then
               morstatqnt(nm,idx(2)) = q1
               morstatqnt(nm,idx(3)) = q2
            endif
         endif
         if (idx(4)>0 .and. wght>0.0_fp) then
            presval = sqrt(morstatqnt(nm,idx(4))**2+morstatqnt(nm,idx(5))**2)
            if (presval<newval) then
               morstatqnt(nm,idx(4)) = q1
               morstatqnt(nm,idx(5)) = q2
            endif
         endif
         if (idx(6)>0) then
            morstatqnt(nm,idx(6)) = morstatqnt(nm,idx(6)) + wght*q1
            morstatqnt(nm,idx(7)) = morstatqnt(nm,idx(7)) + wght*q2
         endif
         if (idx(8)>0) then
            morstatqnt(nm,idx(8)) = morstatqnt(nm,idx(8)) + wght*newval**2
         endif
         if (idx(9)>0) then
            morstatqnt(nm,idx(9))  = morstatqnt(nm,idx(9)) + dts*q1
            morstatqnt(nm,idx(10)) = morstatqnt(nm,idx(10)) + dts*q2
         endif
      end subroutine local_stats_vec

   end subroutine morstats_full

  ! Write sedmor statistics to NetCDF
subroutine unc_write_sed(tim)
    use m_flow
    use m_flowtimes
    use unstruc_netcdf
    use unstruc_model
    use unstruc_files , only: defaultFilename

    implicit none

    double precision, intent(in) :: tim

    type(t_unc_sedids), save :: sedids
    integer                  :: ierr
    character(len=256)       :: filnam

    if ( md_mapformat.eq.IFORMAT_NETCDF .or. md_mapformat.eq.IFORMAT_NETCDF_AND_TECPLOT .or. md_mapformat == IFORMAT_UGRID) then
       ! Sedmor output always UGRID
       if (sedids%ncid /= 0 .and. ((md_unc_conv == UNC_CONV_UGRID .and. sedids%id_tsp%idx_curtime == 0))) then
          ierr = unc_close(sedids%ncid)
          sedids%ncid = 0
       end if

       if (sedids%ncid == 0) then
            filnam = defaultFilename('avgsedquant')
            ierr = unc_create(filnam , 0, sedids%ncid)
            if (ierr /= nf90_noerr) then
                call mess(LEVEL_WARN, 'Could not create sedmor averaged quantity file.')
                sedids%ncid = 0
            end if
       endif

       if (sedids%ncid .ne. 0) then
          call unc_write_sedstat_filepointer_ugrid(sedids,tim)
       endif

       ierr = nf90_sync(sedids%ncid) ! Flush file
    end if

end subroutine unc_write_sed

subroutine unc_write_sedstat_filepointer_ugrid(sedids,tim)
   use m_alloc
   use io_ugrid
   use unstruc_netcdf
   use m_flowgeom
   use m_flowparameters
   use m_flowtimes, only: Tudunitstr
   use m_sediment, only: stmpar
   use morphology_data_module, only: MOR_STAT_MIN, MOR_STAT_MAX, MOR_STAT_MEAN, MOR_STAT_STD, MOR_STAT_CUM, MOR_STAT_BODS

   implicit none

   type(t_unc_sedids), intent(inout)            :: sedids
   double precision, intent(in)                 :: tim

   integer                                      :: j, k, ll
   integer                                      :: ndim
   integer                                      :: itim
   integer                                      :: iq
   integer                                      :: ierr
   integer, dimension(4)                        :: id_mean_x, id_std_x, id_max_x, id_min_x, id_net_x
   integer, dimension(4)                        :: id_mean_y, id_std_y, id_max_y, id_min_y, id_net_y
   integer                                      :: idx
   integer                                      :: dim
   integer, dimension(:), allocatable           :: dimids_
   double precision                             :: meanmag2
   double precision                             :: morfc
   double precision, save                       :: morft0, hydrt0
   double precision, dimension(:,:), allocatable:: work
   double precision, dimension(:),   allocatable:: work2
   double precision, dimension(:),   allocatable:: wghtfac
   character(len=10)                            :: transpunit
   character(len=75)                            :: var1, var2
   character(len=150)                           :: descr1, descr2

   if (jased==0 .or. (.not. stm_included) .or. stmpar%lsedtot==0) then
      return
   endif
   !
   ! Use nr of dimensions in netCDF file a quick check whether vardefs were written
   ! before in previous calls.
   ndim = 0
   ierr = nf90_inquire(sedids%ncid, nDimensions=ndim)

   id_mean_x = -1
   id_std_x  = -1
   id_max_x  = -1
   id_min_x  = -1
   id_net_x  = -1
   id_mean_y = -1
   id_std_y  = -1
   id_max_y  = -1
   id_min_y  = -1
   id_net_y  = -1

   ! Only write net and flow geometry data the first time, or for a separate map file.
   if (ndim == 0) then
      !
      ierr = ug_addglobalatts(sedids%ncid, ug_meta_fm)
      call unc_write_flowgeom_filepointer_ugrid(sedids%ncid, sedids%id_tsp)
      !
      ierr = nf90_def_dim(sedids%ncid, 'time', nf90_unlimited, sedids%id_tsp%id_timedim)
      call check_error(ierr, 'def time dim')
      ierr = unc_def_var_nonspatial(sedids%ncid, sedids%id_time, nf90_double, (/ sedids%id_tsp%id_timedim /), 'time', 'time', '', trim(Tudunitstr))
      ierr = unc_def_var_nonspatial(sedids%ncid, sedids%id_interval, nf90_double, (/ sedids%id_tsp%id_timedim /), 'averaging interval', 'averaging interval', '', 's')
      ierr = unc_def_var_nonspatial(sedids%ncid, sedids%id_morfac, nf90_double, (/ sedids%id_tsp%id_timedim /), 'morfac', 'morphological accelaration factor', '', '-')

      ierr = nf90_def_dim(sedids%ncid, 'nSedTot', stmpar%lsedtot, sedids%id_tsp%id_sedtotdim)

      if (stmpar%morpar%moroutput%dmsedcum) then
           ierr = unc_def_var_map(sedids%ncid, sedids%id_tsp, sedids%id_dmsedcum, nf90_double, UNC_LOC_S, 'dmsedcum','net sedimentation flux over time interval','', 'kg m-2',dimids = (/ -2, sedids%id_tsp%id_sedtotdim, -1 /))
      endif
      !
      select case(stmpar%morpar%moroutput%transptype)
      case (0)
         transpunit = 'kg/(s m)'
      case (1)
         transpunit = 'm3/(s m)'
      case (2)
         transpunit = 'm3/(s m)'
      end select
      stmpar%morpar%moroutput%statunt(3) = trim(transpunit) ! bed load
      stmpar%morpar%moroutput%statunt(4) = trim(transpunit) ! suspended load
      !
      ! Conditional defs based on requested output. This can be improved, I guess... Runtime variable definition would be nice to have :)
      !
      do iq = 1,4  ! 1 = waterdepth, 2 = velocity, 3 = bedload, 4 = suspload
         !
         !  TODO for later: generalize to separate fractions
         select case (iq)
            case (1)
               call realloc(dimids_, 2 )
               dimids_= (/ -2, -1 /)
            case (2)
               call realloc(dimids_, 2 )
               dimids_= (/ -2,  -1 /)
            case (3)
               !call realloc(dimids_, 3 )
               !dimids_= (/ -2, sedids%id_tsp%id_sedtotdim, -1 /)
               call realloc(dimids_, 2 )
               dimids_= (/ -2,  -1 /)
            case (4)
               !call realloc(dimids_, 3 )
               !dimids_= (/ -2, sedids%id_tsp%id_sedtotdim, -1 /)
               call realloc(dimids_, 2 )
               dimids_= (/ -2,  -1 /)
         end select

         idx = stmpar%morpar%moroutput%statflg(1,iq)

         if (stmpar%morpar%moroutput%statflg(1,iq)>0) then   ! variable requested
            !
            if (iand(idx,MOR_STAT_MIN)>0) then
               if (iq/=1) then
                  var1 = 'MIN_'//trim(stmpar%morpar%moroutput%statqnt(iq))//'_X'
                  var2 = 'MIN_'//trim(stmpar%morpar%moroutput%statqnt(iq))//'_Y'
                  descr1 = 'minimum '//trim(stmpar%morpar%moroutput%statnam(iq))//', x-component'
                  descr2 = 'minimum '//trim(stmpar%morpar%moroutput%statnam(iq))//', y-component'
               else
                  var1 = 'MIN_'//trim(stmpar%morpar%moroutput%statqnt(iq))
                  descr1  = 'minimum '//trim(stmpar%morpar%moroutput%statnam(iq))
               endif
               ierr = unc_def_var_map(sedids%ncid, sedids%id_tsp, id_min_x, nf90_double, UNC_LOC_S, var1,descr1,descr1, stmpar%morpar%moroutput%statunt(iq),dimids=dimids_)
               if (iq/=1) then
                  ierr = unc_def_var_map(sedids%ncid, sedids%id_tsp, id_min_y, nf90_double, UNC_LOC_S, var2,descr2,descr2, stmpar%morpar%moroutput%statunt(iq),dimids=dimids_)
               endif
            endif
            !
            if (iand(idx,MOR_STAT_MAX)>0) then
               if (iq/=1) then
                  var1 = 'MAX_'//trim(stmpar%morpar%moroutput%statqnt(iq))//'_X'
                  var2 = 'MAX_'//trim(stmpar%morpar%moroutput%statqnt(iq))//'_Y'
                  descr1 = 'maximum '//trim(stmpar%morpar%moroutput%statnam(iq))//', x-component'
                  descr2 = 'maximum '//trim(stmpar%morpar%moroutput%statnam(iq))//', y-component'
               else
                  var1 = 'MAX_'//trim(stmpar%morpar%moroutput%statqnt(iq))
                  descr1  = 'maximum '//trim(stmpar%morpar%moroutput%statnam(iq))
               endif
               ierr = unc_def_var_map(sedids%ncid, sedids%id_tsp, id_max_x, nf90_double, UNC_LOC_S, var1,descr1,descr1, stmpar%morpar%moroutput%statunt(iq),dimids=dimids_)
               if (iq/=1) then
                  ierr = unc_def_var_map(sedids%ncid, sedids%id_tsp, id_max_y, nf90_double, UNC_LOC_S, var2,descr2,descr2, stmpar%morpar%moroutput%statunt(iq),dimids=dimids_)
               endif
            endif
            !
            if (iand(idx,MOR_STAT_MEAN)>0) then
               if (iq/=1) then
                  var1 = 'MEAN_'//trim(stmpar%morpar%moroutput%statqnt(iq))//'_X'
                  var2 = 'MEAN_'//trim(stmpar%morpar%moroutput%statqnt(iq))//'_Y'
                  descr1 = 'mean '//trim(stmpar%morpar%moroutput%statnam(iq))//', x-component'
                  descr2 = 'mean '//trim(stmpar%morpar%moroutput%statnam(iq))//', y-component'
               else
                  var1 = 'MEAN_'//trim(stmpar%morpar%moroutput%statqnt(iq))
                  descr1  = 'mean '//trim(stmpar%morpar%moroutput%statnam(iq))
               endif
               ierr = unc_def_var_map(sedids%ncid, sedids%id_tsp, id_mean_x, nf90_double, UNC_LOC_S, var1,descr1,descr1, stmpar%morpar%moroutput%statunt(iq),dimids=dimids_)
               if (iq/=1) then
                  ierr = unc_def_var_map(sedids%ncid, sedids%id_tsp, id_mean_y, nf90_double, UNC_LOC_S, var2,descr2,descr2, stmpar%morpar%moroutput%statunt(iq),dimids=dimids_)
               endif
            endif
            !
            if (iand(idx,MOR_STAT_STD)>0) then
               var1 = 'STD_'//trim(stmpar%morpar%moroutput%statqnt(iq))
               descr1  = 'standard deviation of the magnitude of '//trim(stmpar%morpar%moroutput%statnam(iq))
               ierr = unc_def_var_map(sedids%ncid, sedids%id_tsp, id_std_x, nf90_double, UNC_LOC_S, var1,descr1,descr1, stmpar%morpar%moroutput%statunt(iq),dimids=dimids_)
            endif
            !
            if (iq==3 .or. iq==4) then    ! does not make much sense for waterlevels and velocities
               if (iand(idx,MOR_STAT_CUM)>0) then
                  if (iq/=1) then
                     var1 = 'NET_'//trim(stmpar%morpar%moroutput%statqnt(iq))//'_X'
                     var2 = 'NET_'//trim(stmpar%morpar%moroutput%statqnt(iq))//'_Y'
                     descr1 = 'net '//trim(stmpar%morpar%moroutput%statnam(iq))//', x-component'
                     descr2 = 'net '//trim(stmpar%morpar%moroutput%statnam(iq))//', y-component'
                  endif
                  ierr = unc_def_var_map(sedids%ncid, sedids%id_tsp, id_net_x, nf90_double, UNC_LOC_S, var1,descr1,descr1, stmpar%morpar%moroutput%statunt(iq),dimids=dimids_)
                  if (iq/=1) then
                     ierr = unc_def_var_map(sedids%ncid, sedids%id_tsp, id_net_y, nf90_double, UNC_LOC_S, var2,descr2,descr2, stmpar%morpar%moroutput%statunt(iq),dimids=dimids_)
                  endif
               endif
            endif
         endif
         !
         ! Copy back temp id's to correct variable id's
         !
         select case (iq)
            case (1)
               sedids%id_hs_mean=id_mean_x; sedids%id_hs_std=id_std_x; sedids%id_hs_min = id_min_x; sedids%id_hs_max = id_max_x;
            case (2)
               sedids%id_ucx_mean = id_mean_x; sedids%id_ucx_std=id_std_x; sedids%id_ucx_min=id_min_x; sedids%id_ucx_max=id_max_x;
               sedids%id_ucy_mean = id_mean_y;  sedids%id_ucy_min=id_min_y; sedids%id_ucy_max=id_max_y;
            case (3)
               sedids%id_sbx_mean = id_mean_x; sedids%id_sbx_std=id_std_x; sedids%id_sbx_min=id_min_x; sedids%id_sbx_max=id_max_x;sedids%id_netsbx=id_net_x
               sedids%id_sby_mean = id_mean_y; sedids%id_sby_min=id_min_y; sedids%id_sby_max=id_max_y;sedids%id_netsby=id_net_y
            case (4)
               sedids%id_ssx_mean = id_mean_x; sedids%id_ssx_std=id_std_x; sedids%id_ssx_min=id_min_x; sedids%id_ssx_max=id_max_x;sedids%id_netssx=id_net_x
               sedids%id_ssy_mean = id_mean_y;  sedids%id_ssy_min=id_min_y; sedids%id_ssy_max=id_max_y;sedids%id_netssy=id_net_y
         end select

      enddo

      ierr = nf90_enddef(sedids%ncid)

      morft0 = morstatt0
      hydrt0 = time1
   endif
   !
   ! write data to netcdf file
   !
   sedids%id_tsp%idx_curtime = sedids%id_tsp%idx_curtime+1
   itim                      = sedids%id_tsp%idx_curtime
   if (itim==1) return
   !
   morfc  = (stmpar%morpar%morft - morft0)/(time1 - hydrt0)*86400d0
   morft0 = stmpar%morpar%morft
   hydrt0 = time1
   !
   ierr = nf90_put_var(sedids%ncid, sedids%id_time, tim, (/ itim /))
   ierr = nf90_put_var(sedids%ncid, sedids%id_interval, morstatqnt(1,1)*ti_sed, (/ itim /))
   ierr = nf90_put_var(sedids%ncid, sedids%id_morfac, morfc, (/ itim /))
   !
   if (stmpar%morpar%moroutput%dmsedcum) then
      allocate( work(ndx, stmpar%lsedtot) )
      do ll = 1, stmpar%lsedtot
          do k = 1, ndx
              work(k, ll) = morstatqnt(k, 1+ll)                ! has morfac incorporated
          enddo
      enddo
      ierr = unc_put_var_map(sedids%ncid, sedids%id_tsp, sedids%id_dmsedcum, UNC_LOC_S, work)
      if (ierr/=0) then
          call mess(LEVEL_FATAL, 'fm_morstatistics::unc_write_sedstat_filepointer_ugrid - Error in subroutine unc_put_var_map (dmsedcum).')
      end if
   endif
   !
   do iq=1,4
      select case (iq)
         case (1)
            id_mean_x = sedids%id_hs_mean; id_std_x = sedids%id_hs_std; id_min_x = sedids%id_hs_min; id_max_x = sedids%id_hs_max; dim=1
         case (2)
            id_mean_x = sedids%id_ucx_mean; id_std_x = sedids%id_ucx_std; id_min_x = sedids%id_ucx_min; id_max_x = sedids%id_ucx_max; dim=2
            id_mean_y = sedids%id_ucy_mean; id_min_y = sedids%id_ucy_min; id_max_y = sedids%id_ucy_max;
         case (3)
            id_mean_x = sedids%id_sbx_mean; id_std_x = sedids%id_sbx_std; id_min_x = sedids%id_sbx_min; id_max_x = sedids%id_sbx_max; id_net_x = sedids%id_netsbx; dim=2
            id_mean_y = sedids%id_sby_mean; id_min_y = sedids%id_sby_min; id_max_y = sedids%id_sby_max; id_net_y = sedids%id_netsby
         case (4)
            id_mean_x = sedids%id_ssx_mean; id_std_x = sedids%id_ssx_std; id_min_x = sedids%id_ssx_min; id_max_x = sedids%id_ssx_max; id_net_x = sedids%id_netssx; dim=2
            id_mean_y = sedids%id_ssy_mean; id_min_y = sedids%id_ssy_min; id_max_y = sedids%id_ssy_max; id_net_y = sedids%id_netssy
      end select
      !
      if (stmpar%morpar%moroutput%statflg(1,iq)>0) then

         idx = stmpar%morpar%moroutput%statflg(1,iq)

         if (iand(idx,MOR_STAT_MEAN)>0 .or. iand(idx,MOR_STAT_STD)>0) then
            if (allocated(wghtfac)) deallocate(wghtfac, work2)
            allocate(wghtfac(1:ndx), work2(1:ndx))
            wghtfac = 1d0; work2 = 0d0
            if (stmpar%morpar%moroutput%weightflg==MOR_STAT_BODS) then
               wghtfac = 1d0/max(morstatqnt(:,1),eps10)
            endif
         endif

         if (dim==1) then                                    ! just waterlevel
            if (iand(idx,MOR_STAT_MIN)>0) then
               where (morstatqnt(:,1)<=0.0)
                  morstatqnt(:,morstatflg(2,iq)) = -999d0
               endwhere
               ierr = unc_put_var_map(sedids%ncid, sedids%id_tsp, id_min_x, UNC_LOC_S, morstatqnt(:,morstatflg(2,iq)))
            endif
            !
            if (iand(idx,MOR_STAT_MAX)>0) then
               where (morstatqnt(:,1)<=0.0)
                  morstatqnt(:,morstatflg(3,iq)) = -999d0
               endwhere
               ierr = unc_put_var_map(sedids%ncid, sedids%id_tsp, id_max_x, UNC_LOC_S, morstatqnt(:,morstatflg(3,iq)))
            endif
            !
            if (iand(idx,MOR_STAT_MEAN)>0) then
               work2 = morstatqnt(:,morstatflg(4,iq))*wghtfac
               where (morstatqnt(:,1)<=0.0)
                  work2 = -999d0
               endwhere
               ierr  = unc_put_var_map(sedids%ncid, sedids%id_tsp, id_mean_x, UNC_LOC_S, work2)
            endif
            !
            if (iand(idx,MOR_STAT_STD)>0) then
               j = morstatflg(5,iq)
               do k = 1,ndx
                  morstatqnt(k, j) = morstatqnt(k, j)*wghtfac(k) - morstatqnt(k, j-1)**2
                  if (morstatqnt(k, j)>0.0_fp) then  ! safety
                      morstatqnt(k, j)  = sqrt(morstatqnt(k, j))
                  else
                      morstatqnt(k, j)  = 0.0_fp
                  endif
               enddo

               where (morstatqnt(:,1)<=0.0)
                  morstatqnt(:,morstatflg(5,iq)) = -999d0
               endwhere

               ierr = unc_put_var_map(sedids%ncid, sedids%id_tsp, id_mean_x, UNC_LOC_S, morstatqnt(:,morstatflg(5,iq)))
            endif
         else
            if (iq==2) then
               morfc = 1d0
            else
               morfc = max(morfc,1d0)
            endif

            if (iand(idx,MOR_STAT_MIN)>0) then

               where (morstatqnt(:,1)<=0.0)
                  morstatqnt(:,morstatflg(2,iq)) = -999d0
                  morstatqnt(:,morstatflg(3,iq)) = -999d0
               endwhere

               ierr = unc_put_var_map(sedids%ncid, sedids%id_tsp, id_min_x, UNC_LOC_S, morstatqnt(:,morstatflg(2,iq)))
               ierr = unc_put_var_map(sedids%ncid, sedids%id_tsp, id_min_y, UNC_LOC_S, morstatqnt(:,morstatflg(3,iq)))
            endif
            !
            if (iand(idx,MOR_STAT_MAX)>0) then
               where (morstatqnt(:,1)<=0.0)
                  morstatqnt(:,morstatflg(4,iq)) = -999d0
                  morstatqnt(:,morstatflg(5,iq)) = -999d0
               endwhere
               ierr = unc_put_var_map(sedids%ncid, sedids%id_tsp, id_max_x, UNC_LOC_S, morstatqnt(:,morstatflg(4,iq)))
               ierr = unc_put_var_map(sedids%ncid, sedids%id_tsp, id_max_y, UNC_LOC_S, morstatqnt(:,morstatflg(5,iq)))
            endif
            !
            if (iand(idx,MOR_STAT_MEAN)>0) then
               work2 = morstatqnt(:,morstatflg(6,iq))*wghtfac*morfc
               where (morstatqnt(:,1)<=0.0)
                  work2 = -999d0
               endwhere
               ierr = unc_put_var_map(sedids%ncid, sedids%id_tsp, id_mean_x, UNC_LOC_S, work2)
               work2 = morstatqnt(:,morstatflg(7,iq))*wghtfac*morfc
               where (morstatqnt(:,1)<=0.0)
                  work2 = -999d0
               endwhere
               ierr = unc_put_var_map(sedids%ncid, sedids%id_tsp, id_mean_y, UNC_LOC_S, work2)
            endif
            !
            if (iand(idx,MOR_STAT_STD)>0) then
               do k = 1,ndx
                  meanmag2 = (morstatqnt(k, morstatflg(6,iq))**2+morstatqnt(k, morstatflg(7,iq))**2)*wghtfac(k)**2
                  morstatqnt(k, morstatflg(8,iq)) = morstatqnt(k, morstatflg(8,iq))*wghtfac(k) - meanmag2
                  if (morstatqnt(k, morstatflg(8,iq))>0.0_fp) then  ! safety
                      morstatqnt(k, morstatflg(8,iq))  = sqrt(morstatqnt(k, morstatflg(8,iq)))*morfc
                  else
                      morstatqnt(k, morstatflg(8,iq))  = 0.0_fp
                  endif
               enddo
               where (morstatqnt(:,1)<=0.0)
                  morstatqnt(:,morstatflg(8,iq)) = -999d0
               endwhere
               ierr = unc_put_var_map(sedids%ncid, sedids%id_tsp, id_std_x, UNC_LOC_S, morstatqnt(:,morstatflg(8,iq)))
            endif
            !
            if (iand(idx,MOR_STAT_CUM)>0 .and. (iq==3 .or. iq==4)) then
               where (morstatqnt(:,1)<=0.0)
                  morstatqnt(:,morstatflg(9,iq)) = -999d0
               endwhere
               ierr = unc_put_var_map(sedids%ncid, sedids%id_tsp, id_net_x, UNC_LOC_S, morstatqnt(:,morstatflg(9,iq))*morfc)
               where (morstatqnt(:,1)<=0.0)
                  morstatqnt(:,morstatflg(10,iq)) = -999d0
               endwhere
               ierr = unc_put_var_map(sedids%ncid, sedids%id_tsp, id_net_y, UNC_LOC_S, morstatqnt(:,morstatflg(10,iq))*morfc)
            endif
         endif
      endif
      ierr = nf90_sync(sedids%ncid)
   end do
   if (ierr/=0) then
       call mess(LEVEL_FATAL, 'fm_morstatistics::unc_write_sedstat_filepointer_ugrid - Error in subroutine unc_put_var_map (morstatqnt).')
   end if
end subroutine unc_write_sedstat_filepointer_ugrid

   end module m_fm_morstatistics
