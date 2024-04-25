subroutine wrhfluff(lundia    ,error     ,filename  ,grpnam    , &
                  & nostat    ,lsed      ,irequest  , &
                  & fds       ,nostatto  ,nostatgl  ,order_sta ,gdp       )
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
!    Function: Writes the time varying data for the fluff layer
!              to the sediment group on the NEFIS FLOW MAP file
!
! Method used:
!
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use wrtarray, only: wrtarray_n, station
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                              , pointer :: celidt
    type (datagroup)                     , pointer :: group
    real(fp)        , dimension(:,:)     , pointer :: mfluff
    integer         , dimension(:, :)    , pointer :: mnstat
!
! Global variables
!
    character(16)                                                       , intent(in)  :: grpnam
    integer                                                             , intent(in)  :: irequest !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                                           :: lsed     !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: lundia   !  Description and declaration in inout.igs
    integer                                                                           :: nostat   !  Description and declaration in dimens.igs
    logical                                                             , intent(out) :: error    !!  Flag=TRUE if an error is encountered
    character(*)                                                        , intent(in)  :: filename !  File name
    integer                                                             , intent(in)  :: fds      !  File handle of output NEFIS/NetCDF file
    integer                                                             , intent(in)  :: nostatgl ! global number of stations (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                                             , intent(in)  :: nostatto ! total number of stations (including "duplicate" stations located in halo regions)
    integer       , dimension(nostat)                                   , intent(in)  :: order_sta

!
! Local variables
!
    integer                                           :: filetype
    real(fp)        , dimension(:,:)  , allocatable   :: rbuff2
    integer                                           :: ierror         ! Local error flag
    integer                                           :: ii
    integer                                           :: istat
    integer                                           :: l
    integer                                           :: n
    integer                                           :: nm
    integer                                           :: m    
    !
    integer                                           :: iddim_nostat
    integer                                           :: iddim_lsed
    !
    integer                                           :: idatt_sta
!
!! executable statements -------------------------------------------------------
!
    if (gdp%gdmorpar%flufflyr%iflufflyr==0) return
    if (lsed == 0) return
    !
    ierror = 0
    call getdatagroup(gdp, FILOUT_HIS, grpnam, group)
    celidt     => group%celidt
    filetype = getfiletype(gdp, FILOUT_HIS)
    !
    mnstat      => gdp%gdstations%mnstat
    !
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Define dimensions
       !
       iddim_nostat  = adddim(gdp, lundia, FILOUT_HIS, 'NOSTAT', nostatgl)
       iddim_lsed    = adddim(gdp, lundia, FILOUT_HIS, 'LSED', lsed)
       idatt_sta = addatt(gdp, lundia, FILOUT_HIS, 'coordinates','NAMST XSTAT YSTAT')
       !
       ! Define elements
       !
       call addelm(gdp, lundia, FILOUT_HIS, grpnam, 'MFLUFF', ' ', IO_REAL4, 2, dimids=(/iddim_nostat, iddim_lsed/), longname='Sediment mass in fluff layer (kg/m2)', unit='kg/m2', attribs=(/idatt_sta/) )
    case (REQUESTTYPE_WRITE)
       !
       ! Write data to file
       !
       ! element 'MFLUFF'
       !
       mfluff => gdp%gdmorpar%flufflyr%mfluff
       allocate (rbuff2(1:nostat,1:lsed), stat=istat)
       rbuff2 = 0.0_fp
       do l = 1, lsed
          do ii = 1, nostat
             m  = mnstat(1,ii)
             if (m<0) cycle
             n  = mnstat(2,ii)
             if (n<0) cycle
             !
             call n_and_m_to_nm(n, m, nm, gdp)
             !
             rbuff2(ii,l) = mfluff(l,nm)
          enddo
       enddo
       call wrtarray_n(fds, filename, filetype, grpnam, &
              & celidt, nostat, nostatto, nostatgl, order_sta, gdp, &
              & lsed, &
              & ierror, lundia, rbuff2, 'MFLUFF', station)
       deallocate(rbuff2)
       if (ierror /= 0) goto 9999
       !
    endselect
    !
9999 continue
    if (ierror/= 0) error = .true.
end subroutine wrhfluff
