!nesthd1
      program nesthd1
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
!****&******************************************************************
! Deltares                         marine and coastal management
!
! program            : nesthd1
! version            : v1.0
! date               : June 1997
! programmer         : Lamber Hulsen/Theo van der Kaaij
! version            : 1.50.01
!
! function           :    determine nest stations, weight factors and
!                         angles in an overall model to generate
!                         boundary conditions (with NESTHD2) at boundary
!                         support points in a nested model
! notes              : 1. the nest characteristics are determined both
!                         for a nesting at water level points as well as
!                         a nesting at velocity points. In this
!                         way you are free to choose the kind of
!                         boundary forcing when running NESTHD2 without
!                         re-running NESTHD1.
!                      2. NESTHD1 requires that the overall and nested
!                         models are defined in the same co-ordinate
!                         system.
!                      3. This version of NESTHD1 will NOT compose a
!                         grid for spherical or rectilinear models.
!                         For these models a user-defined grid is
!                         required.
!****&******************************************************************

      use precision
      use nesthd1_version_module
!
      integer      , dimension(:)    , allocatable :: ipx
      integer      , dimension(:)    , allocatable :: ipy
      integer      , dimension(:)    , allocatable :: itotpx
      integer      , dimension(:)    , allocatable :: itotpy
!
      integer      , dimension(:,:)  , allocatable :: icom1
      integer      , dimension(:,:)  , allocatable :: icom2
      integer      , dimension(:,:)  , allocatable :: mcbsp
      integer      , dimension(:,:)  , allocatable :: ncbsp
!
      integer      , dimension(:,:,:), allocatable :: mcnes
      integer      , dimension(:,:,:), allocatable :: ncnes
!
      real         , dimension(:)    , allocatable :: angle
!
      real         , dimension(:,:)  , allocatable :: x1
      real         , dimension(:,:)  , allocatable :: x2
      real         , dimension(:,:)  , allocatable :: y1
      real         , dimension(:,:)  , allocatable :: y2
      real         , dimension(:,:)  , allocatable :: xbnd
      real         , dimension(:,:)  , allocatable :: ybnd
!
      real         , dimension(:,:,:), allocatable :: weight
!
      character(1) , dimension(:)    , allocatable :: typbnd
      character(20), dimension(:)    , allocatable :: nambnd
!
      integer       lun   (  7   )
      character*80  filnam (7)
      character*80  CIDENT
      logical       spher_crs, spher_det
!
!     Version string
!
    cident = ' '
    call getfullversionstring_nesthd1(cident)
!-----------------------------------------------------------------------
!---- 0. Initialisation
!-----------------------------------------------------------------------
      lunscr = 6

!-----------------------------------------------------------------------
!---- 1. Open all files
!-----------------------------------------------------------------------
      write (*     ,'(/, 2a)') ' ',trim(cident)
      call opnfl1(lun   , filnam, CIDENT)

!-----------------------------------------------------------------------
!---- 2. Dynamic memory allocation (FMM)
!-----------------------------------------------------------------------
!
!     Get dimensions for allocation
      call getdim(lun   , mmax1 , nmax1  , &
     &            mmax2 , nmax2 , maxnrp , maxbnd )
!
      allocate(ipx   (maxnrp), STAT = istat)
      allocate(ipy   (maxnrp), STAT = istat)
      allocate(itotpx(maxnrp), STAT = istat)
      allocate(itotpy(maxnrp), STAT = istat)
!
      allocate(icom1 ( mmax1 , nmax1 ), STAT = istat)
      allocate(icom2 ( mmax2 , nmax2 ), STAT = istat)
      allocate(mcbsp ( maxbnd, 2     ), STAT = istat)
      allocate(ncbsp ( maxbnd, 2     ), STAT = istat)
!
      allocate(mcnes ( maxbnd, 2, 4     ), STAT = istat)
      allocate(ncnes ( maxbnd, 2, 4     ), STAT = istat)
!
      allocate(angle ( maxbnd), STAT = istat)
!
      allocate(x1    ( mmax1 , nmax1 ), STAT = istat)
      allocate(x2    ( mmax2 , nmax2 ), STAT = istat)
      allocate(y1    ( mmax1 , nmax1 ), STAT = istat)
      allocate(y2    ( mmax2 , nmax2 ), STAT = istat)
      allocate(xbnd  ( maxbnd, 2     ), STAT = istat)
      allocate(ybnd  ( maxbnd, 2     ), STAT = istat)
!
      allocate(weight( maxbnd, 2, 4     ), STAT = istat)
!
      allocate(typbnd (maxbnd), STAT = istat)
      allocate(nambnd (maxbnd), STAT = istat)

!-----------------------------------------------------------------------
!---- 3. Zeroise arrays
!-----------------------------------------------------------------------

      mcbsp = 0
      ncbsp = 0
      icom1 = 0
      icom2 = 0

      mcnes = 0
      ncnes = 0

      angle = 0.0

      x1    = 0.0
      x2    = 0.0
      y1    = 0.0
      y2    = 0.0

      weight= 0.0

!-----------------------------------------------------------------------
!---- 4. Read grid and boundary data models
!-----------------------------------------------------------------------

      call reargf(lun(1)    ,x1       ,y1       ,mmax1  ,nmax1  ,mc1   ,nc1, &
                & spher_crs )

      ipx = 0
      ipy = 0
      itotpx = 0
      itotpy = 0
      call inigrd(lun(2),ifout ,nmax1 ,mmax1 ,icom1, ipx,ipy, &
     &            itotpx,itotpy,idum                          )

      if (ifout .ne. 0) stop 'Error in grid enclosure overall model'

      call reargf(lun(3)   ,x2  ,y2 ,mmax2  ,nmax2  ,mc2   ,nc2   , &
               &  spher_det)
      if (spher_crs .neqv. spher_det) stop 'Coarse and detailled model should be in the same coordinate system'

      ipx = 0
      ipy = 0
      itotpx = 0
      itotpy = 0
      call inigrd(lun(4),ifout ,nmax2 ,mmax2 ,icom2, ipx,ipy, &
     &            itotpx,itotpy,idum                          )
      if (ifout .ne. 0) stop 'Error in grid enclosure detailed model'

      call dimbnd(lun(5), nobnd )
      if (nobnd .gt. maxbnd) stop 'Enlarge maxbnd'
      call reabnd(lun(5),mcbsp, ncbsp, typbnd, nambnd, &
     &            nobnd)

!-----------------------------------------------------------------------
!---- 5. Determine weight factors water level boundary
!-----------------------------------------------------------------------

      call detxy (mcbsp , ncbsp , x2     , y2    , icom2 , &
     &            xbnd  , ybnd  , mmax2  , nmax2 , maxbnd, &
     &            nobnd , 'WL'                          )

      call detnst(x1      , y1      , icom1   , xbnd    , ybnd     , &
     &            mcbsp   , ncbsp   , mcnes   , ncnes   , weight   , &
     &            mmax1   , nmax1   , mc1     , nc1     , maxbnd   , &
     &            nobnd   , spher_crs                   )

!-----------------------------------------------------------------------
!---- 6. Write weight factors water level boundary to administration
!        file and write stations to trisula input file
!-----------------------------------------------------------------------

      call wrinst(lun(6)   , mcbsp    , ncbsp    , mcnes    , ncnes   , &
     &            weight   , angle    , maxbnd   , nobnd    , 'WL'    )

      call wrista(lun(7), mcnes    , ncnes    , maxbnd)

!-----------------------------------------------------------------------
!---- 7. Determine weight factors velocity boundary but first
!        zeroise arrays
!-----------------------------------------------------------------------

      mcnes = 0
      ncnes = 0

      weight = 0.0

      call detxy  (mcbsp    ,ncbsp    ,x2    ,y2    ,icom2    , &
     &             xbnd     ,ybnd     ,mmax2 ,nmax2 ,maxbnd   , &
     &             nobnd    , 'UV'                          )

      call detnst (x1      ,y1      ,icom1    ,xbnd    ,ybnd     , &
     &             mcbsp   ,ncbsp   ,mcnes    ,ncnes   ,weight   , &
     &             mmax1   , nmax1  ,mc1      ,nc1     ,maxbnd   , &
     &             nobnd   , spher_crs                           )

!-----------------------------------------------------------------------
!---- 8. Determine angles velocity boundary
!-----------------------------------------------------------------------

      call detang (xbnd     ,ybnd   ,angle    ,mcbsp    ,ncbsp    , &
     &             mmax2    ,nmax2  ,maxbnd   ,nobnd              )

!-----------------------------------------------------------------------
!---- 9. Write weight factors/angles velocity boundary
!-----------------------------------------------------------------------

      call wrinst(lun(6)   ,mcbsp    ,ncbsp    ,mcnes    ,ncnes    , &
     &            weight   ,angle    ,maxbnd   ,nobnd    ,'UV'        )

      call wrista(lun(7), mcnes    , ncnes    , maxbnd)

!-----------------------------------------------------------------------
!---- end program
!-----------------------------------------------------------------------

      call clsfil(lun   ,7     )

 9000 continue
      endprogram nesthd1
