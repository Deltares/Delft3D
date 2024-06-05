!> \file
!! Writes the hyd file that determines everything for the Delwaq GUI
      subroutine wrwaqhyd ( filnam , itdate , tstart , tstop  , dt     , &
     &                      itwqff , itwqfl , itwqfi , nmax   , mmax   , &
     &                      kmax   , thick  , lsal   , ltem   , lsed   , &
     &                      chez   , nsrc   , mnksrc , namsrc , runid  , &
     &                      nowalk , iwlk   , aggre  , flaggr , zmodel , &
     &                      ilaggr , nd     , nlb    , nub    , mlb    , &
     &                      mub    , kfsmin , ksrwaq , noseg  , noq1   , &
     &                      noq2   , noq3   , xz     , yz     , zbot   , &
     &                      ztop, gdp)
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
      use time_module
      use flow2d3d_version_module
      use globaldata
            
      implicit none
!      
      type(globdat),target :: gdp
!
!           Global variables
!
      character(*)  filnam             !<  Filename without extension
      integer       itdate             !<  reference time in YYYYMMDD
      real     (fp) tstart             !<  Flow start time in minutes
      real     (fp) tstop              !<  Flow stop time in minutes
      real     (fp) dt                 !<  Flow time step in minutes
      integer       itwqff             !<  first waq time step
      integer       itwqfl             !<  last  waq time step
      integer       itwqfi             !<  waq time step
      integer       nmax               !<  Dimension of first index in 2d arrays
      integer       mmax               !<  Dimension of second index in 2d arrays
      integer       kmax               !<  number of layers
      real     (fp) thick(kmax)        !<  Relative layer thickness sigma coords
      integer       lsal               !<  Substance number salinity
      integer       ltem               !<  Substance number temperature
      integer       lsed               !<  Number of substances for sediment
      logical       chez               !<  if true, then chezy values
      integer       nsrc               !<  number of sources
      integer       mnksrc(7,nsrc)     !<  location and type of sources
      character(20) namsrc(  nsrc)     !<  names of the sources
      character( *) runid              !<  name of the run
      integer       nowalk             !<  number of walking discharges
      integer       iwlk  (  nsrc)     !<  location of walking discharges
      integer       aggre              !<  1 if aggregation, 0 is condensation
      character( *) flaggr             !<  Name of aggregation file id aggre
      logical       zmodel             !<  true if z-model feature is used
      integer       ilaggr(kmax)       !<  vertical layer aggregation array
      integer       nd                 !<  number of domains
      integer(4)    nlb                !<  Lower bound of all n dimensions
      integer(4)    nub                !<  Upper bound of all n dimensions
      integer(4)    mlb                !<  Lower bound of all m dimensions
      integer(4)    mub                !<  Upper bound of all m dimensions
      integer  kfsmin(nlb:nub,mlb:mub) !<  Variable lowest active layer (z-model-only)
      integer(4)    ksrwaq(2*nsrc)     !<  stored value of nr of layers source locations
      integer(4)    noseg              !<  Total number of segments
      integer(4)    noq1               !<  Exchanges in first direction
      integer(4)    noq2               !<  Exchanges in second direction
      integer(4)    noq3               !<  Exchanges in third direction
      real(fp) , dimension(nlb:nub,mlb:mub) , intent(in) :: xz     !  Description and declaration in esm_alloc_real.f90
      real(fp) , dimension(nlb:nub,mlb:mub) , intent(in) :: yz     !  Description and declaration in esm_alloc_real.f90
      real(fp)      zbot               !<  Maximum depth in the model (relative to the reference level; unit: metres; positive upwards).
      real(fp)      ztop               !<  The "imaginary" maximum water level in the model (relative to the reference level; unit: metres; positive upwards).
!
!           Local  variables
!
      real     (fp) timsec
      integer       julday, idate, itime ! waq handles julian times better !!!
      integer       i, ilay            !! loop variables
      integer       l                  !! counter nr of sediment substances
      integer       il                 !! counter inlet-outlet
      integer       n, m, nl           !! grid indices
      real     ( 4) anl                !! for waq layers
      integer  ( 4) kfmin, kfmax       !! help variables z-model
      character(256) full_version      !! Delft3D FLOW version information
      character(20)  rundat            !! Current date and time containing a combination of DATE and TIME
      character(21)  datetime          !! Date/time to be filled in the header
      character(256) filstring, file1, file2
      character(6) sf                  !! character variable for s(ediment)f(iles)
      integer(4)    lunout1 !! write hyd file for structured grid needed for delpar
      integer(4)    lunout2 !! write hyd-file for DeltaShell (unstructured format)
      integer(4)    nm_sink !! NM lcoation of the sink (sink-sources)
      integer(4)    nm_source !! NM lcoation of the sourc (sin-sources)
!
!! executable statements -------------------------------------------------------
!
      file1 = trim(filnam)//'.hyd'
      open  ( newunit = lunout1 , file=trim(file1) )
      file2 = trim(filnam)//'_unstructured.hyd'
      open  ( newunit = lunout2 , file=trim(file2) )

      full_version  = ' '
      call getfullversionstring_flow2d3d(full_version)
      write(lunout1,'(a,a)') 'file-created-by  '//trim(full_version)
      write(lunout2,'(a,a)') 'file-created-by  '//trim(full_version)

      call dattim(rundat)
      datetime = rundat(1:4)//'-'//rundat(6:7)//'-'//rundat(9:10)//','//rundat(11:19)
      write(lunout1,'(a,a)') 'file-creation-date  '//datetime
      write(lunout2,'(a,a)') 'file-creation-date  '//datetime

      write ( lunout1 , '(a      )' ) 'task      full-coupling'
      write ( lunout2 , '(a      )' ) 'task      full-coupling'
      if (zmodel) then
         write ( lunout1 , '(a      )' ) 'geometry  curvilinear-grid  z-layers'
         write ( lunout2 , '(a      )' ) 'geometry  unstructured  z-layers'
      else
         write ( lunout1 , '(a      )' ) 'geometry  curvilinear-grid'
         write ( lunout2 , '(a      )' ) 'geometry  unstructured'
      end if
      if ( aggre .lt. 0 ) then
         write ( lunout1 , '(a,a    )' ) 'horizontal-aggregation       ','no'
         write ( lunout2 , '(a,a    )' ) 'horizontal-aggregation       ','no'
      end if 
      if ( aggre .eq. 0 ) then
         write ( lunout1 , '(a,a    )' ) 'horizontal-aggregation       ','automatic'
         write ( lunout2 , '(a,a    )' ) 'horizontal-aggregation       ','automatic'
      end if 
      if ( aggre .gt. 0 ) then
         write ( lunout1 , '(a,a    )' ) 'horizontal-aggregation       ','yes'
         write ( lunout2 , '(a,a    )' ) 'horizontal-aggregation       ','yes'
      end if 
      write ( lunout1 , '(a,a    )' ) 'minimum-vert-diffusion-used  ','no'
      write ( lunout2 , '(a,a    )' ) 'minimum-vert-diffusion-used  ','no'
      write ( lunout1 , '(a,a    )' ) 'vertical-diffusion           ','calculated'
      write ( lunout2 , '(a,a    )' ) 'vertical-diffusion           ','calculated'
      write ( lunout1 , '(a      )' ) 'description'
      write ( lunout2 , '(a      )' ) 'description'
      write ( lunout1 , '(a      )' ) '''                                                            '''
      write ( lunout2 , '(a      )' ) '''                                                            '''
      write ( lunout1 , '(a      )' ) '''                                                            '''
      write ( lunout2 , '(a      )' ) '''                                                            '''
      write ( lunout1 , '(a      )' ) '''                                                            '''
      write ( lunout2 , '(a      )' ) '''                                                            '''
      write ( lunout1 , '(a      )' ) 'end-description'
      write ( lunout2 , '(a      )' ) 'end-description'
      julday = ymd2jul ( itdate )
      write ( lunout1 , '(a,i8,a )' ) 'reference-time           ''',itdate,'000000'''
      write ( lunout2 , '(a,i8,a )' ) 'reference-time           ''',itdate,'000000'''
      timsec = tstart*60.0
      call timdat( julday, timsec, idate, itime )
      write ( lunout1 , '(a,i8,i6.6,a)') 'hydrodynamic-start-time  ''',idate,itime,''''
      write ( lunout2 , '(a,i8,i6.6,a)') 'hydrodynamic-start-time  ''',idate,itime,''''
      timsec = tstop *60.0
      call timdat( julday, timsec, idate, itime )
      write ( lunout1 , '(a,i8,i6.6,a)') 'hydrodynamic-stop-time   ''',idate,itime,''''
      write ( lunout2 , '(a,i8,i6.6,a)') 'hydrodynamic-stop-time   ''',idate,itime,''''
      timsec = dt    *60.0
      call timdat( julday, timsec, idate, itime )
      write ( lunout1 , '(a,i6.6,a )' ) 'hydrodynamic-timestep    ''00000000',itime,''''
      write ( lunout2 , '(a,i6.6,a )' ) 'hydrodynamic-timestep    ''00000000',itime,''''
      write ( lunout1 , '(a,i8,a )' ) 'conversion-ref-time      ''',itdate,'000000'''
      write ( lunout2 , '(a,i8,a )' ) 'conversion-ref-time      ''',itdate,'000000'''
      timsec = max(itwqff*dt*60.0,  tstart*60.0)
      call timdat( julday, timsec, idate, itime )
      write ( lunout1 , '(a,i8,i6.6,a)') 'conversion-start-time    ''',idate,itime,''''
      write ( lunout2 , '(a,i8,i6.6,a)') 'conversion-start-time    ''',idate,itime,''''
      timsec = itwqfl*dt*60.0
      call timdat( julday, timsec, idate, itime )
      write ( lunout1 , '(a,i8,i6.6,a)') 'conversion-stop-time     ''',idate,itime,''''
      write ( lunout2 , '(a,i8,i6.6,a)') 'conversion-stop-time     ''',idate,itime,''''
      timsec = itwqfi*dt*60.0
      call timdat( julday, timsec, idate, itime )
      idate = idate - itdate
      write ( lunout1 , '(a,i8.8,i6.6,a )' ) 'conversion-timestep      ''',idate,itime,''''
      write ( lunout2 , '(a,i8.8,i6.6,a )' ) 'conversion-timestep      ''',idate,itime,''''
      
      write ( lunout1 , '(a,i7   )' ) 'grid-cells-first-direction              ', mmax
      write ( lunout2 , '(a,i7   )' ) 'grid-cells-first-direction              ', mmax
      write ( lunout1 , '(a,i7   )' ) 'grid-cells-second-direction             ', nmax
      write ( lunout2 , '(a,i7   )' ) 'grid-cells-second-direction             ', nmax
      write ( lunout1 , '(a,i7   )' ) 'number-hydrodynamic-layers              ', kmax
      write ( lunout2 , '(a,i7   )' ) 'number-hydrodynamic-layers              ', kmax
      
      write ( lunout2 , '(a,i7   )' ) 'number-horizontal-exchanges             ', noq1+noq2
      write ( lunout2 , '(a,i7   )' ) 'number-vertical-exchanges               ', noq3
      write ( lunout2 , '(a,i7   )' ) 'number-water-quality-segments-per-layer ', noseg/ilaggr(kmax)

      write ( lunout1 , '(a,i7   )' ) 'number-water-quality-layers             ', ilaggr(kmax)
      write ( lunout2 , '(a,i7   )' ) 'number-water-quality-layers             ', ilaggr(kmax)
      filstring = ''''//trim(filnam)//'.dat'''
      write ( lunout1 , '(a,a    )' ) 'hydrodynamic-file        ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'hydrodynamic-file        ',trim(filstring)
      if ( aggre .le. 0 ) then
         write ( lunout1 , '(a,a    )' ) 'aggregation-file         none'
         write ( lunout2 , '(a,a    )' ) 'aggregation-file         none'
      else
         filstring = ''''//flaggr(1:len_trim(flaggr))//''''
         write ( lunout1 , '(a,a    )' ) 'aggregation-file         ',trim(filstring)
         write ( lunout2 , '(a,a    )' ) 'aggregation-file         ',trim(filstring)
      endif
      
      filstring = ''''//trim(filnam)//'.lga'''
      write ( lunout1 , '(a,a    )' ) 'grid-indices-file        ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'grid-indices-file        ',trim(filstring)
      filstring = ''''//trim(filnam)//'.bnd'''
      write ( lunout2 , '(a,a    )' ) 'boundaries-file          ',trim(filstring)
      
      filstring = ''''//trim(filnam)//'.cco'''
      write ( lunout1 , '(a,a    )' ) 'grid-coordinates-file    ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'grid-coordinates-file    ',trim(filstring)
      filstring = ''''//trim(filnam)//'_waqgeom.nc'''
      write ( lunout2 , '(a,a    )' ) 'waqgeom-file             ',trim(filstring)

      filstring = ''''//trim(filnam)//'.vol'''
      write ( lunout1 , '(a,a    )' ) 'volumes-file             ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'volumes-file             ',trim(filstring)

      filstring = ''''//trim(filnam)//'.are'''
      write ( lunout1 , '(a,a    )' ) 'areas-file               ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'areas-file               ',trim(filstring)

      filstring = ''''//trim(filnam)//'.flo'''
      write ( lunout1 , '(a,a    )' ) 'flows-file               ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'flows-file               ',trim(filstring)

      filstring = ''''//trim(filnam)//'.poi'''
      write ( lunout1 , '(a,a    )' ) 'pointers-file            ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'pointers-file            ',trim(filstring)

      filstring = ''''//trim(filnam)//'.len'''
      write ( lunout1 , '(a,a    )' ) 'lengths-file             ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'lengths-file             ',trim(filstring)

      if ( lsal .gt. 0 ) then
         filstring = ''''//trim(filnam)//'.sal'''
      else
         filstring = 'none'
      endif
      write ( lunout1 , '(a,a    )' ) 'salinity-file            ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'salinity-file            ',trim(filstring)
      if ( ltem .gt. 0 ) then
         filstring = ''''//trim(filnam)//'.tem'''
      else
         filstring = 'none'
      endif
      write ( lunout1 , '(a,a    )' ) 'temperature-file         ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'temperature-file         ',trim(filstring)
      do l = 1, lsed
         sf = ".sed00"
         write( sf(5:6), '(i2.2)' ) l
         filstring = ''''//trim(filnam)//sf//''''
         write ( lunout1 , '(A,A )' ) 'sediment-file            ',trim(filstring)
         write ( lunout2 , '(A,A )' ) 'sediment-file            ',trim(filstring)
      enddo
      if ( ilaggr(kmax) .gt. 1 ) then
         filstring = ''''//trim(filnam)//'.vdf'''
      else
         filstring = 'none'
      endif
      write ( lunout1 , '(a,a    )' ) 'vert-diffusion-file      ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'vert-diffusion-file      ',trim(filstring)

      filstring = ''''//trim(filnam)//'.srfold'''
      write ( lunout1 , '(a,a    )' ) 'surfaces-file            ',trim(filstring)
      filstring = ''''//trim(filnam)//'.srf'''
      write ( lunout2 , '(a,a    )' ) 'horizontal-surfaces-file ',trim(filstring)

      filstring = ''''//trim(filnam)//'.dps'''
      write ( lunout1 , '(a,a    )' ) 'depths-file              ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'depths-file              ',trim(filstring)

      filstring = ''''//trim(filnam)//'.lgt'''
      write ( lunout1 , '(a,a    )' ) 'total-grid-file          ',trim(filstring)
      ! write ( lunout2 , '(a,a    )' ) 'total-grid-file          ',trim(filstring) ! not needed for unstructured

      filstring = ''''//trim(filnam)//'.src'''
      write ( lunout1 , '(a,a    )' ) 'discharges-file          ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'discharges-file          ',trim(filstring)

      if ( chez ) then
         filstring = ''''//trim(filnam)//'.chz'''
      else
         filstring = 'none'
      endif
      write ( lunout1 , '(a,a    )' ) 'chezy-coefficients-file  ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'chezy-coefficients-file  ',trim(filstring)

      filstring = ''''//trim(filnam)//'.tau'''
      write ( lunout1 , '(a,a    )' ) 'shear-stresses-file      ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'shear-stresses-file      ',trim(filstring)

      filstring = ''''//trim(filnam)//'.wlk'''
      write ( lunout1 , '(a,a    )' ) 'walking-discharges-file  ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'walking-discharges-file  ',trim(filstring)

      filstring = ''''//trim(filnam)//'.atr'''
      write ( lunout1 , '(a,a    )' ) 'attributes-file          ',trim(filstring)
      write ( lunout2 , '(a,a    )' ) 'attributes-file          ',trim(filstring)

      if (zmodel) then
         write ( lunout1 , '(a, f15.6 )' ) 'z-layers-ztop ', ztop
         write ( lunout2 , '(a, f15.6 )' ) 'z-layers-ztop ', ztop
         write ( lunout1 , '(a, f15.6 )' ) 'z-layers-zbot ', zbot
         write ( lunout2 , '(a, f15.6 )' ) 'z-layers-zbot ', zbot
      endif

      if ( ilaggr(kmax) .gt. 1 ) then
         write ( lunout1 , '(a,a    )' ) 'minimum-vert-diffusion   '
         write ( lunout2 , '(a,a    )' ) 'minimum-vert-diffusion   '
         write ( lunout1 , '(a,e13.4)' ) '   upper-layer    ',   0.0000E+00
         write ( lunout2 , '(a,e13.4)' ) '   upper-layer    ',   0.0000E+00
         write ( lunout1 , '(a,e13.4)' ) '   lower-layer    ',   0.0000E+00
         write ( lunout2 , '(a,e13.4)' ) '   lower-layer    ',   0.0000E+00
         write ( lunout1 , '(a,e13.4)' ) '   interface-depth',   0.0000E+00
         write ( lunout2 , '(a,e13.4)' ) '   interface-depth',   0.0000E+00
         write ( lunout1 , '(a      )' ) 'end-minimum-vert-diffusion'
         write ( lunout2 , '(a      )' ) 'end-minimum-vert-diffusion'
      endif
      write ( lunout1 , '(a      )' ) 'constant-dispersion       '
      write ( lunout2 , '(a      )' ) 'constant-dispersion       '
      write ( lunout1 , '(a,e13.4)' ) '   first-direction ',   0.1000E+01
      write ( lunout2 , '(a,e13.4)' ) '   first-direction ',   0.1000E+01
      write ( lunout1 , '(a,e13.4)' ) '   second-direction',   0.1000E+01
      write ( lunout2 , '(a,e13.4)' ) '   second-direction',   0.1000E+01
      write ( lunout1 , '(a,e13.4)' ) '   third-direction ',   0.1000E-06
      write ( lunout2 , '(a,e13.4)' ) '   third-direction ',   0.1000E-06
      write ( lunout1 , '(a      )' ) 'end-constant-dispersion'
      write ( lunout2 , '(a      )' ) 'end-constant-dispersion'
      write ( lunout1 , '(a      )' ) 'hydrodynamic-layers'
      write ( lunout2 , '(a      )' ) 'hydrodynamic-layers'
      if (zmodel) then
         do i = kmax, 1, -1
            write ( lunout1 , '(f15.8  )' ) thick(i)
            write ( lunout2 , '(f15.8  )' ) thick(i)
         enddo
      else
         do i = 1,kmax
            write ( lunout1 , '(f15.8  )' ) thick(i)
            write ( lunout2 , '(f15.8  )' ) thick(i)
         enddo
      endif
      write ( lunout1 , '(a      )' ) 'end-hydrodynamic-layers'
      write ( lunout2 , '(a      )' ) 'end-hydrodynamic-layers'
      write ( lunout1 , '(a      )' ) 'water-quality-layers   '
      write ( lunout2 , '(a      )' ) 'water-quality-layers   '
      anl = 1.000
      do i = 1,kmax-1
         if ( ilaggr(i+1) .ne. ilaggr(i) ) then
            write ( lunout1 , '(f13.1  )' ) anl
            write ( lunout2 , '(f13.1  )' ) anl
            anl = 1.000
         else
            anl = anl + 1.000
         endif
      enddo
      write ( lunout1 , '(f13.1  )' ) anl
      write ( lunout2 , '(f13.1  )' ) anl
      write ( lunout1 , '(a      )' ) 'end-water-quality-layers'
      write ( lunout2 , '(a      )' ) 'end-water-quality-layers'
      write ( lunout1 , '(a      )' ) 'discharges'
      write ( lunout2 , '(a      )' ) 'sink-sources'
      nowalk = 0
      il     = 1
      do i = 1,nsrc
         if (mnksrc(3,i) == -1) cycle ! awkward disabling of discharges outside partition when running parallel
         if (mnksrc(7,i) == 4 .or. mnksrc(7,i) == 5 .or. mnksrc(7,i) == 8) cycle ! skip e,d and f culverts, output is not correct
         m = mnksrc(1,i)
         n = mnksrc(2,i)
         filstring = ''''//trim(namsrc(i))//''''//' normal'
         if ( mnksrc(7,i) .eq. 1 ) then
            filstring = ''''//trim(namsrc(i))//''''//' walking'
            nowalk = nowalk + 1
            iwlk(nowalk) = i
         endif
         if ( mnksrc(7,i) .eq. 2 ) then
            filstring = ''''//'INLET    '
                              write ( filstring(7:7),'(I1)' ) il
            if ( il .gt.  9 ) write ( filstring(7:8),'(I2)' ) il
            if ( il .gt. 99 ) write ( filstring(7:9),'(I3)' ) il
            filstring = trim(filstring)//' '//trim(namsrc(i))//''''//' inlet'
            il = il + 1
         endif
         if ( mnksrc(7,i) .eq. 3 ) then
            filstring = ''''//'INLET    '
                              write ( filstring(7:7),'(I1)' ) il
            if ( il .gt.  9 ) write ( filstring(7:8),'(I2)' ) il
            if ( il .gt. 99 ) write ( filstring(7:9),'(I3)' ) il
            filstring = trim(filstring)//' '//trim(namsrc(i))//''''//' culvert-inlet'
            il = il + 1
         endif
         call n_and_m_to_nm(mnksrc(2,i), mnksrc(1,i), nm_sink, gdp)
         call n_and_m_to_nm(mnksrc(5,i), mnksrc(4,i), nm_source, gdp)
         if ( zmodel ) then
            kfmin     = kfsmin(n,m)
            ksrwaq(i) = ilaggr(kmax-kfmin+1)     ! store value with source index
            if ( mnksrc(3,i) .eq. 0 ) then ! uniform source over depth
               do ilay = 1, ksrwaq(i)
                  write ( lunout1 , '(3(i0,3x),a)' ) n, m, ilay, trim(filstring)
                  write ( lunout2 , '(3(i0,3x),4es15.7,1x,i0,1x,a)' ) i, nm_sink, nm_source, &
                          xz(mnksrc(2,i), mnksrc(1,i)), yz(mnksrc(2,1), mnksrc(1,i)), &
                          xz(mnksrc(5,i), mnksrc(4,i)), yz(mnksrc(5,i), mnksrc(4,i)), ilay, trim(namsrc(i))
               enddo
            else
               nl = ilaggr(kmax-max(kfmin,mnksrc(3,i))+1)
               write ( lunout1 , '(3(i0,3x),a)' ) n, m, nl, trim(filstring)
               write ( lunout2 , '(3(i0,3x),4es15.7,1x,i0,1x,a)' ) i, nm_sink, nm_source, &
                       xz(mnksrc(2,i), mnksrc(1,i)), yz(mnksrc(2,1), mnksrc(1,i)), &
                       xz(mnksrc(5,i), mnksrc(4,i)), yz(mnksrc(5,i), mnksrc(4,i)), nl, trim(namsrc(i))
            endif
         else
            if ( mnksrc(3,i) .eq. 0 ) then
               write ( lunout1 , '(3(i0,3x),a)' ) n, m, 0, trim(filstring)
            else
               write ( lunout1 , '(3(i0,3x),a)' ) n, m, ilaggr(mnksrc(3,i)), trim(filstring)
            endif
            write ( lunout2 , '(3(i0,3x),4es15.7,1x,a)' ) i, nm_sink, nm_source, &
                    xz(mnksrc(2,i), mnksrc(1,i)), yz(mnksrc(2,1), mnksrc(1,i)), &
                    xz(mnksrc(5,i), mnksrc(4,i)), yz(mnksrc(5,i), mnksrc(4,i)), trim(namsrc(i))
         endif
      enddo
      il = 1
      do i = 1,nsrc
         if (mnksrc(3,i) ==  -1) cycle ! awkward disabling of discharges outside partition when running parallel
         if (mnksrc(7,i) == 4 .or. mnksrc(7,i) == 5 .or. mnksrc(7,i) == 8) cycle ! skip e,d and f culverts, output is not correct
         if (mnksrc(7,i) .le. 1) cycle
         m = mnksrc(4,i)
         n = mnksrc(5,i)
         if ( mnksrc(7,i) .eq. 2 ) then
            filstring = ''''//'OUTLET    '
                              write ( filstring(8: 8),'(I1)' ) il
            if ( il .gt.  9 ) write ( filstring(8: 9),'(I2)' ) il
            if ( il .gt. 99 ) write ( filstring(8:10),'(I3)' ) il
            filstring = trim(filstring)//' '//trim(namsrc(i))//''''//' outlet'
            il = il + 1
         endif
         if ( mnksrc(7,i) .eq. 3 ) then
            filstring = ''''//'OUTLET    '
                              write ( filstring(8: 8),'(I1)' ) il
            if ( il .gt.  9 ) write ( filstring(8: 9),'(I2)' ) il
            if ( il .gt. 99 ) write ( filstring(8:10),'(I3)' ) il
            filstring = trim(filstring)//' '//trim(namsrc(i))//''''//' culvert-outlet'
            il = il + 1
         endif
         call n_and_m_to_nm(mnksrc(2,i), mnksrc(1,i), nm_sink, gdp)
         call n_and_m_to_nm(mnksrc(5,i), mnksrc(4,i), nm_source, gdp)
         if ( zmodel ) then
            kfmin     = kfsmin(n,m)
            ksrwaq(nsrc+i) = ilaggr(kmax-kfmin+1)     ! store value with source index
            if ( mnksrc(6,i) .eq. 0 ) then ! uniform source over depth
               do ilay = 1, ksrwaq(nsrc+i)
                  write ( lunout1 , '(3(i0,3x),a)' ) n, m, ilay, trim(filstring)
                  write ( lunout2 , '(3(i0,3x),4es15.7,1x,i0,2x,a)' ) i, nm_sink, nm_source, &
                          xz(mnksrc(2,i), mnksrc(1,i)), yz(mnksrc(2,1), mnksrc(1,i)), &
                          xz(mnksrc(5,i), mnksrc(4,i)), yz(mnksrc(5,i), mnksrc(4,i)), ilay, trim(namsrc(i))
               enddo
            else
               nl = ilaggr(kmax-max(kfmin,mnksrc(6,i))+1)
               write ( lunout1 , '(3(i0,3x),a)' ) n, m, nl, trim(filstring)
               write ( lunout2 , '(3(i0,3x),4es15.7,1x,i0,2x,a)' ) i, nm_sink, nm_source, &
                       xz(mnksrc(2,i), mnksrc(1,i)), yz(mnksrc(2,1), mnksrc(1,i)), &
                       xz(mnksrc(5,i), mnksrc(4,i)), yz(mnksrc(5,i), mnksrc(4,i)), nl, trim(namsrc(i))
            endif
         else 
            if ( mnksrc(6,i) .eq. 0 ) then
               write ( lunout1 , '(3(i0,3x),a)' ) n, m, 0, trim(filstring)
            else
               write ( lunout1 , '(3(i0,3x),a)' ) n, m, ilaggr(mnksrc(6,i)), trim(filstring)
            endif
            write ( lunout2 , '(3(i0,3x),4es15.7,1x,a)' ) i, nm_sink, nm_source, & 
                    xz(mnksrc(2,i), mnksrc(1,i)), yz(mnksrc(2,1), mnksrc(1,i)), &
                    xz(mnksrc(5,i), mnksrc(4,i)), yz(mnksrc(5,i), mnksrc(4,i)), trim(namsrc(i))
         endif
      enddo
      write ( lunout1 , '(a      )' ) 'end-discharges'
      write ( lunout2 , '(a      )' ) 'end-sink-sources'
      close ( lunout1 )
      close ( lunout2 )
      end subroutine wrwaqhyd
