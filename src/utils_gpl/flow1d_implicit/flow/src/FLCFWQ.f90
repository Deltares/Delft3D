subroutine FLCFWQ(fd_nefis_waq, ngrid, nqlat, grnamd, grnamw,&
&namdes, desdes, itim, psi, elwqi,&
&dlwqts, neferr)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLCFWQ (FLow Create File for Water Quality)
!
! Module description: This function defines a NEFIS file which is being
!                     used to store aggregated flows for the water qual-
!                     ity interface module.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 dafdwq            P  -
!  1 defdwq            P  -
!  8 desdes            P  -
! 12 dlwqts            IO Highest cell number on water quality interface
!                         file. (DELWAQ time step number)
! 11 elwqi             P  -
!  5 grnamd            P  -
!  6 grnamw            P  -
!  9 itim(2)           I  Actual time level tn+1 expressed in date and
!                         time. Format (integer):
!                         itim(1) = YYYYMMDD (year,month,day)
!                         itim(2) = HHMMSSHH (hour,minute,second,
!                                   hundredth of a second)
!  7 namdes            P  -
! 13 neferr            O  Nefis error code:
!                         0   = No error
!                         <0  = Error:  odd  = fatal
!                                       even = non fatal
!  3 ngrid             I  Number of grid points in network.
!  4 nqlat             I  Number of lateral discharge stations.
! 10 psi               I  Space weight factor in Preissmann scheme.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! credat  CREation of a data group in the DATa file
! defcel  DEFinition of a CELl
! defelm  DEFinition of an ELeMent
! defgrp  DEFinition of a GRouP
! flsdef  FLuSh buffers of DEFinition file
! getiel  GET Integer ELement from nefis file
! inqdat  INQuire for info of DATa group on data file
! inqmxi  INQuire for MaXimum Index of data group
! putrel  PUT Real ELement to a nefis file
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flcfwq.pf,v $
! Revision 1.5  1999/03/15  14:19:31  kuipe_j
! improve writing Aggr-file
!
! Revision 1.4  1996/09/03  14:51:50  kuipe_j
! frequency time hist,Messages controllers added
!
! Revision 1.3  1995/05/30  09:54:51  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:47  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:33  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:47  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer       fd_nefis_waq ,itim(2)
   integer       ngrid, nqlat, dlwqts, neferr
   real          psi
   character*16  grnamd, grnamw
   character*16  elwqi(*)
   character*16  namdes(2)
   character*64  desdes(2)
!
!     Nefis variables
!
   integer       error
   integer       credat, defelm, defcel, defgrp, flsdef,&
   &getiel, inqdat, inqmxi, putrel
   external      credat, defelm, defcel, defgrp, flsdef,&
   &getiel, inqdat, inqmxi, putrel
!
!     REAL array CELl definitions
!
   integer       i  ,itimf(2)

   integer, parameter                :: nelwqi = 9
   character*16  clnamd ,clnamw
   integer       ndim(nelwqi), dimw(2,nelwqi), dimd(1), ord(1)
   integer       ibuf(1), uindex(3), ncel, icel
   real          buf(1)
!
!     Group contains discharges, widths, areas and lateral discharges
!

!
!     Af: Flow area channel
!
   ndim (   1) = 1
   dimw (1, 1) = ngrid
!
!     Afs: Flow area sections
!
   ndim (   2) = 2
   dimw (1, 2) = ngrid
   dimw (2, 2) = 2
!
!     At: Total area
!
   ndim (   3) = 1
   dimw (1, 3) = ngrid
!
!     C: Chezy values
!
   ndim (   4) = 2
   dimw (1, 4) = ngrid
   dimw (2, 4) = 4
!
!     DLWQTM: time step
!
   ndim (   5) = 1
   dimw (1, 5) = 2
!
!     Wf: Flow width
!
   ndim (   6) = 1
   dimw (1, 6) = ngrid
!
!     Wfs: Flow width sections
!
   ndim (   7) = 2
   dimw (1, 7) = ngrid
   dimw (2, 7) = 2
!
!     Q aggr: aggregated flows
!
   ndim (   8) = 2
   dimw (1, 8) = ngrid
   dimw (2, 8) = 3
!
!     Q laggr: aggregated lateral discharges
!
   ndim (   9) = 1
   dimw (1, 9) = nqlat
!
!     Definition of Block Descriptor
!
   error  = inqdat (fd_nefis_waq ,grnamd ,grnamd)
!
   if (error .eq. 6004) then
!
!        Data group for description doesn't exist, so
!        define elements, cel and group def.
!        Apparently a new output file starts.
!
      dimd(1) = 0
      error = defelm (fd_nefis_waq, namdes(1), 'INTEGER', 4,&
      &'t', 's', desdes(1), 0, 0)

!
      error = defelm (fd_nefis_waq, namdes(2), 'REAL', 4,&
      &'?', '-', desdes(2), 0, 0)
!
      clnamd = 'WQINT-DES-CEL'
      error  = defcel (fd_nefis_waq, clnamd ,2 ,namdes)
      if (error.ne.0) goto 1000
!
      ord(1)  = 1
      error  = defgrp (fd_nefis_waq, grnamd, clnamd, 0 ,dimd ,ord )
      if (error.ne.0) goto 1000
!
!        Create data group on data file.
!
      error = credat (fd_nefis_waq, grnamd, grnamd)
      if (error.ne.0) goto 1000
!
      dlwqts = 0
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
!
      buf(1) = psi
      error  = PUTREL (fd_nefis_waq  ,grnamd ,namdes(2) ,&
      &uindex  ,ord    ,buf    )
      if (error.ne.0) goto 1000
!
!        Define elements on definition file (Real array variables)
!
      do 100 i = 1, nelwqi
         if (i.eq.5) then
            error = defelm(fd_nefis_waq, elwqi(i), 'INTEGER',4, ' ',&
            &' ', ' ', ndim(i), dimw(1,i))
         else
            error = defelm ( fd_nefis_waq, elwqi(i), 'REAL', 4, ' ',&
            &' ', ' ', ndim(i), dimw(1,i))
         endif
         if (error.ne.0) goto 1000
100   continue
!
!        Define cell which contains the single variables
!
      clnamw = 'WQINT-CEL'
      error = defcel ( fd_nefis_waq, clnamw, nelwqi, elwqi )
      if (error.ne.0) goto 1000
!
!        Define group which contains the single variables
!
      dimd(1) = 0
      ord(1)  = 1
      error = defgrp ( fd_nefis_waq, grnamw, clnamw, 1, dimd, ord )
      if (error.ne.0) goto 1000
!
!        Flush definition
!
      error = flsdef ( fd_nefis_waq )
      if (error.ne.0) goto 1000
!
!        Create data on the data file
!
      error = credat ( fd_nefis_waq, grnamw, grnamw )
      if (error.ne.0) goto 1000
   else
!
!        Data group already exists, so read last cel numbers.
!        Apparently an existing output file will be extended.
!
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      ord(1)    = 0
!
      error = getiel (fd_nefis_waq, grnamd ,namdes(1) ,&
      &uindex  ,ord    ,4      ,ibuf      )
      dlwqts = ibuf(1)
      if (error.ne.0) goto 1000
!
   endif
!
!     Search for starting cel number in case of an existing group.
!     (Function inqmxi must return no error code in that case).
!
   error = inqmxi (fd_nefis_waq, grnamw ,icel )
   if (error.eq.0) then
!
!        dlwqts points to the last written cel. As this cell
!        will be overwritten partly, dlwqts will not be
!        incremented.
!
      icel = min(icel,dlwqts)
   else
      icel = 0
   endif
!
!     Search for cel with a time equal or lower than the starting
!     time.
!
   ncel      = 0
   uindex(3) = 1
   ord(1)    = 1
!
! ----Backwards loop over time steps on file -------->
!
10 continue
   if (icel.gt.1) then
      icel = icel - 1
      uindex(1) = icel
      uindex(2) = icel
      error = getiel (fd_nefis_waq, grnamw ,elwqi(5) ,&
      &uindex  ,ord    ,8      ,itimf    )
      if (error.ne.0) goto 1000
      if (itimf(1).eq.itim(1) .and. itimf(2).eq.itim(2)) then
!           Current time on file is equal to the starting time.
         ncel = icel
         icel = 1
      endif
      goto 10
   endif
!
! <---End loop ------------------------------------
!
   dlwqts = ncel
1000 continue
   neferr = error
!
end
