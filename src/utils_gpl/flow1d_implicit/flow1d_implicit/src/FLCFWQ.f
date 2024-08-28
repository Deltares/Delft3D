      subroutine FLCFWQ(fd_nefis_waq, ngrid, nqlat, grnamd, grnamw,
     +                  namdes, desdes, itim, psi, elwqi,
     +                  dlwqts, neferr)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLCFWQ (FLow Create File for Water Quality)
c
c Module description: This function defines a NEFIS file which is being
c                     used to store aggregated flows for the water qual-
c                     ity interface module.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 dafdwq            P  -
c  1 defdwq            P  -
c  8 desdes            P  -
c 12 dlwqts            IO Highest cell number on water quality interface
c                         file. (DELWAQ time step number)
c 11 elwqi             P  -
c  5 grnamd            P  -
c  6 grnamw            P  -
c  9 itim(2)           I  Actual time level tn+1 expressed in date and
c                         time. Format (integer):
c                         itim(1) = YYYYMMDD (year,month,day)
c                         itim(2) = HHMMSSHH (hour,minute,second,
c                                   hundredth of a second)
c  7 namdes            P  -
c 13 neferr            O  Nefis error code:
c                         0   = No error
c                         <0  = Error:  odd  = fatal
c                                       even = non fatal
c  3 ngrid             I  Number of grid points in network.
c  4 nqlat             I  Number of lateral discharge stations.
c 10 psi               I  Space weight factor in Preissmann scheme.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c credat  CREation of a data group in the DATa file
c defcel  DEFinition of a CELl
c defelm  DEFinition of an ELeMent
c defgrp  DEFinition of a GRouP
c flsdef  FLuSh buffers of DEFinition file
c getiel  GET Integer ELement from nefis file
c inqdat  INQuire for info of DATa group on data file
c inqmxi  INQuire for MaXimum Index of data group
c putrel  PUT Real ELement to a nefis file
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flcfwq.pf,v $
c Revision 1.5  1999/03/15  14:19:31  kuipe_j
c improve writing Aggr-file
c
c Revision 1.4  1996/09/03  14:51:50  kuipe_j
c frequency time hist,Messages controllers added
c
c Revision 1.3  1995/05/30  09:54:51  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:47  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:33  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:47  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer       fd_nefis_waq ,itim(2)
      integer       ngrid, nqlat, dlwqts, neferr
      real          psi
      character(len=16)  grnamd, grnamw
      character(len=16)  elwqi(*)
      character(len=16)  namdes(2)
      character(len=64)  desdes(2)
c
c     Nefis variables
c
      integer       error
      integer       credat, defelm, defcel, defgrp, flsdef,
     &              getiel, inqdat, inqmxi, putrel
      external      credat, defelm, defcel, defgrp, flsdef,
     &              getiel, inqdat, inqmxi, putrel
c
c     REAL array CELl definitions
c
      integer       i  ,itimf(2)

      integer, parameter                :: nelwqi = 9
      character(len=16) clnamd ,clnamw
      integer       ndim(nelwqi), dimw(2,nelwqi), dimd(1), ord(1)
      integer       ibuf(1), uindex(3), ncel, icel
      real          buf(1)
c
c     Group contains discharges, widths, areas and lateral discharges
c

c
c     Af: Flow area channel
c
      ndim (   1) = 1
      dimw (1, 1) = ngrid
c
c     Afs: Flow area sections
c
      ndim (   2) = 2
      dimw (1, 2) = ngrid
      dimw (2, 2) = 2
c
c     At: Total area
c
      ndim (   3) = 1
      dimw (1, 3) = ngrid
c
c     C: Chezy values
c
      ndim (   4) = 2
      dimw (1, 4) = ngrid
      dimw (2, 4) = 4
c
c     DLWQTM: time step
c
      ndim (   5) = 1
      dimw (1, 5) = 2
c
c     Wf: Flow width
c
      ndim (   6) = 1
      dimw (1, 6) = ngrid
c
c     Wfs: Flow width sections
c
      ndim (   7) = 2
      dimw (1, 7) = ngrid
      dimw (2, 7) = 2
c
c     Q aggr: aggregated flows
c
      ndim (   8) = 2
      dimw (1, 8) = ngrid
      dimw (2, 8) = 3
c
c     Q laggr: aggregated lateral discharges
c
      ndim (   9) = 1
      dimw (1, 9) = nqlat
c
c     Definition of Block Descriptor
c
      error  = inqdat (fd_nefis_waq ,grnamd ,grnamd)
c
      if (error .eq. 6004) then
c
c        Data group for description doesn't exist, so
c        define elements, cel and group def.
c        Apparently a new output file starts.
c
         dimd(1) = 0
         error = defelm (fd_nefis_waq, namdes(1), 'INTEGER', 4,
     &                   't', 's', desdes(1), 0, 0)

c
         error = defelm (fd_nefis_waq, namdes(2), 'REAL', 4,
     &                   '?', '-', desdes(2), 0, 0)
c
         clnamd = 'WQINT-DES-CEL'
         error  = defcel (fd_nefis_waq, clnamd ,2 ,namdes)
         if (error.ne.0) goto 1000
c
         ord(1)  = 1
         error  = defgrp (fd_nefis_waq, grnamd, clnamd, 0 ,dimd ,ord )
         if (error.ne.0) goto 1000
c
c        Create data group on data file.
c
         error = credat (fd_nefis_waq, grnamd, grnamd)
         if (error.ne.0) goto 1000
c
         dlwqts = 0
         uindex(1) = 1
         uindex(2) = 1
         uindex(3) = 1
c
         buf(1) = psi
         error  = PUTREL (fd_nefis_waq  ,grnamd ,namdes(2) ,
     &                    uindex  ,ord    ,buf    )
         if (error.ne.0) goto 1000
c
c        Define elements on definition file (Real array variables)
c
         do 100 i = 1, nelwqi
            if (i.eq.5) then
               error = defelm(fd_nefis_waq, elwqi(i), 'INTEGER',4, ' ',
     +                          ' ', ' ', ndim(i), dimw(1,i))
            else
               error = defelm ( fd_nefis_waq, elwqi(i), 'REAL', 4, ' ',
     +                          ' ', ' ', ndim(i), dimw(1,i))
            endif
            if (error.ne.0) goto 1000
  100    continue
c
c        Define cell which contains the single variables
c
         clnamw = 'WQINT-CEL'
         error = defcel ( fd_nefis_waq, clnamw, nelwqi, elwqi )
         if (error.ne.0) goto 1000
c
c        Define group which contains the single variables
c
         dimd(1) = 0
         ord(1)  = 1
         error = defgrp ( fd_nefis_waq, grnamw, clnamw, 1, dimd, ord )
         if (error.ne.0) goto 1000
c
c        Flush definition
c
         error = flsdef ( fd_nefis_waq )
         if (error.ne.0) goto 1000
c
c        Create data on the data file
c
         error = credat ( fd_nefis_waq, grnamw, grnamw )
         if (error.ne.0) goto 1000
      else
c
c        Data group already exists, so read last cel numbers.
c        Apparently an existing output file will be extended.
c
        uindex(1) = 1
        uindex(2) = 1
        uindex(3) = 1
        ord(1)    = 0
c
         error = getiel (fd_nefis_waq, grnamd ,namdes(1) ,
     &                   uindex  ,ord    ,4      ,ibuf      )
         dlwqts = ibuf(1)
         if (error.ne.0) goto 1000
c
      endif
c
c     Search for starting cel number in case of an existing group.
c     (Function inqmxi must return no error code in that case).
c
      error = inqmxi (fd_nefis_waq, grnamw ,icel )
      if (error.eq.0) then
c
c        dlwqts points to the last written cel. As this cell
c        will be overwritten partly, dlwqts will not be
c        incremented.
c
         icel = min(icel,dlwqts)
      else
         icel = 0
      endif
c
c     Search for cel with a time equal or lower than the starting
c     time.
c
      ncel      = 0
      uindex(3) = 1
      ord(1)    = 1
c
c ----Backwards loop over time steps on file -------->
c
   10 continue
      if (icel.gt.1) then
         icel = icel - 1
         uindex(1) = icel
         uindex(2) = icel
         error = getiel (fd_nefis_waq, grnamw ,elwqi(5) ,
     &                   uindex  ,ord    ,8      ,itimf    )
         if (error.ne.0) goto 1000
         if (itimf(1).eq.itim(1) .and. itimf(2).eq.itim(2)) then
c           Current time on file is equal to the starting time.
            ncel = icel
            icel = 1
         endif
         goto 10
      endif
c
c <---End loop ------------------------------------
c
      dlwqts = ncel
 1000 continue
      neferr = error
c
      end
