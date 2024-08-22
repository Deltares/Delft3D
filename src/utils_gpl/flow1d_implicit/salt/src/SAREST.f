      subroutine sarest (fd_nefis_rst, grnams ,nentri ,nbran  ,nboun ,
     &                   nmouth ,ngrid  ,dsopt  ,ncelst ,nameel ,csa2  ,
     &                   csd2   ,cdcdx0 ,cdcdx1 ,cdcdx2 ,thasca ,mouqpu,
     &                   thcsum ,timout ,sbdscr ,neferr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SAREST (SAlt REading of reSTart information)
c
c Module description: Reading  of restart information.
c
c                     The group definition depends a.o. on the selected
c                     dispersion formulation.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 14 cdcdx0            P  -
c 15 cdcdx1            P  -
c 16 cdcdx2            P  -
c 12 csa2              P  -
c 13 csd2              P  -
c  2 dafdst            P  -
c  1 defdst            P  -
c  9 dsopt             I  Option for dispersion for the whole network:
c                           cds1fu (1) : One function of place or time
c                           cds2fu (2) : Two functions of place or time
c                           cdsthh (3) : Thatcher-Harleman formulation
c                           cdsemp (4) : Empirical formulation
c  3 grnams            P  -
c 18 mouqpu            P  -
c 11 nameel            P  -
c  6 nboun             I  Number of boundary nodes.
c  5 nbran             I  Number of branches.
c 10 ncelst            I  Actual cell number of a restart block of the
c                         restart file.
c 22 neferr            O  Nefis error code:
c                         0   = No error
c                         <0  = Error:  odd  = fatal
c                                       even = non fatal
c  4 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c  8 ngrid             I  Number of grid points in network.
c  7 nmouth            I  Maximum number of mouths in the network.
c 21 sbdscr            P  -
c 17 thasca            P  -
c 19 thcsum            P  -
c 20 timout            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c getrel  GET Real ELement from a nefis file
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sarest.pf,v $
c Revision 1.2  1995/05/30  07:06:12  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:54  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:33:56  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:15  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer       nentri    ,nbran    ,nboun  ,nmouth ,ngrid  ,dsopt ,
     &              ncelst    ,neferr
      integer       fd_nefis_rst
      real          csa2  (ngrid)    ,csd2  (ngrid) ,
     &              cdcdx0(ngrid)    ,cdcdx1(ngrid)   ,cdcdx2(ngrid) ,
     &              thasca(3    )    ,mouqpu(3,0:2,*) ,
     &              thcsum(2,nbran)  ,timout(2,*)     ,sbdscr(3,nboun)
      character*(*) grnams
      character*(*) nameel(nentri)
c
c     Declaration of local variables
c
      integer       error     ,buflen
      integer       uindex(3) ,usrord(1)
c
c     Declaration of external functions
c
      integer       getrel
      external      getrel
c
      data          usrord    /1/
c
      uindex(1) = ncelst
      uindex(2) = ncelst
      uindex(3) = 1
c
      buflen    = ngrid*4
c
      error = getrel (fd_nefis_rst, grnams ,nameel(2) ,
     &                uindex  ,usrord ,buflen ,csa2   )
      if (error.ne.0) goto 1000
c
      error = getrel (fd_nefis_rst, grnams ,nameel(3) ,
     &                uindex  ,usrord ,buflen ,csd2   )
      if (error.ne.0) goto 1000
c
      if (dsopt.eq.3 .or. dsopt.eq.4) then
c
         error = getrel (fd_nefis_rst, grnams ,nameel(4) ,
     &                   uindex  ,usrord ,buflen ,cdcdx0  )
         if (error.ne.0) goto 1000
c
         error = getrel (fd_nefis_rst, grnams ,nameel(5) ,
     &                   uindex  ,usrord ,buflen ,cdcdx1  )
         if (error.ne.0) goto 1000
c
         error = getrel (fd_nefis_rst, grnams ,nameel(6) ,
     &                   uindex  ,usrord ,buflen ,cdcdx2  )
         if (error.ne.0) goto 1000
c
         buflen = nbran*2*4
         error  = getrel (fd_nefis_rst, grnams ,nameel(7) ,
     &                    uindex  ,usrord ,buflen ,thcsum  )
         if (error.ne.0) goto 1000
c
         buflen = nmouth*3*3*4
         error  = getrel (fd_nefis_rst, grnams ,nameel(8) ,
     &                    uindex  ,usrord ,buflen ,mouqpu  )
         if (error.ne.0) goto 1000
c
         buflen = nmouth*2*4
         error  = getrel (fd_nefis_rst, grnams ,nameel(9) ,
     &                    uindex  ,usrord ,buflen ,timout  )
         if (error.ne.0) goto 1000
c
         buflen = 3*4
         error  = getrel (fd_nefis_rst, grnams ,nameel(10) ,
     &                    uindex  ,usrord ,buflen ,thasca   )
         if (error.ne.0) goto 1000
c
      endif
c
      if (nboun.gt.0) then
         buflen = nboun*3*4
         error  = getrel (fd_nefis_rst, grnams ,nameel(11) ,
     &                    uindex  ,usrord ,buflen ,sbdscr   )
         if (error.ne.0) goto 1000
      endif
c
 1000 continue
      neferr = error
      end
