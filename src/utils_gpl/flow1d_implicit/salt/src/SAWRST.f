      subroutine sawrst (fd_nefis_rst, grnams ,nentri ,nbran  ,nboun ,
     &                   ngrid  ,dsopt  ,ncelst ,itim   ,curtim ,nameel,
     &                   csa2   ,csd2   ,cdcdx0 ,cdcdx1 ,cdcdx2 ,thasca,
     &                   mouqpu ,thcsum ,nmouth ,timout ,sbdscr ,sbdpar,
     &                   neferr)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SAWRST (SAlt WRiting of reSTart information)
c
c Module description: Writing  of restart information.
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
c  8 dsopt             I  Option for dispersion for the whole network:
c                           cds1fu (1) : One function of place or time
c                           cds2fu (2) : Two functions of place or time
c                           cdsthh (3) : Thatcher-Harleman formulation
c                           cdsemp (4) : Empirical formulation
c  3 grnams            P  -
c 10 itim              P  -
c 10 curtim            P  -
c 18 mouqpu            P  -
c 11 nameel            P  -
c  6 nboun             I  Number of boundary nodes.
c  5 nbran             I  Number of branches.
c  9 ncelst            I  Actual cell number of a restart block of the
c                         restart file.
c 22 neferr            O  Nefis error code:
c                         0   = No error
c                         <0  = Error:  odd  = fatal
c                                       even = non fatal
c  4 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c  7 ngrid             I  Number of grid points in network.
c  2 nmouth            I  Maximum number of mouths in the network.
c 21 sbdscr            P  -
c 21 sbdpar            P  -
c 17 thasca            P  -
c 19 thcsum            P  -
c 20 timout            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flsdat  FLuSh buffers of DATa file
c putiel  PUT Integer ELement to nefis file
c putrel  PUT Real ELement to a nefis file
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
c $Log: sawrst.pf,v $
c Revision 1.6  1999/03/15  15:53:29  kuipe_j
c tabs removed
c
c Revision 1.5  1996/12/04  12:00:40  kuipe_j
c declarations / undefined vars
c
c Revision 1.4  1996/12/02  15:31:44  kuipe_j
c Salt restart improved
c
c Revision 1.3  1995/05/30  09:56:20  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:06:25  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:06  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:15  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:17  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer       nentri    ,nbran    ,nboun  ,ngrid  ,dsopt ,
     &              ncelst    ,neferr   ,nmouth
      integer       fd_nefis_rst, itim(2)
      real          csa2  (ngrid)    ,csd2  (ngrid) ,
     &              cdcdx0(ngrid)    ,cdcdx1(ngrid)   ,cdcdx2(ngrid)  ,
     &              thasca(3    )    ,mouqpu(3,0:2,nmouth)   ,curtim  ,
     &              thcsum(2,nbran)  ,timout(2,nmouth),sbdscr(3,nboun),
     &              sbdpar(5,nboun) 
      character*(*) grnams
      character*(*) nameel(nentri)
c
c     Declaration of local variables
c
      integer       error
      integer       uindex(3) ,usrord(1)
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Declaration of external functions
c
      integer       flsdat    ,putrel    ,putiel
      external      flsdat    ,putrel    ,putiel
c
      data          usrord    /1/
c
c     In SOBEK a restart simulation always starts with zero time. 
c     Therefore typical times for the Thatcher Harleman boundary 
c     condition and Thatcher Harleman dispersion formulae should be 
c     corrected before storing them on the restart file.                      
c       
      call sarlti (-curtim , nboun , sbdpar , sbdscr , 
     *              dsopt  , thasca, nmouth , timout             )
c
      uindex(1) = ncelst
      uindex(2) = ncelst
      uindex(3) = 1
c
      error   = putiel (fd_nefis_rst, grnams ,nameel(1) ,
     &                  uindex  ,usrord ,itim   )
      if (error.ne.0) goto 1000
c
      error = putrel (fd_nefis_rst, grnams ,nameel(2) ,
     &                uindex  ,usrord ,csa2   )
      if (error.ne.0) goto 1000
c
      error = putrel (fd_nefis_rst, grnams ,nameel(3) ,
     &                uindex  ,usrord ,csd2   )
      if (error.ne.0) goto 1000
c
      if (dsopt .eq. cdsthh .or. dsopt .eq. cdsemp) then
c
         error = putrel (fd_nefis_rst, grnams ,nameel(4) ,
     &                   uindex  ,usrord ,cdcdx0 )
         if (error.ne.0) goto 1000
c
         error = putrel (fd_nefis_rst, grnams ,nameel(5) ,
     &                   uindex  ,usrord ,cdcdx1 )
         if (error.ne.0) goto 1000
c
         error = putrel (fd_nefis_rst, grnams ,nameel(6) ,
     &                   uindex  ,usrord ,cdcdx2 )
         if (error.ne.0) goto 1000
c
         error = putrel (fd_nefis_rst, grnams ,nameel(7) ,
     &                   uindex  ,usrord ,thcsum )
         if (error.ne.0) goto 1000
c
         error = putrel (fd_nefis_rst, grnams ,nameel(8) ,
     &                   uindex  ,usrord ,mouqpu )
         if (error.ne.0) goto 1000
c
         error = putrel (fd_nefis_rst, grnams ,nameel(9) ,
     &                   uindex  ,usrord ,timout )
         if (error.ne.0) goto 1000
c
         error = putrel (fd_nefis_rst, grnams ,nameel(10) ,
     &                   uindex  ,usrord ,thasca )
         if (error.ne.0) goto 1000
c
      endif
c
      if (nboun.gt.0) then
         error = putrel (fd_nefis_rst, grnams ,nameel(11) ,
     &                   uindex  ,usrord ,sbdscr )
         if (error.ne.0) goto 1000
      endif
c
      error = flsdat(fd_nefis_rst)
c
 1000 continue
c
c     During a simulation more restart times may be written.
c     So, a reset of corrected Thatcher Harleman times (see above) 
c     is required.
c
      call sarlti (+curtim , nboun , sbdpar , sbdscr , 
     *              dsopt  , thasca, nmouth , timout            )

      neferr = error
      end
