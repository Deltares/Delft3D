      subroutine SODESA ( dsopt, nbran, nboun, ngrid, nmouth,
     +                    juer,  ker
     +                  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SODESA (SObek DEclare SAlt variables)
c
c Module description: Declare variables for usage in the salt module.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 dsopt             I  Option for dispersion for the whole network:
c                           cds1fu (1) : One function of place or time
c                           cds2fu (2) : Two functions of place or time
c                           cdsthh (3) : Thatcher-Harleman formulation
c                           cdsemp (4) : Empirical formulation
c  6 juer              P  -
c  7 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  3 nboun             I  Number of boundary nodes.
c  2 nbran             I  Number of branches.
c  4 ngrid             I  Number of grid points in network.
c  5 nmouth            I  Maximum number of mouths in the network.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c gtipnt  GeT Integer PoiNTer
c mkipnt  MaKe Integer PoiNTer
c mkrpnt  MaKe Real PoiNTer
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
c $Log: sodesa.pf,v $
c Revision 1.6  1999/03/15  15:19:41  kuipe_j
c tabs removed
c
c Revision 1.5  1997/06/17  11:29:21  kuipe_j
c output in history format
c
c Revision 1.4  1995/09/22  10:04:00  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:56:48  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:41  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:06  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:09:39  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:41  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Parameters
c
      integer          dsopt, nbran, nboun, ngrid, nmouth,
     +                 juer,  ker
c
c     Variables
c
      integer          errcod, size, errno
      integer          nsaman
      character*16     name
      character*80     txt
      parameter       (nsaman = 6)
c
c     External functions
c
      integer          mkrpnt, mkipnt, gtipnt
      external         mkrpnt, mkipnt, gtipnt
c
c     Include memory pool
c
      include '..\include\mempool.i'
      include '..\include\errcod.i'
c
c     DC/DX term info
c
      size   = ngrid * 3
      name   = 'CDCDX'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Diffusion
c
      size   = ngrid * 2
      name   = 'CSD'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Dispersion coefficient for each gridpoint on time t(n+1)
c
      size   = ngrid
      name   = 'DISGR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Mouth info
c
      if (dsopt .gt. 2) then
         size = 3 * 3 * nmouth
      else
         size = 1
      endif
      name = 'MOUQPU'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Cel-numbers output and restart file
c
      size   = 3
      name   = 'NCELSA'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Number of main output codes salt module
c
      size   = 1
      name   = 'NSAMAN'
      errcod = mkipnt ( name, size )
      if ( errcod .lt. 0) goto 900
c
c     Assign value
c
      ip(gtipnt(name)) = nsaman
c
c     Pre-codes output module
c
      size   = nsaman
      name   = 'SACPRE'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Intermediate results at salt boundaries
c
      size   = 3 * nboun
      name   = 'SBDSCR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Administration for calculation of <c/c0*dc/dx>
c
      size   = 3
      name   = 'THASCA'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Thatcher-Harleman constant sum per branch
c
      if (dsopt .gt. 2) then
         size = nbran * 2
      else
         size = 1
      endif
      name   = 'THCSUM'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Administration for the calculation of fresh water discharge,
c     flood volume and maximum flood velocity for every mouth
c
      if (dsopt .gt. 2) then
         size = nmouth * 2
      else
         size = 1
      endif
      name   = 'TIMOUT'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Check for opening HIS files
c
      size   = 2      
      name   = 'SALINI'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c

      return

c
c     Error handler
c
 900  continue

      ker = fatal

      if (errcod .eq. -1) errno = evrdec
      if (errcod .eq. -2) errno = eoutds
      if (errcod .eq. -3) errno = eoutns

      txt = 'SODESA Memory error for @' // name // '@'

      call error ( juer, txt, errno, ker )

      return
      end
