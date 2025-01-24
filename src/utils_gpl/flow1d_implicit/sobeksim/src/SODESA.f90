subroutine SODESA ( dsopt, nbran, nboun, ngrid, nmouth,&
&juer,  ker&
&)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SODESA (SObek DEclare SAlt variables)
!
! Module description: Declare variables for usage in the salt module.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 dsopt             I  Option for dispersion for the whole network:
!                           cds1fu (1) : One function of place or time
!                           cds2fu (2) : Two functions of place or time
!                           cdsthh (3) : Thatcher-Harleman formulation
!                           cdsemp (4) : Empirical formulation
!  6 juer              P  -
!  7 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  3 nboun             I  Number of boundary nodes.
!  2 nbran             I  Number of branches.
!  4 ngrid             I  Number of grid points in network.
!  5 nmouth            I  Maximum number of mouths in the network.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! gtipnt  GeT Integer PoiNTer
! mkipnt  MaKe Integer PoiNTer
! mkrpnt  MaKe Real PoiNTer
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sodesa.pf,v $
! Revision 1.6  1999/03/15  15:19:41  kuipe_j
! tabs removed
!
! Revision 1.5  1997/06/17  11:29:21  kuipe_j
! output in history format
!
! Revision 1.4  1995/09/22  10:04:00  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:56:48  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:41  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:06  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:09:39  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:41  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
!     Parameters
!
   integer          dsopt, nbran, nboun, ngrid, nmouth,&
   &juer,  ker
!
!     Variables
!
   integer          errcod, size, errno
   integer          nsaman
   character*16     name
   character*80     txt
   parameter       (nsaman = 6)
!
!     External functions
!
   integer          mkrpnt, mkipnt, gtipnt
   external         mkrpnt, mkipnt, gtipnt
!
!     Include memory pool
!
   include '..\include\mempool.i'
   include '..\include\errcod.i'
!
!     DC/DX term info
!
   size   = ngrid * 3
   name   = 'CDCDX'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Diffusion
!
   size   = ngrid * 2
   name   = 'CSD'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Dispersion coefficient for each gridpoint on time t(n+1)
!
   size   = ngrid
   name   = 'DISGR'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Mouth info
!
   if (dsopt .gt. 2) then
      size = 3 * 3 * nmouth
   else
      size = 1
   endif
   name = 'MOUQPU'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Cel-numbers output and restart file
!
   size   = 3
   name   = 'NCELSA'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Number of main output codes salt module
!
   size   = 1
   name   = 'NSAMAN'
   errcod = mkipnt ( name, size )
   if ( errcod .lt. 0) goto 900
!
!     Assign value
!
   ip(gtipnt(name)) = nsaman
!
!     Pre-codes output module
!
   size   = nsaman
   name   = 'SACPRE'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Intermediate results at salt boundaries
!
   size   = 3 * nboun
   name   = 'SBDSCR'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Administration for calculation of <c/c0*dc/dx>
!
   size   = 3
   name   = 'THASCA'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Thatcher-Harleman constant sum per branch
!
   if (dsopt .gt. 2) then
      size = nbran * 2
   else
      size = 1
   endif
   name   = 'THCSUM'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Administration for the calculation of fresh water discharge,
!     flood volume and maximum flood velocity for every mouth
!
   if (dsopt .gt. 2) then
      size = nmouth * 2
   else
      size = 1
   endif
   name   = 'TIMOUT'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Check for opening HIS files
!
   size   = 2
   name   = 'SALINI'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!

   return

!
!     Error handler
!
900 continue

   ker = fatal

   if (errcod .eq. -1) errno = evrdec
   if (errcod .eq. -2) errno = eoutds
   if (errcod .eq. -3) errno = eoutns

   txt = 'SODESA Memory error for @' // name // '@'

   call error ( juer, txt, errno, ker )

   return
end
