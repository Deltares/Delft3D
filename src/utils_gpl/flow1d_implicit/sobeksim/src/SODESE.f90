subroutine SODESE ( nbran, ngrid, nsedrd , nbrnod,&
&juer , lmorp, ker  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SODESE (SObek DEclare SEdiment transport variables)
!
! Module description: Declare variables for usage in the sediment trans-
!                     port module.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  5 juer              P  -
!  6 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  1 nbran             I  Number of branches.
!  4 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  2 ngrid             I  Number of grid points in network.
!  3 nsedrd            I  Number of defined sedredge branches.
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
! $Log: sodese.pf,v $
! Revision 1.8  1999/03/15  15:19:42  kuipe_j
! tabs removed
!
! Revision 1.7  1997/08/21  12:48:54  kuipe_j
! Decl for only sediment
!
! Revision 1.6  1997/06/17  11:29:22  kuipe_j
! output in history format
!
! Revision 1.5  1996/03/07  10:44:28  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.4  1995/09/22  10:04:02  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:56:48  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:41  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:07  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:09:41  kuipe_j
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
   integer          nbran, ngrid, nsedrd, juer, ker, nbrnod
!
!     Variables
!
   integer          errcod, size, errno
   integer          nseman
   character*16     name
   character*80     txt
   logical          lmorp
   parameter       (nseman = 3)
!
!     External functions
!
   integer          mkipnt, mkrpnt, gtipnt
   external         mkipnt, mkrpnt, gtipnt
!
!     Include memory pool
!
   include '..\include\mempool.i'
   include '..\include\errcod.i'
!
!     Numbers of outflowing branches
!
   size   = 3 * nbrnod
   name   = 'BGOUT'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Celerities
!
   if (nsedrd .eq. 0) then
      size = ngrid
   else
      size = ngrid * 2
   endif
!
   name   = 'CELER'
   errcod = mkrpnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Distributed sediment transports in nodes
!
   size   = nbran * 4
   name   = 'DISSED'
   errcod = mkrpnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Indicator array for flow direction at gripoints
!
   size = ngrid
   name = 'FLWDIR'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Constants for transport formulas
!
   if (nsedrd .eq. 0) then
      size   = 4 * ngrid
   else
      size   = 4 * ngrid * 3
   endif
   name   = 'FORCON'
   errcod = mkrpnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Output administration
!
   size   = 3
   name   = 'NCELSE'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Define number of output codes
!
   size   = 1
   name   = 'NSEMAN'
   errcod = mkipnt ( 'NSEMAN', size )
   if (errcod .lt. 0) goto 900
!
!     Assign value
!
   ip(gtipnt(name)) = nseman
!
!     Pre-codes output
!
   size   = nseman
   name   = 'SECPRE'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Sediment transports
!
   if (nsedrd .eq. 0) then
      size = ngrid
   else
      size = ngrid * 3
   endif
!
   name   = 'SEDTR'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Lateral sediment
!
   if (nsedrd .eq. 0) then
      size = ngrid
   else
      size = ngrid * 2
   endif
!
   name   = 'SLAT'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Check for opening HIS files
!
   size   = 2
   name   = 'SEDINI'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     These arrays are not present in case of UI, so make
!     dummy arrays
   if (.not.lmorp) then
      size   = 1
      name   = 'SDRDBF'
      errcod = mkipnt ( name , size )
      name   = 'MBDPAR'
      errcod = mkipnt ( name , size )
   endif
   size   = 1
   name   = 'RC'
   errcod = mkrpnt ( name , size )

   return

!
!     Error handler
!
900 continue

   ker = fatal

   if (errcod .eq. -1) errno = evrdec
   if (errcod .eq. -2) errno = eoutds
   if (errcod .eq. -3) errno = eoutns

   txt = 'SODESE Memory error for @' // name // '@'

   call error ( juer, txt, errno, ker )

   return
end
