subroutine SODEMO ( lestu ,nbran ,ngrid ,juer ,ker )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SODEMO (SObek DEclare MOrphology variables)
!
! Module description: Declare variables for usage in the morphology
!                     module.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 juer              P  -
!  5 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  1 lestu             I  Switch to indicate estuary case
!  2 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
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
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sodemo.pf,v $
! Revision 1.7  1998/06/11  11:47:37  kuipe_j
! Estuary special integrated
!
! Revision 1.6  1997/06/17  11:29:20  kuipe_j
! output in history format
!
! Revision 1.5  1996/04/11  08:16:28  kuipe_j
! Kalman module added
!
! Revision 1.4  1995/09/22  10:03:59  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:56:47  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:40  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:05  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:09:38  kuipe_j
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
   logical          lestu
   integer          nbran, ngrid, juer, ker

!
!     Variables
!
   integer          errcod, size, errno
   integer          nmoman
   character*16     name
   character*80     txt
   parameter       (nmoman = 3)
!
!     External functions
!
   integer          mkrpnt, mkipnt, gtipnt, mkdpnt
   external         mkrpnt, mkipnt, gtipnt, mkdpnt
!
!     Include memory pool
!
   include '..\include\mempool.i'
   include '..\include\errcod.i'
!
!     Allocate memory for some averaged hydraulic and sediment
!     parameters. (only for estuarium morphology)
!
   if (lestu) then
!
!        Averaged water levels
!
      size   = 3 * ngrid
      name   = 'AHPACK'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
!
!        Avaraged distributed sediment transports
!
      size   = nbran * 4
      name   = 'ADISSD'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
!
!        Avaraged lateral sediment
!
      size   = ngrid
      name   = 'ASLAT'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
!
!        Avaraged sediment transports
!
      size   = ngrid
      name   = 'ASEDTR'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
!
!        Water levels and discharges
!        characteristic for tidal period
!
      size   = ngrid * 2
      name   = 'HQAV'
      errcod = mkdpnt ( name , size )
      if (errcod .lt. 0) goto 900
   endif
!
!     Pre-codes output
!
   size   = nmoman
   name   = 'MOCPRE'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Output administration
!
   size   = 3
   name   = 'NCELMO'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Define number of output codes
!
   size   = 1
   name   = 'NMOMAN'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value
!
   ip(gtipnt(name)) = nmoman
!
!     Check for opening HIS files
!
   size   = 2
   name   = 'MORINI'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Incread in cross sectional area
!
   size   = ngrid
   name   = 'SUMDA'
   errcod = mkrpnt ( name , size )
   if (errcod .lt. 0) goto 900
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

   txt = 'SODEMO Memory error for @' // name // '@'

   call error ( juer, txt, errno, ker )

   return
end

