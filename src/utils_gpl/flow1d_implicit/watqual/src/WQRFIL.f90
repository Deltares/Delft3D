subroutine wqrfil (fd_nefis_waq, ngrid  ,nqlat  ,&
&dlwqts ,af     ,afs    ,at     ,&
&cp     ,dlwqtm ,wt     ,wfs    ,&
&qaggr  ,qlaggr ,juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQRFIL (Water Quality Read FILe)
!
! Module description: This routine read a record from the interface file
!                     created by the flow module.
!
!                     The next cell is read from the interface file to
!                     be processed by the interface routines.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 af                P  -
!  7 afs               P  -
!  8 cp                P  -
!  2 datfds            P  -
!  1 deffds            P  -
!  9 dlwqtm            P  -
!  5 dlwqts            I  Current step counter for interface file.
! 14 juer              P  -
! 15 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  3 ngrid             I  Number of grid points in network.
!  4 nqlat             I  Number of lateral discharge stations.
! 12 qaggr             P  -
! 13 qlaggr            P  -
! 10 wf                P  -
! 11 wfs               P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! getiel  GET Integer ELement from nefis file
! getrel  GET Real ELement from a nefis file
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
! $Log: wqrfil.pf,v $
! Revision 1.4  1999/03/12  12:42:25  kuipe_j
! parallel segments added
!
! Revision 1.3  1995/05/30  09:56:38  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:08:42  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:11:03  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:51  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:28  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer       fd_nefis_waq
   integer       ngrid, nqlat, dlwqts, dlwqtm(2)
   integer       juer, ker

   real          af     (ngrid),&
   &afs    (ngrid,2),&
   &at     (ngrid),&
   &cp     (ngrid,4),&
   &wt     (ngrid),&
   &wfs    (ngrid,2),&
   &qaggr  (ngrid,3),&
   &qlaggr (*)
!
!     Variables
!
   integer       errcod

   integer       start, stop, incr, ncell, i, nrcel
   parameter     (start=1, stop=2, incr=3, ncell=9)
   integer       usrord(5), uindex(3,5)
   character*16  ecell (ncell), grpnam
   character*40  txt1
   character*8   txt2
   integer       lcell (ncell)
!
!     Include sobek error code file
!
   include '..\include\errcod.i'

   integer       getrel, getiel
   external      getrel, getiel
!
!     Set user order
!
   do 100 i = 1, 5
      usrord (i) = i
100 continue

!
!     Set user index (Group dimensioning)
!
   uindex (start, 1) = dlwqts
   uindex (stop,  1) = dlwqts
   uindex (incr,  1) = 1

   ecell(1)  = 'AF'
   lcell(1)  = ngrid * 4

   ecell(2)  = 'AFS'
   lcell(2)  = ngrid * 2 * 4

   ecell(3)  = 'AT'
   lcell(3)  = ngrid * 4

   ecell(4)  = 'C'
   lcell(4)  = ngrid * 4 * 4

   ecell(5)  = 'DLWQTM'
   lcell(5)  = 2 * 4

   ecell(6)  = 'WT'
   lcell(6)  = ngrid * 4

   ecell(7)  = 'WFS'
   lcell(7)  = ngrid * 2 * 4

   ecell(8)  = 'QAGGR'
   lcell(8)  = ngrid * 3 * 4

   ecell(9)  = 'QLAGGR'
   lcell(9)  = nqlat * 4
!
!     Read each array from the data file
!
   if (nqlat .eq. 0) then
      nrcel = ncell - 1
   else
      nrcel = ncell
   endif
!
   grpnam = 'WQINT-GROUP'
   do 200 i = 1, nrcel
      if (ecell(i) .eq. 'AF') then
         errcod = getrel(fd_nefis_waq, grpnam  , ecell(i),&
         &uindex, usrord, lcell(i), af)

      elseif (ecell(i) .eq. 'AFS') then
         errcod = getrel(fd_nefis_waq, grpnam  , ecell(i),&
         &uindex, usrord, lcell(i), afs)

      elseif (ecell(i) .eq. 'AT') then
         errcod = getrel(fd_nefis_waq, grpnam  , ecell(i),&
         &uindex, usrord, lcell(i), at)

      elseif (ecell(i) .eq. 'C') then
         errcod = getrel(fd_nefis_waq, grpnam  , ecell(i),&
         &uindex, usrord, lcell(i), cp)

      elseif (ecell(i) .eq. 'DLWQTM') then
         errcod = getiel(fd_nefis_waq, grpnam  , ecell(i),&
         &uindex, usrord, lcell(i), dlwqtm)

      elseif (ecell(i) .eq. 'WT') then
         errcod = getrel(fd_nefis_waq, grpnam, ecell(i),&
         &uindex, usrord, lcell(i), wt)

      elseif (ecell(i) .eq. 'WFS') then
         errcod = getrel(fd_nefis_waq, grpnam, ecell(i),&
         &uindex, usrord, lcell(i), wfs)

      elseif (ecell(i) .eq. 'QAGGR') then
         errcod = getrel(fd_nefis_waq, grpnam, ecell(i),&
         &uindex, usrord, lcell(i), qaggr)

      elseif (ecell(i) .eq. 'QLAGGR') then
         errcod = getrel(fd_nefis_waq, grpnam, ecell(i),&
         &uindex, usrord, lcell(i), qlaggr)

      endif
!
!        Check for error
!
      if (errcod .ne. 0) then
         txt1 = 'WQRFIL @' // ecell(i) // '@ @'
         write(txt2,'(i8)') errcod
         ker = fatal
         call error ( juer, txt1 // txt2 // '@' , ewqelm, ker )
         goto 300
      endif
200 continue

300 continue

   return
end
