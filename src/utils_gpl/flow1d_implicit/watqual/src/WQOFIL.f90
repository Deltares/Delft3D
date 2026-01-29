subroutine wqofil ( agrnam , fd_nefis_waq, maxts  ,&
&psi    ,juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQOFIL (Water Quality Open FILe)
!
! Module description: This routine opens the interface file created by
!                     the flow module.
!
!                     The interface file from the flow module to the
!                     water quality interface module will be a NEFIS
!                     file.
!                     This file is opened and an inquire will be execu-
!                     ted to determine the number of time steps stored
!                     in the file.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 agrnam            I  Path and name of aggregation file
!  3 datfds            P  -
!  2 deffds            P  -
!  6 juer              P  -
!  7 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  4 maxts             O  Number of time steps on file.
!  5 psi               O  Space weight factor in Preissmann scheme.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! getiel  GET Integer ELement from nefis file
! getrel  GET Real ELement from a nefis file
! opndat  OPeNs a DATa file
! opndef  OPeNs a DEFinition file
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
! $Log: wqofil.pf,v $
! Revision 1.4  1999/03/15  15:54:05  kuipe_j
! tabs removed
!
! Revision 1.3  1995/05/30  09:56:38  hoeks_a
! Minor changes
!
! Revision 1.2  1993/11/26  15:35:47  kuipe_j
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
   character*120 agrnam
   integer       fd_nefis_waq
   integer       maxts
   integer       juer, ker
   real          psi
!
!     Variables
!
   character*120 datnam, defnam
   integer       errcod
   integer       lnam  , lennam
   character*1   coding
   character*16  grpnam, elmnam
   character*8   txt

   integer       start, stop, incr
   parameter    (start=1, stop=2, incr=3)
   integer       usrord(5), uindex(3,5), i
   integer       ibuf(1)
   real          rbuf(1)
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     External functions
!
   integer, external     :: crenef, getrel, getiel
!
!     Open definition and data file (extensions .WDF & .WDA)
!
   lnam   = index (agrnam, ' ')
   if (lnam .eq. 0) then
      lnam = 9
   endif
!
!     Create file names
!
   defnam = agrnam (1:lnam-1) // '.wdf'
   datnam = agrnam (1:lnam-1) // '.wda'

!
!     Open NEFIS-file
!
   errcod = crenef (fd_nefis_waq, datnam, defnam, coding, 'u')
   if (errcod .ne. 0) then
      write(txt,'(i8)') errcod
      ker = fatal
      call error (juer, 'WQOFIL @crenef@ @' // txt // '@',&
      &ewqodf, ker )
   endif
!
!     Read the stored number of delwaq time steps
!
   if (errcod .eq. 0) then
!
!        Set user order
!
      do 100 i = 1, 5
         usrord(i) = i
100   continue
!
!        Set uindex
!
      uindex(start, 1) = 1
      uindex(stop , 1) = 1
      uindex(incr , 1) = 1
!
!        Set group name and element name
!
      grpnam = 'WQINT-DES-GROUP'
      elmnam = 'DLWQTS'
!
!        Read delwaq time steps
!
      errcod = getiel (fd_nefis_waq, grpnam, elmnam,&
      &uindex, usrord, 4     , ibuf   )
      maxts =  ibuf(1)
!
!        Check for error
!
      if (errcod .ne. 0) then
         write(txt,'(i8)') errcod
         ker = fatal
         call error (juer, 'WQOFIL @'//elmnam//'@ @' // txt // '@',&
         &ewqelm, ker )
      endif
!
!        Read psi
!
      elmnam = 'PSI'
!
      errcod = getrel (fd_nefis_waq, grpnam, elmnam,&
      &uindex, usrord, 4     , rbuf  )
!
      psi = rbuf(1)
!
!        Check for error
!
      if (errcod .ne. 0) then
         write(txt,'(i8)') errcod
         ker = fatal
         call error (juer, 'WQOFIL @'//elmnam//'@ @' // txt // '@',&
         &ewqelm, ker )
      endif
   else
      maxts = 0
   endif

   return
end
