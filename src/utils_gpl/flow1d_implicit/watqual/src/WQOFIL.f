      subroutine wqofil ( agrnam , fd_nefis_waq, maxts  ,
     +                    psi    ,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQOFIL (Water Quality Open FILe)
c
c Module description: This routine opens the interface file created by
c                     the flow module.
c
c                     The interface file from the flow module to the
c                     water quality interface module will be a NEFIS
c                     file.
c                     This file is opened and an inquire will be execu-
c                     ted to determine the number of time steps stored
c                     in the file.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 agrnam            I  Path and name of aggregation file
c  3 datfds            P  -
c  2 deffds            P  -
c  6 juer              P  -
c  7 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  4 maxts             O  Number of time steps on file.
c  5 psi               O  Space weight factor in Preissmann scheme.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c getiel  GET Integer ELement from nefis file
c getrel  GET Real ELement from a nefis file
c opndat  OPeNs a DATa file
c opndef  OPeNs a DEFinition file
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
c $Log: wqofil.pf,v $
c Revision 1.4  1999/03/15  15:54:05  kuipe_j
c tabs removed
c
c Revision 1.3  1995/05/30  09:56:38  hoeks_a
c Minor changes
c
c Revision 1.2  1993/11/26  15:35:47  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:28  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      character*120 agrnam
      integer       fd_nefis_waq
      integer       maxts
      integer       juer, ker
      real          psi
c
c     Variables
c
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
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     External functions
c
      integer, external     :: crenef, getrel, getiel
c
c     Open definition and data file (extensions .WDF & .WDA)
c
      lnam   = index (agrnam, ' ')
      if (lnam .eq. 0) then
         lnam = 9
      endif
c
c     Create file names
c
      defnam = agrnam (1:lnam-1) // '.wdf'
      datnam = agrnam (1:lnam-1) // '.wda'

c
c     Open NEFIS-file
c
      errcod = crenef (fd_nefis_waq, datnam, defnam, coding, 'u')
      if (errcod .ne. 0) then
         write(txt,'(i8)') errcod
         ker = fatal
         call error (juer, 'WQOFIL @crenef@ @' // txt // '@',
     +               ewqodf, ker )
      endif
c
c     Read the stored number of delwaq time steps
c
      if (errcod .eq. 0) then
c
c        Set user order
c
         do 100 i = 1, 5
            usrord(i) = i
 100     continue
c
c        Set uindex
c
         uindex(start, 1) = 1
         uindex(stop , 1) = 1
         uindex(incr , 1) = 1
c
c        Set group name and element name
c
         grpnam = 'WQINT-DES-GROUP'
         elmnam = 'DLWQTS'
c
c        Read delwaq time steps
c
         errcod = getiel (fd_nefis_waq, grpnam, elmnam,
     +                     uindex, usrord, 4     , ibuf   )
         maxts =  ibuf(1)
c
c        Check for error
c
         if (errcod .ne. 0) then
            write(txt,'(i8)') errcod
            ker = fatal
            call error (juer, 'WQOFIL @'//elmnam//'@ @' // txt // '@',
     +                  ewqelm, ker )
         endif
c
c        Read psi
c
         elmnam = 'PSI'
c
         errcod = getrel (fd_nefis_waq, grpnam, elmnam,
     +                     uindex, usrord, 4     , rbuf  )
c
         psi = rbuf(1)
c
c        Check for error
c
         if (errcod .ne. 0) then
            write(txt,'(i8)') errcod
            ker = fatal
            call error (juer, 'WQOFIL @'//elmnam//'@ @' // txt // '@',
     +                  ewqelm, ker )
         endif
      else
         maxts = 0
      endif

      return
      end
