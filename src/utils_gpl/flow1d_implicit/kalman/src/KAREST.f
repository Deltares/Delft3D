      subroutine KAREST (fd_nefis_rst, grnamf ,nentri ,np     ,nnf   ,
     &                   nnmu   ,ncelst ,nameel ,p1     ,pfa    ,pmua  ,
     &                   pw     ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAREST (KAlman REad reSTart information)
c
c Module description: Read the Kalman restart information from the restart
c                     file.
c
c                     When the user restarts a simulation run from a
c                     specific point of time this routine will read the
c                     saved restart information from the previous run.
c                     The restart information contains the actual
c                     correction parameters, covariances and noises at
c                     the time level the information was saved.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 dafdst            P  -
c  1 defdst            P  -
c  3 grnamf            P  -
c 14 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  9 nameel            P  -
c  8 ncelst            I  Actual cell number of a restart block of the
c                         restart file.
c  4 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c  6 nnf               I  Number of uncertain bed friction parameters.
c  7 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c  5 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c 10 p1                P  -
c 11 pfa               P  -
c 12 pmua              P  -
c 13 pw                I  Uncertain wind stress parameter.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c getrel  GET Real ELement from a nefis file
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: karest.pf,v $
c Revision 1.3  1999/03/15  15:52:09  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:05:19  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:51  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer       nentri ,np ,nnf ,nnmu ,ncelst ,ker
      integer       fd_nefis_rst
      real          p1(np,np)      ,pfa(nnf)       ,pmua(nnmu) ,pw
      character*(*) grnamf
      character*(*) nameel(nentri)
c
c     Declaration of local variables
c
      integer       error     ,buflen
      integer       uindex(3) ,usrord(1)
      real          pwa(1)
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
      buflen = np*np*4
      error = getrel (fd_nefis_rst, grnamf ,nameel(2) ,
     &                uindex  ,usrord ,buflen ,p1     )
      if (error.ne.0) goto 1000
c
      buflen = nnf*4
      error = getrel (fd_nefis_rst, grnamf ,nameel(3) ,
     &                uindex  ,usrord ,buflen ,pfa    )
      if (error.ne.0) goto 1000
c
      if (nnmu .gt. 0) then
c
         buflen = nnmu*4
         error = getrel (fd_nefis_rst, grnamf ,nameel(4) ,
     &                   uindex  ,usrord ,buflen ,pmua    )
         if (error.ne.0) goto 1000
      endif
c
      buflen = 4
      pwa(1) = pw
      error = getrel (fd_nefis_rst, grnamf ,nameel(5) ,
     &                uindex  ,usrord ,buflen ,pwa     )
      if (error.ne.0) goto 1000
c
      goto 1010
c
 1000 continue
      ker = error
 1010 continue
      end
