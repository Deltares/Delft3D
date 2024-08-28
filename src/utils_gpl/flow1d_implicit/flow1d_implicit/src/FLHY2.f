      subroutine flhy2 (fd_nefis_res ,nstman ,nsttim ,ngrid  ,
     &                  itim   ,istep  ,nstep  ,first  ,writim ,
     &                  juer   ,strtim ,nstru  ,strhis ,
     &                  nclstr ,stcpre ,buf    ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLHY2 (FLow HYdrodynamic results 2)
c
c Module description: Routine FLHY2 writes the user selected structure
c                     results to the result file. The result file is
c                     processed by the User Interface. The reason for a
c                     separate routine is that locations are now defined
c                     by structure numbers in stead of grid points as in
c                     FLHY1.
c
c                     In subroutine FLHY2 the user selected structure
c                     results at user supplied locations and time levels
c                     (System Specifications for 1D Modelling System
c                     Front End) will be stored on the result file. The
c                     stored data can be processed further by the User
c                     Interface. The user can select functions of place
c                     or functions of time. See [S-DD-003.2JK], chapter
c                     5 structure results for a specification of the
c                     Nefis names.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 17 buf(ngrid)        O  Buffer for results to be written to NEFIS.
c  2 dafdrs            P  -
c  1 defdrs            P  -
c  9 first             I  True in case of first call.
c  7 istep             I  Current time step number (t(n+1)).
c  6 itim              P  -
c 11 juer              P  -
c 18 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 15 nclstr            I  Actual cell number of a history block for the
c                         structures results at the NEFIS resultfile.
c  5 ngrid             I  Number of grid points in network.
c  8 nstep             I  Last time step number in simulation.
c  3 nstman            I  Number of main codes of structures results.
c 13 nstru             P  -
c  4 nsttim            I  Number of entries in strtim.
c 16 stcpre(nstman)    I  stcpre(i) = index in block table (1...nentri)
c                         for main code i of the structures results.
c 14 strhis(10,nstru)  I  For each structure the discharge and the
c                         parameters to be controlled must be saved to
c                         be able to write to the output file. This will
c                         be done in array strhis(8,nstru). This array
c                         will also be used to check the values of the
c                         controlled parameters or to determine if
c                         increase(open) or decrease(close) of these
c                         parameters occurs. This array will also be
c                         part of the restart file.
c                         (1,i) = Gate height
c                         (2,i) = Crest height
c                         (3,i) = Crest width
c                         (4,i) = Discharge through structure
c                         (5,i) = Gate height at previous time step
c                         (6,i) = Crest height at previous time step
c                         (7,i) = Crest width at previous time step
c                         (8,i) = Flow condition of general structure:
c                                 formno = 0, closed or other structure
c                                 formno = 1, free weir
c                                 formno = 2, drowned weir
c                                 formno = 3, free gate
c                                 formno = 4, drowned gate
c                         (9,i) = coefficient Q-H-realtion asde
c                         (10,i)= coefficient Q-H-realtion bsde
c                         (11,i)= coefficient Q-H-realtion csde
c                         (12,i)= coefficient Q-H-realtion dsde
c 12 strtim(nsttim)    I  Parameter list for HIST block with hydrodyna-
c                         mic structure results:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report parameter code 1
c                         (i+2)    = Report parameter sub code 1
c                         (nsttim) = Report parameter n sub code
c 10 writim            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c flsdat  FLuSh buffers of DATa file
c putrel  PUT Real ELement to a nefis file
c resdes  RESults; DEScription group is defined
c resini  RESults; writing is INItialized
c restim  RESults; writing of current TIMe
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flhy2.pf,v $
c Revision 1.10  1999/03/15  15:50:01  kuipe_j
c tabs removed
c
c Revision 1.9  1997/06/17  11:26:33  kuipe_j
c output in history format
c
c Revision 1.8  1996/12/02  15:41:25  kuipe_j
c dimension for histories improvred
c
c Revision 1.7  1996/09/03  14:51:58  kuipe_j
c frequency time hist,Messages controllers added
c
c Revision 1.6  1996/01/17  14:38:28  kuipe_j
c header update
c
c Revision 1.5  1995/11/21  14:16:19  kuipe_j
c proper initialization of error code
c
c Revision 1.4  1995/09/22  10:01:39  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/08/30  12:36:37  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.2  1995/05/30  09:55:04  hoeks_a
c Minor changes
c
c Revision 1.1  1995/04/13  07:07:46  hoeks_a
c Initial check-in
c
c Revision 1.3  1995/03/08  09:07:25  kuipe_j
c Call to resdes improved
c
c Revision 1.2  1993/11/26  15:30:57  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:50  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters
c
      integer  nstman  ,nsttim ,ngrid ,juer  ,istep  ,nstep ,
     &         nstru   ,nceld  ,nclstr ,ker
      integer  fd_nefis_res, strtim(nsttim), stcpre(nstman),itim(2)
      real     strhis(dmstrh,*), buf(ngrid)
      logical      first   ,writim
c
c     Declaration of local variables
c
      integer      nentri ,nstmap ,istru
      parameter   (nentri=4 ,nstmap=1)
      integer      errr  ,i ,j ,ie  ,nrerr ,nsk ,nlc ,lastcod
      integer      strmap(nstmap)  ,usrord(1),
     &             uindex(3)
      character(len=16) grnamm          ,grnamh          ,grnamd
      character(len=16) nameel(nentri),quanel(nentri),unitel(nentri),
     &             nameac(nentri+1)
      character(len=64) descel(nentri)
      character(len=9)  txt
      logical      llog, new
c
c     Include sobek error code file
c
      include '../include/errcod.i'
c
c     Declaration of (external) functions
c
      integer      putrel ,flsdat
      logical      yesmap ,newuit
      external     putrel ,flsdat ,yesmap
c
      data  usrord /1/
c
c     Definition of elements.
c
      data (descel(i) ,nameel(i) ,quanel(i) ,
     &      unitel(i),i=1,nentri) /
c
c      1 
     &    'Gate height'                     ,'HIS_Gh'   ,'Gh', 'm'    ,
c      1
     &    'Crest level'                     ,'HIS_Cl'   ,'Cl', 'm'    ,
c      1
     &    'Crest width'                     ,'HIS_Cw'   ,'Cw', 'm'    ,
c      2
     &    'Q-structure'                     ,'HIS_Qs'   ,'Qs', 'm3/s' /
c
      data  grnamm   ,grnamh ,grnamd/
     &      'UNUSED' ,
     &      'FLSTR-HIS-GROUP' ,
     &      'FLSTR-DES-GROUP' /
c
      strmap(1) = 0
      lastcod   = nentri
c
c     Initialize.
c
      if (first) then
         nrerr = eflboo
c
         call resini (fd_nefis_res ,grnamd ,grnamm ,grnamh ,'FLSTR',
     &                nentri ,nstmap ,nsttim ,nstru  ,itim   ,nameel ,
     &                quanel ,unitel ,descel ,stcpre ,strmap ,strtim ,
     &                nceld  ,nclstr ,nameac ,lastcod,errr   )
c
         if (errr.ne.0) goto 1000
      endif
c
c     Write History results.
c
      nlc = strtim(1)
      new = mod(nsttim-nlc,2) .eq. 0

      if ( new ) then
         nsk = 3
      else
         nsk = 0
      endif
c
      if (nsttim .gt. 1+nsk) then
         if (new) then
            newuit = yesmap(strtim(nlc+2),strtim(nlc+3),strtim(nlc+4),
     &               istep)
         else
            newuit = .false.
         endif
         if ( (strtim(1).gt.0 .and. .not. new) .or. newuit ) then
            nrerr = eflhis
c
            call restim (fd_nefis_res ,grnamh ,1  ,itim  ,nclstr ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('FLSTR', fd_nefis_res ,grnamd ,2 ,writim ,
     &                   strtim ,nceld  ,nclstr ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = nclstr
            uindex(2) = nclstr
            uindex(3) = 1
c
            do 100 i = strtim(1)+2+nsk,nsttim,2
               ie = stcpre(strtim(i)) + strtim(i+1)
               do 25 j=1,strtim(1)
                  istru  = strtim(j+1)
                  buf(j) = strhis(ie,istru)
   25          continue
               errr = putrel (fd_nefis_res, grnamh ,nameel(ie) ,
     &                        uindex  ,usrord ,buf    )
               if (errr.ne.0) goto 1000
  100       continue
         endif
      endif
c
c     Be sure that at the last step the number of cells has been
c     written correctly.
c
      if (istep.ge.nstep) then
         nrerr = efleoo
         llog  = .true.
c
         call resdes('FLSTR', fd_nefis_res, grnamd ,2 ,llog ,
     &                 strtim ,nceld  ,nclstr ,errr )
         if (errr.ne.0) goto 1000
c
         errr = flsdat(fd_nefis_res)
         if (errr.ne.0) goto 1000
      endif
c
      return
c
 1000 continue
c
      ker = fatal
      write (txt,'(i8)') errr
      call sre_error (juer ,'FLHY2 @'//txt//'@' ,nrerr ,ker)
c
      end
