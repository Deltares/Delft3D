subroutine flhy2 (fd_nefis_res ,nstman ,nsttim ,ngrid  ,&
&itim   ,istep  ,nstep  ,first  ,writim ,&
&juer   ,strtim ,nstru  ,strhis ,&
&nclstr ,stcpre ,buf    ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLHY2 (FLow HYdrodynamic results 2)
!
! Module description: Routine FLHY2 writes the user selected structure
!                     results to the result file. The result file is
!                     processed by the User Interface. The reason for a
!                     separate routine is that locations are now defined
!                     by structure numbers in stead of grid points as in
!                     FLHY1.
!
!                     In subroutine FLHY2 the user selected structure
!                     results at user supplied locations and time levels
!                     (System Specifications for 1D Modelling System
!                     Front End) will be stored on the result file. The
!                     stored data can be processed further by the User
!                     Interface. The user can select functions of place
!                     or functions of time. See [S-DD-003.2JK], chapter
!                     5 structure results for a specification of the
!                     Nefis names.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 17 buf(ngrid)        O  Buffer for results to be written to NEFIS.
!  2 dafdrs            P  -
!  1 defdrs            P  -
!  9 first             I  True in case of first call.
!  7 istep             I  Current time step number (t(n+1)).
!  6 itim              P  -
! 11 juer              P  -
! 18 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 15 nclstr            I  Actual cell number of a history block for the
!                         structures results at the NEFIS resultfile.
!  5 ngrid             I  Number of grid points in network.
!  8 nstep             I  Last time step number in simulation.
!  3 nstman            I  Number of main codes of structures results.
! 13 nstru             P  -
!  4 nsttim            I  Number of entries in strtim.
! 16 stcpre(nstman)    I  stcpre(i) = index in block table (1...nentri)
!                         for main code i of the structures results.
! 14 strhis(10,nstru)  I  For each structure the discharge and the
!                         parameters to be controlled must be saved to
!                         be able to write to the output file. This will
!                         be done in array strhis(8,nstru). This array
!                         will also be used to check the values of the
!                         controlled parameters or to determine if
!                         increase(open) or decrease(close) of these
!                         parameters occurs. This array will also be
!                         part of the restart file.
!                         (1,i) = Gate height
!                         (2,i) = Crest height
!                         (3,i) = Crest width
!                         (4,i) = Discharge through structure
!                         (5,i) = Gate height at previous time step
!                         (6,i) = Crest height at previous time step
!                         (7,i) = Crest width at previous time step
!                         (8,i) = Flow condition of general structure:
!                                 formno = 0, closed or other structure
!                                 formno = 1, free weir
!                                 formno = 2, drowned weir
!                                 formno = 3, free gate
!                                 formno = 4, drowned gate
!                         (9,i) = coefficient Q-H-realtion asde
!                         (10,i)= coefficient Q-H-realtion bsde
!                         (11,i)= coefficient Q-H-realtion csde
!                         (12,i)= coefficient Q-H-realtion dsde
! 12 strtim(nsttim)    I  Parameter list for HIST block with hydrodyna-
!                         mic structure results:
!                         (1)      = Number of places
!                         (2)      = Report grid point 1
!                         (3)      = Report grid point 2
!                         (i)      = Report grid point n
!                         (i+1)    = Report parameter code 1
!                         (i+2)    = Report parameter sub code 1
!                         (nsttim) = Report parameter n sub code
! 10 writim            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! flsdat  FLuSh buffers of DATa file
! putrel  PUT Real ELement to a nefis file
! resdes  RESults; DEScription group is defined
! resini  RESults; writing is INItialized
! restim  RESults; writing of current TIMe
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flhy2.pf,v $
! Revision 1.10  1999/03/15  15:50:01  kuipe_j
! tabs removed
!
! Revision 1.9  1997/06/17  11:26:33  kuipe_j
! output in history format
!
! Revision 1.8  1996/12/02  15:41:25  kuipe_j
! dimension for histories improvred
!
! Revision 1.7  1996/09/03  14:51:58  kuipe_j
! frequency time hist,Messages controllers added
!
! Revision 1.6  1996/01/17  14:38:28  kuipe_j
! header update
!
! Revision 1.5  1995/11/21  14:16:19  kuipe_j
! proper initialization of error code
!
! Revision 1.4  1995/09/22  10:01:39  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/08/30  12:36:37  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.2  1995/05/30  09:55:04  hoeks_a
! Minor changes
!
! Revision 1.1  1995/04/13  07:07:46  hoeks_a
! Initial check-in
!
! Revision 1.3  1995/03/08  09:07:25  kuipe_j
! Call to resdes improved
!
! Revision 1.2  1993/11/26  15:30:57  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:50  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Declaration of parameters
!
   integer  nstman  ,nsttim ,ngrid ,juer  ,istep  ,nstep ,&
   &nstru   ,nceld  ,nclstr ,ker
   integer  fd_nefis_res, strtim(nsttim), stcpre(nstman),itim(2)
   real     strhis(dmstrh,*), buf(ngrid)
   logical      first   ,writim
!
!     Declaration of local variables
!
   integer      nentri ,nstmap ,istru
   parameter   (nentri=4 ,nstmap=1)
   integer      errr  ,i ,j ,ie  ,nrerr ,nsk ,nlc ,lastcod
   integer      strmap(nstmap)  ,usrord(1),&
   &uindex(3)
   character(len=16) grnamm          ,grnamh          ,grnamd
   character(len=16) nameel(nentri),quanel(nentri),unitel(nentri),&
   &nameac(nentri+1)
   character(len=64) descel(nentri)
   character(len=9)  txt
   logical      llog, new
!
!     Include sobek error code file
!
   include '../include/errcod.i'
!
!     Declaration of (external) functions
!
   integer      putrel ,flsdat
   logical      yesmap ,newuit
   external     putrel ,flsdat ,yesmap
!
   data  usrord /1/
!
!     Definition of elements.
!
   data (descel(i) ,nameel(i) ,quanel(i) ,&
   &unitel(i),i=1,nentri) /&
!
!      1
   &'Gate height'                     ,'HIS_Gh'   ,'Gh', 'm'    ,&
!      1
   &'Crest level'                     ,'HIS_Cl'   ,'Cl', 'm'    ,&
!      1
   &'Crest width'                     ,'HIS_Cw'   ,'Cw', 'm'    ,&
!      2
   &'Q-structure'                     ,'HIS_Qs'   ,'Qs', 'm3/s' /
!
   data  grnamm   ,grnamh ,grnamd/&
   &'UNUSED' ,&
   &'FLSTR-HIS-GROUP' ,&
   &'FLSTR-DES-GROUP' /
!
   strmap(1) = 0
   lastcod   = nentri
!
!     Initialize.
!
   if (first) then
      nrerr = eflboo
!
      call resini (fd_nefis_res ,grnamd ,grnamm ,grnamh ,'FLSTR',&
      &nentri ,nstmap ,nsttim ,nstru  ,itim   ,nameel ,&
      &quanel ,unitel ,descel ,stcpre ,strmap ,strtim ,&
      &nceld  ,nclstr ,nameac ,lastcod,errr   )
!
      if (errr.ne.0) goto 1000
   endif
!
!     Write History results.
!
   nlc = strtim(1)
   new = mod(nsttim-nlc,2) .eq. 0

   if ( new ) then
      nsk = 3
   else
      nsk = 0
   endif
!
   if (nsttim .gt. 1+nsk) then
      if (new) then
         newuit = yesmap(strtim(nlc+2),strtim(nlc+3),strtim(nlc+4),&
         &istep)
      else
         newuit = .false.
      endif
      if ( (strtim(1).gt.0 .and. .not. new) .or. newuit ) then
         nrerr = eflhis
!
         call restim (fd_nefis_res ,grnamh ,1  ,itim  ,nclstr ,&
         &nameac ,errr   )
         if (errr.ne.0) goto 1000
!
         call resdes ('FLSTR', fd_nefis_res ,grnamd ,2 ,writim ,&
         &strtim ,nceld  ,nclstr ,errr   )
         if (errr.ne.0) goto 1000
!
         uindex(1) = nclstr
         uindex(2) = nclstr
         uindex(3) = 1
!
         do 100 i = strtim(1)+2+nsk,nsttim,2
            ie = stcpre(strtim(i)) + strtim(i+1)
            do 25 j=1,strtim(1)
               istru  = strtim(j+1)
               buf(j) = strhis(ie,istru)
25          continue
            errr = putrel (fd_nefis_res, grnamh ,nameel(ie) ,&
            &uindex  ,usrord ,buf    )
            if (errr.ne.0) goto 1000
100      continue
      endif
   endif
!
!     Be sure that at the last step the number of cells has been
!     written correctly.
!
   if (istep.ge.nstep) then
      nrerr = efleoo
      llog  = .true.
!
      call resdes('FLSTR', fd_nefis_res, grnamd ,2 ,llog ,&
      &strtim ,nceld  ,nclstr ,errr )
      if (errr.ne.0) goto 1000
!
      errr = flsdat(fd_nefis_res)
      if (errr.ne.0) goto 1000
   endif
!
   return
!
1000 continue
!
   ker = fatal
   write (txt,'(i8)') errr
   call sre_error (juer ,'FLHY2 @'//txt//'@' ,nrerr ,ker)
!
end
