subroutine soini1(sbkrel, itstat, lbatch, lfrou, ldebug, lurtn,&
&juscr, filnam, ker)

   use sobeksim_version_module

   implicit none
   include '..\include\filsim.i'
   integer       juscr,  sbkrel(3),itstat(4),   ker,lurtn
   logical       lbatch, lfrou,  ldebug
   character*256 filnam


!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\mempool.i'
!
!     Define sobek release number
!
!     Sub number will be displayed in 3 digits. So
!     sbkrel(2)=2     :  x.002
!     sbkrel(2)=20    :  x.020
!     sbkrel(2)=200   :  x.200

!     sbkrel(2) = 156  Van 30-6-2000
!     sbkrel(2) = 160  Van 14-9-2000
!     sbkrel(2) = 161  Van 11-10-2000
!     sbkrel(2) = 162  Van 24-10-2000
!     sbkrel(2) = 169  Van 1-11-2000
!     sbkrel(2) = 170  Van 6-11-2000
!     sbkrel(2) = 180  Van 24-11-2000
!     sbkrel(2) = 181  Van 4-1-2001
!     sbkrel(2) = 182  Van 18-1-2001
!     sbkrel(2) = 183  Van 21-2-2001
!     sbkrel(2) = 184  Van 6-3-2001
!     sbkrel(2) = 185  Van 19-4-2001
!     sbkrel(2) = 186  Van 21-5-2001
!     sbkrel(2) = 187  Van 25-6-2001
!     sbkrel(2) = 190  Van 20-8-2001
!     sbkrel(2) = 191  Van 28-8-2001
!     sbkrel(2) = 192  Van 4-10-2001
!     sbkrel(2) = 193  Van 16-10-2001
!     sbkrel(2) = 194  Van 16-10-2001
!     sbkrel(2) = 195  Van 1-11-2001
!     sbkrel(2) = 200  Van 7-11-2001
!     sbkrel(2) = 201  Van 22-11-2001
!     sbkrel(2) = 202  Van 27-11-2001
!     sbkrel(2) = 203  Van 9-1-2002
!     sbkrel(2) = 204  Van 11-1-2002
!     sbkrel(2) = 205  Van 17-1-2002
!     sbkrel(2) = 206  Van 4-3-2002 (voor jos)
!     sbkrel(2) = 206  Van 28-3-2002
!     sbkrel(2) = 207  Van 18-4-2002
!     sbkrel(2) = 208  Van 10-6-2002

!     sbkrel(2) = 209  Van 10-6-2002
!     sbkrel(2) = 210  Van 18-6-2002
!     sbkrel(2) = 211  Van 30-7-2002
!     sbkrel(2) = 212  Van 12-8-2002
!     sbkrel(2) = 213  Van ??-??-????
!     sbkrel(2) = 214  Van 09-12-2002
!     sbkrel(2) = 215  Van 27-03-2003 (special voor Meander Advies)
!     sbkrel(2) = 216  Van 22-05-2003
!     sbkrel(2) = 217  Van 09-12-2003
!     sbkrel(2) = 218  Van 20-01-2004
!     sbkrel(2) = 219  Van 07-06-2004 (ARS 13077)
!     sbkrel(2) = 220  Van 21-06-2004 (ARS 12354)
!     sbkrel(2) = 221  Van 23-06-2004 (ARS 13180) --> v.2.52.004
!     sbkrel(2) = 222  Van 13-10-2004 (ARS 13638)
!     sbkrel(2) = 223  Van 18-02-2005 (ARS 14222)
!     sbkrel(2) = 224  Van 25-02-2005 (special voor 2.52.04 + Froudmax; ARS 14293)
!     sbkrel(2) = 225  Van 21-07-2005 (ARS 14704)
!     sbkrel(2) = 226  Van 31-08-2005 (ARS 14734)
!     sbkrel(2) = 227  Van 02-11-2005 (ARS 15044)
!     sbkrel(2) = 228  Van 30-11-2005 (ARS 07786, 13824) --> v.2.52.005
!     sbkrel(2) = 229  Van 27-04-2006 (ARS 11484 - retentiebekkens)
!     sbkrel(2) = 230  Van 05-03-2007 (OpenMI Compliant version of Comp. core)
!     sbkrel(2) = 231  Van 10-05-2007 (gereserveerd voor versie met Nefis4)
!     sbkrel(2) = 232  Van 05-03-2007 (betreft code excl. SRW en Mozart tbv Linux)
!     sbkrel(2) = 233  Van 05-12-2007 (JIRA SOBEK-19359, WiBo)
!                      N.B.: wijzigingen 231 en 232 NIET in deze versie 233
!     sbkrel(2) = 234  Van 05-11-2008 (tbv Arcadis-Meander: by-pass hydr.str.<0)
!                                      en aanpassing voor rekenen met Fr^2>0.98
!                      N.B.: wijzigingen 231, 232 en 233 NIET in versie 234
!     sbkrel(2) = 235  Van 03-03-2009 (Special voor HKV tbv rekenen met Fr^2>0.98)
!     sbkrel(2) = 236  Van 14-04-2009 (SobekRE v. 2.52.007)
!                      N.B.: wijzigingen 231, 232, 234 en 235 NIET in versie 236
!     sbkrel(2) = 237  Van 03-02-2010 (Special voor Deltares/BfG tbv rekenen met Fr^2>0.98)
!     sbkrel(2) = 238  Van 21-03-2010 (Voorl.by-pass ikv Sobek-21617: afh. interval controllers)
!     sbkrel(2) = 239  Van 25-05-2010 (Def.patch ikv Sobek-21617: afh. interval controllers)
!     sbkrel(2) = 240  Van 10-08-2010 (Sobek-21948: MATH-error, hydr.rad. < 0)
!     sbkrel(2) = 241  Van 04-10-2010 (Special voor BfG tbv Morf.ber. met double precision)
!     sbkrel(2) = 242  Van 24-12-2010 (Sobek-22204 [voorl. fix] en Sobek-22368 [def. patch])
!     sbkrel(2) = 243  Van 30-05-2011 (Sobek-22689: work-around in verband met histories output
!                                      structures at retention areas)
!     sbkrel(2) = 244  Van 14-05-2012 (integratie diverse wijzigingen, w.o. def.fix Sobek-22204
!                                       en volledige code double precision; tevens eindversie
!                                       voor Sobek-RE v.2.52.008)
!     sbkrel(2) = 245  Van 28-05-2012 (Sobek-22431: PID-controller aanpassingen; tevens eindversie
!                                       voor Sobek-RE v.2.52.009)
!     sbkrel(2) = 247  Van 26-09-2013 (Sobek-25251: corr. wegschrijven en inlezen restartgegevens
!                                       voor h en q)
!                      Van 16-10-2013 (Sobek-25251 vervolg: corr. initialisatie PID-controllers na
!                                       restart; tevens eindversie voor Sobek-RE v.2.52.009a)
!
   call get_sobeksim_sbkrel(sbkrel)
!

   itstat(4) = 0
   ker       = ok
   lfrou     = .false.
!
!    Open screen with carriag control for Visual Fortran
!
#if defined (USE_MSWINDOWS)

   open(UNIT = juscr, CARRIAGECONTROL = 'FORTRAN')
#endif
!
!     Read filename (argument)
!
   call sogarg ( filnam, ker, lbatch, ldebug )
!
!     Write release number in case no argument is given
!
   if (filnam .eq. ' ') then
      ker = fatal
      write (juscr,'(1x,a,i3,a,i3.3,a,i2.2)')&
      &'Sobeksim Release:',sbkrel(1),&
      &'.',sbkrel(2),'.',sbkrel(3)
   endif
!
   if (ker .ne. fatal) then
      call WRLOGO(0, sbkrel(1), 0, sbkrel(2), juscr, sbkrel(3))
      call mafina ( filnam )
      open ( unit = lurtn, file = rtncod )
      write (lurtn,'(a)') '1'
      close (lurtn)
   endif
end
