!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

   ! ********************************************************************
      SUBROUTINE ENTRYFLOW (Y0,J,U,DUDY,TKE,EPS,NUT,GAMT)
! ********************************************************************
!     STRATIFIED MIXING LAYER EXPERIMENTS BY DELFT HYDRAULICS
!                           1987
!
!     INLET CONDITIONS FOR HORIZONTAL VELOCITY, TURBULENT KINETIC
!     ENERGY AND ENERGY DISSIPATION.
!
!     R.E. UITTENBOGAARD.
!     NOV. 1987
!
      double precision :: Y0(*),U,DUDY,TKE,EPS,NUT,GAMT
      double precision :: AU(50,5),CMU,DY,KAP,ST,Y , uwall, uplate
      INTEGER          :: J, NPOINTS , nupnts, ip
!
      DATA AU( 1,1)/   .00/,AU( 1,2)/   .00/,AU( 1,3)/ .1605E+03/, &
           AU( 1,4)/ .0000E+00/,AU( 1,5)/-.2652E+02/
      DATA AU( 2,1)/  1.00/,AU( 2,2)/134.00/,AU( 2,3)/ .8097E+02/, &
           AU( 2,4)/-.7955E+02/,AU( 2,5)/ .2958E+02/
      DATA AU( 3,1)/  2.00/,AU( 3,2)/165.00/,AU( 3,3)/ .1062E+02/, &
            AU( 3,4)/ .9203E+01/,AU( 3,5)/-.4821E+01/
      DATA AU( 4,1)/  3.00/,AU( 4,2)/180.00/,AU( 4,3)/ .1456E+02/, &
           AU( 4,4)/-.5261E+01/,AU( 4,5)/ .1450E+01/
      DATA AU( 5,1)/  4.00/,AU( 5,2)/190.75/,AU( 5,3)/ .8390E+01/, &
           AU( 5,4)/-.9100E+00/,AU( 5,5)/ .5523E-01/
      DATA AU( 6,1)/  7.50/,AU( 6,2)/211.34/,AU( 6,3)/ .4050E+01/, &
           AU( 6,4)/-.3300E+00/,AU( 6,5)/ .1380E-01/
      DATA AU( 7,1)/ 17.00/,AU( 7,2)/231.85/,AU( 7,3)/ .1515E+01/, &
           AU( 7,4)/ .6319E-01/,AU( 7,5)/-.4415E-02/
      DATA AU( 8,1)/ 25.00/,AU( 8,2)/245.75/,AU( 8,3)/ .1678E+01/, &
           AU( 8,4)/-.4277E-01/,AU( 8,5)/ .1367E-02/
      DATA AU( 9,1)/ 35.00/,AU( 9,2)/259.62/,AU( 9,3)/ .1233E+01/, &
           AU( 9,4)/-.1743E-02/,AU( 9,5)/ .5904E-05/
      DATA AU(10,1)/ 50.00/,AU(10,2)/277.74/,AU(10,3)/ .1185E+01/, &
           AU(10,4)/-.1478E-02/,AU(10,5)/-.9626E-04/
      DATA AU(11,1)/ 75.00/,AU(11,2)/304.92/,AU(11,3)/ .9301E+00/, &
           AU(11,4)/-.8697E-02/,AU(11,5)/-.1250E-03/
      DATA AU(12,1)/100.00/,AU(12,2)/320.79/,AU(12,3)/ .2610E+00/, &
           AU(12,4)/-.1807E-01/,AU(12,5)/ .2623E-03/
      DATA AU(13,1)/125.00/,AU(13,2)/320.12/,AU(13,3)/-.1506E+00/, &
           AU(13,4)/ .1606E-02/,AU(13,5)/-.7927E-04/
      DATA AU(14,1)/150.00/,AU(14,2)/316.12/,AU(14,3)/-.2189E+00/, &
           AU(14,4)/-.4339E-02/,AU(14,5)/ .2096E-03/
      DATA AU(15,1)/175.00/,AU(15,2)/311.21/,AU(15,3)/-.4285E-01/, &
           AU(15,4)/ .1138E-01/,AU(15,5)/-.1237E-03/
      DATA AU(16,1)/200.00/,AU(16,2)/315.32/,AU(16,3)/ .2943E+00/, &
           AU(16,4)/ .2104E-02/,AU(16,5)/ .4974E-04/
      DATA AU(17,1)/225.00/,AU(17,2)/324.77/,AU(17,3)/ .4928E+00/, &
           AU(17,4)/ .5834E-02/,AU(17,5)/-.1028E-03/
      DATA AU(18,1)/250.00/,AU(18,2)/339.13/,AU(18,3)/ .5918E+00/, &
           AU(18,4)/-.1873E-02/,AU(18,5)/-.8348E-04/
      DATA AU(19,1)/275.00/,AU(19,2)/351.45/,AU(19,3)/ .3416E+00/, &
           AU(19,4)/-.8134E-02/,AU(19,5)/ .3620E-03/
      DATA AU(20,1)/280.00/,AU(20,2)/353.00/,AU(20,3)/ .2874E+00/, &
           AU(20,4)/-.2704E-02/,AU(20,5)/-.2956E-02/
      DATA AU(21,1)/285.00/,AU(21,2)/354.00/,AU(21,3)/ .3867E-01/, &
           AU(21,4)/-.4705E-01/,AU(21,5)/-.1374E-03/
      DATA AU(22,1)/290.00/,AU(22,2)/353.00/,AU(22,3)/-.4421E+00/, &
           AU(22,4)/-.4911E-01/,AU(22,5)/-.4494E-02/
      DATA AU(23,1)/295.00/,AU(23,2)/349.00/,AU(23,3)/-.1270E+01/, &
           AU(23,4)/-.1165E+00/,AU(23,5)/ .2114E-02/
      DATA AU(24,1)/300.00/,AU(24,2)/340.00/,AU(24,3)/-.2277E+01/, &
           AU(24,4)/-.8480E-01/,AU(24,5)/-.3964E-02/
      DATA AU(25,1)/305.00/,AU(25,2)/326.00/,AU(25,3)/-.3422E+01/, &
           AU(25,4)/-.1443E+00/,AU(25,5)/-.2260E-02/
      DATA AU(26,1)/310.00/,AU(26,2)/305.00/,AU(26,3)/-.5034E+01/, &
           AU(26,4)/-.1782E+00/,AU(26,5)/-.8325E-01/
      DATA AU(27,1)/312.50/,AU(27,2)/290.00/,AU(27,3)/-.7486E+01/, &
           AU(27,4)/-.8026E+00/,AU(27,5)/ .2388E+00/
      DATA AU(28,1)/315.00/,AU(28,2)/270.00/,AU(28,3)/-.7022E+01/, &
           AU(28,4)/ .9884E+00/,AU(28,5)/-.5519E+00/
      DATA AU(29,1)/317.50/,AU(29,2)/250.00/,AU(29,3)/-.1243E+02/, &
           AU(29,4)/-.3151E+01/,AU(29,5)/-.1265E+01/
      DATA AU(30,1)/319.00/,AU(30,2)/220.00/,AU(30,3)/-.3042E+02/, &
           AU(30,4)/-.8843E+01/,AU(30,5)/-.7395E+00/
      DATA AU(31,1)/320.00/,AU(31,2)/180.00/,AU(31,3)/-.5032E+02/, &
           AU(31,4)/-.1106E+02/,AU(31,5)/ .3073E+01/
      DATA AU(32,1)/321.50/,AU(32,2)/ 90.00/,AU(32,3)/-.6277E+02/, &
           AU(32,4)/ .2765E+01/,AU(32,5)/-.6145E+00/
      DATA AU(33,1)/323.00/,AU(33,2)/ 90.00/,AU(33,3)/-.6277E+02/, &
           AU(33,4)/ .2765E+01/,AU(33,5)/-.6145E+00/
      DATA AU(34,1)/323.00/,AU(34,2)/   .00/,AU(34,3)/ .4136E+02/, &
           AU(34,4)/ .0000E+00/,AU(34,5)/ .8638E+01/
      DATA AU(35,1)/324.00/,AU(35,2)/ 50.00/,AU(35,3)/ .6728E+02/, &
           AU(35,4)/ .2591E+02/,AU(35,5)/-.1319E+02/
      DATA AU(36,1)/325.00/,AU(36,2)/130.00/,AU(36,3)/ .7954E+02/, &
           AU(36,4)/-.1365E+02/,AU(36,5)/ .4116E+01/
      DATA AU(37,1)/326.00/,AU(37,2)/200.00/,AU(37,3)/ .6458E+02/, &
           AU(37,4)/-.1304E+01/,AU(37,5)/-.1166E+01/
      DATA AU(38,1)/327.50/,AU(38,2)/290.00/,AU(38,3)/ .5280E+02/, &
          AU(38,4)/-.6552E+01/,AU(38,5)/ .1895E+00/
      DATA AU(39,1)/330.00/,AU(39,2)/384.00/,AU(39,3)/ .2359E+02/, &
           AU(39,4)/-.5131E+01/,AU(39,5)/ .4026E+00/
      DATA AU(40,1)/335.00/,AU(40,2)/424.00/,AU(40,3)/ .2476E+01/, &
           AU(40,4)/ .9080E+00/,AU(40,5)/-.8861E-01/
      DATA AU(41,1)/340.00/,AU(41,2)/448.00/,AU(41,3)/ .4909E+01/, &
           AU(41,4)/-.4212E+00/,AU(41,5)/ .2636E-01/
      DATA AU(42,1)/345.00/,AU(42,2)/465.31/,AU(42,3)/ .2674E+01/, &
           AU(42,4)/-.2581E-01/,AU(42,5)/-.1885E-02/
      DATA AU(43,1)/360.00/,AU(43,2)/493.25/,AU(43,3)/ .6273E+00/, &
           AU(43,4)/-.1106E+00/,AU(43,5)/ .3572E-02/
      DATA AU(44,1)/375.00/,AU(44,2)/489.82/,AU(44,3)/-.2810E+00/, &
           AU(44,4)/ .5009E-01/,AU(44,5)/-.8666E-03/
      DATA AU(45,1)/400.00/,AU(45,2)/500.56/,AU(45,3)/ .5986E+00/, &
           AU(45,4)/-.1490E-01/,AU(45,5)/ .2445E-03/
      DATA AU(46,1)/425.00/,AU(46,2)/510.03/,AU(46,3)/ .3119E+00/, &
           AU(46,4)/ .3436E-02/,AU(46,5)/ .1521E-03/
      DATA AU(47,1)/450.00/,AU(47,2)/522.35/,AU(47,3)/ .7688E+00/, &
           AU(47,4)/ .1484E-01/,AU(47,5)/-.8228E-04/
      DATA AU(48,1)/475.00/,AU(48,2)/549.56/,AU(48,3)/ .1357E+01/, &
           AU(48,4)/ .8670E-02/,AU(48,5)/-.3145E-03/
      DATA AU(49,1)/500.00/,AU(49,2)/583.98/,AU(49,3)/ .1200E+01/, &
           AU(49,4)/-.1491E-01/,AU(49,5)/ .7649E-04/
      DATA AU(50,1)/565.00/,AU(50,2)/583.98/,AU(50,3)/ .1200E+01/, &
           AU(50,4)/-.1491E-01/,AU(50,5)/ .7649E-04/
!
!     SQUARE ROOT OF WALL SHEAR STRESS: UWALL IN MM/S
!       ,,    ,,     SHEAR STRESS ON SPLITTERPLATE: UPLATE IN MM/S
!
      DATA UWALL/11.44d0/,UPLATE/9.11d0/
      DATA KAP/0.41d0/,CMU/0.09d0/,ST/1.0d0/
!
      NUPNTS = 50
!
      Y = 1000.0d0 *Y0(J)
!
      IF (Y.GT.AU(NUPNTS,1)) THEN
        WRITE(*,*) '   STOP **** ERROR DETECTED IN ROUTINE ENTRY:'
        WRITE(*,*) '   AN ATTEMPT WAS MADE TO EXTRAPOLATE IN HEIGHT'
        WRITE(*,*) '   AT Y=',Y
        STOP
      ENDIF
      IP = 2
   10 IF (Y.GT.AU(IP,1)) THEN
         IP = IP+1
         GOTO 10
      ENDIF
      IP = IP-1
!
      DY = Y-AU(IP,1)
      U  = AU(IP,2)+DY*(AU(IP,3)+DY*(AU(IP,4)+DY*AU(IP,5)))
!
!     OPTION FOR DERIVATIVE DUDY = DU/DY:
!
      DUDY =  AU(IP,3)+DY*(2d0*AU(IP,4)+3d0*DY*AU(IP,5))
!
!     INLET CONDITION FOR TURBULENT KINETIC ENERGY (TKE):
!
      IF (Y.LT.110.) THEN
         TKE = 500d0-4d0*Y
      ELSEIF (Y.LT.217.) THEN
         TKE = 60d0
      ELSEIF (Y.LT.323.) THEN
         TKE = -383d0+2.037d0*Y
      ELSEIF (Y.LT.415.4) THEN
         TKE = 974.4d0-2.165d0*Y
      ELSE
         TKE = 76.
      ENDIF
!
!     INLET CONDITION FOR EPSILON (EPS):
!
      IF (Y.LT.1.) THEN
         EPS = 5.*UWALL*UWALL*UWALL/(KAP*1000.*Y0(2))
      ELSEIF (Y.LT.322.) THEN
         EPS = (UWALL*UWALL*UWALL/Y+UPLATE*UPLATE*UPLATE/(323.-Y))/KAP
      ELSEIF (Y.LT.324.) THEN
         DY  = 500.*(Y0(J+1)-Y0(J-1))
         EPS = 5.*UPLATE*UPLATE*UPLATE/(KAP*DY)
      ELSE
         EPS = UPLATE*UPLATE*UPLATE/(KAP*(Y-323.))
      ENDIF

!      CONVERSION FROM (MM) TO (M):

      U    = U  *1.d-3
      TKE  = TKE*1.d-6
      EPS  = EPS*1.d-6

      NUT  = CMU*TKE*TKE/EPS
      GAMT = NUT/ST
      RETURN
   END
