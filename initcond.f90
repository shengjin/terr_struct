SUBROUTINE initcond(Pout)
!read in initial conditions

USE FRACT
USE PARAEOS

IMPLICIT NONE
INCLUDE 'units.h90'

DOUBLE PRECISION, INTENT(OUT) ::Pout
DOUBLE PRECISION              ::frac_tot
LOGICAL,PARAMETER             ::DEBUG=.TRUE.

!read in initial conditions
OPEN(unit=3,file='terrstruc.in')
READ(3,*)Pout
READ(3,*)Mtot
READ(3,*)frac_core
READ(3,*)frac_mantle
READ(3,*)frac_water


OPEN(unit=4,file='paraeos.in')
READ(4,*)k0_water
READ(4,*)k0d_water
READ(4,*)rho0_water
READ(4,*)iEOS_water
READ(4,*)k0_mantle
READ(4,*)k0d_mantle
READ(4,*)rho0_mantle
READ(4,*)iEOS_mantle
READ(4,*)k0_core
READ(4,*)k0d_core
READ(4,*)rho0_core
READ(4,*)iEOS_core


!convert to cgs
Pout=Pout*Bar ! 1 bar = 100,000 Pa
Mtot=Mtot*mearth


frac_tot=frac_core+frac_mantle+frac_water
IF( frac_tot < 0.999999 .OR. frac_tot > 1.000001 )THEN  
    WRITE(*,*) 'frac_tot =', frac_tot
    STOP
END IF



IF(DEBUG)THEN
   WRITE(*,*)
   WRITE(*,*)'SBR INITCOND: Initial conditions'
   WRITE(*,*)'Pout/Bar     ',Pout/Bar
   WRITE(*,*)'Mtot/Mearth  ',Mtot/Mearth
   WRITE(*,*)'frac_core    ',frac_core
   WRITE(*,*)'frac_mantle  ',frac_mantle
   WRITE(*,*)'frac_water   ',frac_water
   WRITE(*,*)
END IF


RETURN

END SUBROUTINE initcond
