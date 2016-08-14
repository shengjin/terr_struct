SUBROUTINE initialize(X,N_frontieres,Rstart)
!set initial values for variables

USE PARAMETERS,ONLY  :nfmax,nvar
USE FRACT,ONLY       :Mtot

IMPLICIT NONE
INCLUDE 'units.h90'

DOUBLE PRECISION,DIMENSION(nvar,nfmax),INTENT(OUT)     ::X
INTEGER,INTENT(OUT)                                    ::N_frontieres
DOUBLE PRECISION,INTENT(OUT)                           ::Rstart

DOUBLE PRECISION                                       ::densite_mean

LOGICAL,PARAMETER             ::DEBUG=.TRUE.
 
!initialze to zero
X=0D0
N_frontieres=0

!estimated initial radius. 
densite_mean=6.5
Rstart=(Mtot/(4D0/3D0*PI*densite_mean))**(1D0/3D0)

IF(DEBUG)THEN
   WRITE(*,*)
   WRITE(*,*)'SBR INITALIZE'
   WRITE(*,*)'N_frontieres    ',N_frontieres
   WRITE(*,*)'X(1,1)          ',X(1,1)
   WRITE(*,*)'Mtot/Mearth     ',Mtot/Mearth
   WRITE(*,*)'densite_mean    ',densite_mean
   WRITE(*,*)'Rstart/Rearth   ',Rstart/Rearth
   WRITE(*,*)
   WRITE(*,*)
END IF

RETURN 
END SUBROUTINE initialize
