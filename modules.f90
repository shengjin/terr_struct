!###############################################################
!###############################################################
!###############################################################

MODULE PARAMETERS
INTEGER,PARAMETER         ::nvar=3                     !number of variables for structure
INTEGER,PARAMETER         ::nfmax=10000                !max number of layers in structure
INTEGER,PARAMETER         ::KMAXX=10000                !max number of steps in integration 
INTEGER,PARAMETER         ::NMAX=3                     !max number of variables for structure
END MODULE PARAMETERS

!###############################################################
!###############################################################
!###############################################################

MODULE FRACT
DOUBLE PRECISION          ::frac_core
DOUBLE PRECISION          ::frac_mantle
DOUBLE PRECISION          ::frac_water
DOUBLE PRECISION          ::Mtot
END MODULE FRACT

!###############################################################
!###############################################################
!###############################################################

MODULE PARAEOS
DOUBLE PRECISION          ::k0_water
DOUBLE PRECISION          ::k0d_water
DOUBLE PRECISION          ::rho0_water
INTEGER                   ::iEOS_water
DOUBLE PRECISION          ::k0_mantle
DOUBLE PRECISION          ::k0d_mantle
DOUBLE PRECISION          ::rho0_mantle
INTEGER                   ::iEOS_mantle
DOUBLE PRECISION          ::k0_core
DOUBLE PRECISION          ::k0d_core
DOUBLE PRECISION          ::rho0_core
INTEGER                   ::iEOS_core
END MODULE PARAEOS

!###############################################################
!###############################################################
!###############################################################
