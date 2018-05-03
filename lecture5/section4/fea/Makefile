F90 = gfortran
.SUFFIXES: .f90 .o
OBJS = ../mod/mod_nodes3d.o \
				../mod/mod_localelement3d.o \
				../mod/mod_elements3d.o \
				../mod/mod_elemstiffmat3d.o \
				../mod/mod_elemexforcevec3d.o \
				../mod/mod_fem3d.o \
				mod_appli.o \
				main_appli.o
fea : ${OBJS}
				${F90} -o ./bin/fea *.o -fbounds-check -fbacktrace -ffpe-trap=invalid,zero,overflow -L../../../lib -llapack -lrefblas -ltmglib
				rm *.mod *.o
.f90.o:
				${F90} -c $< -fbounds-check -fbacktrace -ffpe-trap=invalid,zero,overflow
