F90 = gfortran
.SUFFIXES: .f90 .o
OBJS =  ../mod/mod_nodes3d.o ¥
				../mod/mod_localelement3d.o ¥
				../mod/mod_elements3d.o ¥
 				../mod/mod_elemstiffmat3d.o ¥
 				../mod/mod_elemexforcevec3d.o ¥
 				../mod/mod_fem3d.o ¥
 				../mod/mod_rectmesher3d.o ¥
 				mod_appli.o ¥
 				main_appli.o
meshing: ${OBJS}
 				${F90} -o meshing *.o -L../../lib -llapack -lrefblas -ltmglib
 				rm *.mod *.o
.f90.o:
				${F90} -c $<
