
FC=ifort
FFLAGS= -O0 -g  -fpe0  -traceback  -132
LIBS=-L/opt/intel/mkl/10.0.2.018/lib/em64t -lmkl_lapack -lmkl 
LDFLAGS= -pthread -parallel 

OBJ=  mate.o  linpackdummy.o \
  cross.o  rotdlmm.o  writemesh2.o writemesh.o \
  m_eulerangle.o 

all: main3e

main1: main1.o $(OBJ)
	$(FC) -o $@  main1.o  $(OBJ)  $(LIBS) $(LDFLAGS) 

main1b: main1b.o $(OBJ)
	$(FC) -o $@  main1b.o  $(OBJ)  $(LIBS) $(LDFLAGS)  

main2: main2.o $(OBJ)
	$(FC) -o $@  main2.o  $(OBJ)  $(LIBS) $(LDFLAGS) 

main3: main3.o $(OBJ)
	$(FC) -o $@  main3.o $(OBJ)  $(LIBS) $(LDFLAGS) 
main3b: main3b.o $(OBJ) 
	$(FC) -o $@  main3b.o $(OBJ)  $(LIBS) $(LDFLAGS) 
main3c: main3c.o $(OBJ) 
	$(FC) -o $@  main3c.o $(OBJ)  $(LIBS) $(LDFLAGS) 
main3d: main3d.o $(OBJ) 
	$(FC) -o $@  main3d.o $(OBJ)  $(LIBS) $(LDFLAGS) 

main3e: main3e.o $(OBJ)
	$(FC) -o $@  main3e.o $(OBJ)  $(LIBS) $(LDFLAGS)

axis1: axis1.o 
	$(FC) -o $@  axis1.o $(LIBS) $(LDFLAGS)
axis2: axis2.o 
	$(FC) -o $@  axis2.o $(LIBS) $(LDFLAGS)

clean:
	rm -f *.o 
