      program test
      implicit none
      integer,parameter:: lmax=2
      integer,parameter:: nl=lmax+1
      integer,parameter:: lmx=2*(lmax-1)

      real(8):: rot(3,3)
      integer:: i
      rot=0.0d0
      do i=1,3
       rot(i,i)=1.0d0
      enddo  

      call  writemesh(rot,nl,lmax,lmx,'.cube')
      
      end program test

