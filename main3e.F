      program test
      use m_eulerangle 
      implicit none
      integer,parameter:: lmax=2
      integer,parameter:: nl=lmax+1
      integer,parameter:: lmx=2*(lmax-1)

      integer,parameter:: mng = 5
      real(8):: dlmm(-(nl-1):(nl-1),-(nl-1):(nl-1),0:nl-1,mng) 


      real(8):: rot(3,3)
      real(8):: pi,axisc(3)
      integer:: il,im 
      integer:: i
      real(8):: axisa(3)

      pi=2.0d0*asin(1.0d0)

 130  format(a,9F20.10) 

      axisc=(/1.0d0,1.0d0,0.5d0/)
c      axisc=(/.0d0,.0d0,1.d0/)
      axisa=(/1.0d0,0.0d0,.0d0/)

c#if 0
c      call axisac2rot(axisa,axisc,rot,0.0d0)
c
c      call writemesh(rot,nl,lmax,lmx,'b.cube')
c
c      write(*,*) '----------------------'
c
c#endif
      call axisac2rot(axisa,axisc,rot)
      do i=1,3
        write(*,*) rot(:,i)
      enddo

      call rotdlmm(rot, 1, nl, dlmm) 

      do il=0,lmax
      write(*,*) 'dlmm,il=',il
         do im=-lmax,lmax
         write(*,'(10f10.5)')  dlmm( -lmax:lmax, im, il, 1) 
         enddo
      enddo
c rotate
c         Ylm= \sum m' Ylm' Dm'm

      call writemesh2(nl,lmax,lmx,dlmm,'c.cube')

      end 


