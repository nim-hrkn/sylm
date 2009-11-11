      program test
      integer,parameter:: lmax=2
      integer,parameter:: nl=lmax+1
      integer,parameter:: lmx=2*(lmax-1)
      real(8):: r(3), yl( (lmx+1)*(lmx+1) ), r2s,c((lmx+1)*(lmx+1)) 

      integer,parameter:: mng = 5
      real(8):: dlmm(-(nl-1):(nl-1),-(nl-1):(nl-1),0:nl-1,mng) 
      integer:: ng,ig 
      real(8):: symops(3,3,mng) 

      integer,parameter:: nx=10,ny=10,nz=10
      real(8):: x(3),g,val(-nx:nx,-ny:ny, -nz:nz,(lmx+1)*(lmx+1)) 
     . ,rr,x0(3), 
     .  rotvalL(-nx:nx,-ny:ny, -nz:nz, -lmax:lmax ),
     .  valL(-nx:nx,-ny:ny, -nz:nz, -lmax:lmax )
      integer:: ix,iy,iz

      integer:: ifout=10
      character(20):: fname

      real(8):: rotz(3,3),roty(3,3),rot(3,3), t,pi,p
      integer:: il,im ,ilm,nilm,im2
      integer:: i,j,k
      integer:: ilmlist(2,100)

      pi=2.0d0*asin(1.0d0)

      t=pi*45.0/180.0
      p=pi*60.0d0/180.0d0

c  x=r sin t cos p
c  y=r sin t sin t
c  z=r cos t

c
c    -t on y
c     c  0 -s
c     0  1 0
c     s  0  c
      roty=0.0d0
      roty(1,1)= cos(-t) 
      roty(3,1)= sin(t) 
      roty(2,2)= 1.0d0
      roty(1,3)= -sin(t)
      roty(3,3)= cos(t)

c    p on z
c    c  -s 0
c    s  c  0
c    0  0  1

      rotz=0.0d0
      rotz(1,1)= cos(p)
      rotz(2,1)= -sin(p)
      rotz(1,2)= sin(p)
      rotz(2,2)= cos(p)
      rotz(3,3)=1.0d0
c
c  rot= roty*rotz
c
      rot=0.0d0
      do i=1,3
        do j=1,3
          do k=1,3
          rot(i,k)= rot(i,k)+ roty(k,j)*rotz(j,i)
          enddo 
        enddo
      enddo

      do i=1,3
        write(*,'(3f10.5)') roty(1:3,i)
      enddo
      do i=1,3
        write(*,'(3f10.5)') rotz(1:3,i)
      enddo
      do i=1,3
        write(*,'(3f10.5)') rot(1:3,i)
      enddo
      call rotdlmm(rot, 1, nl, dlmm) 

      do il=0,lmax
      write(*,*) 'il=',il
         do im=-lmax,lmax
         write(*,'(10f10.5)')  dlmm( -lmax:lmax, im, il, 1) 
         enddo
      enddo

      ilm=0
      do il=0,lmax
      do im=-il,il
      ilm=ilm+1
      ilmlist(1,ilm)= il
      ilmlist(2,ilm)= im
      enddo
      enddo
      nilm=ilm
      write(*,'(20i3)')  ( i,i=1,nilm )
      write(*,'(20i3)')  ( ilmlist(1,1:nilm) )
      write(*,'(20i3)')  ( ilmlist(2,1:nilm) )

      range=3.0d0
      write(*,*) lmax,lmx, (lmx+1)*(lmx+1) 
      do ix=-nx,nx
      do iy=-ny,ny
      do iz=-nz,nz
         x0(1)=range*dble(ix)/nx
         x0(2)=range*dble(iy)/ny
         if (nz.eq.0) then
           x0(3)=0.0d0
         else
         x0(3)=range*dble(iz)/nz
         endif
         rr=x0(1)**2+x0(2)**2+x0(3)**2 
         x=x0/sqrt(rr) 
         call sylm(x,yl,lmx,r2s)
         call sylmnc(c,lmx)
         g0=nx/4
         g=exp(-rr/g0**2) 

         do id=1, (lmx+1)**2
         val(ix,iy,iz,id)=yl(id)*g*c(id)
         enddo

c         do id=1,(lmx+1)**2
c         write(10+id,'(3i5,10F15.5)') ix,iy,iz,x0,yl(id)*g*c(id)
c         enddo

      enddo
      enddo
c         do id=1,(lmx+1)**2
c         write(10+id,*) ' '
c         enddo
      enddo

      write(*,*) id, 'max','min'
      do id=1,(lmx+1)**2
        write(*,*) id, maxval(val(:,:,:,id)), minval(val(:,:,:,id))
      enddo

      id=0
      do il=0,2

      valL(:,:,:,-il:il)= val(:,:,:,id+1:id+1+2*il+1) 

      do im=-il,il
         write(*,'(10f10.5)') dlmm(:,im,il,1)
      enddo 

      ! rotate
      !   Ylm'= \sum m' Ylm Dmm'
      rotvalL=0.0d0
       do im=-il,il
         do im2=-il,il
           rotvalL(:,:,:,im)=  rotvalL(:,:,:,im) + 
     .  valL(:,:,:,im2)*dlmm(im2,im,il,1)
         enddo
       enddo 

       do im=-il,il
         write(*,*) 'max,min=',maxval(rotvalL(:,:,:,im)),
     .          minval(rotvalL(:,:,:,im)) 
       enddo

      do im=-il,il
      id=id+1
      ! rotate 

      ifout=10+id
      write(fname,'(i02,a)')ifout,'.cube'
      open(file=fname,unit=ifout) 
      write(ifout,*)'TEST', id
      write(ifout,*)'TEST'
      write(ifout,110) 4, -1.0d0*range, -1.0d0*range, -1.0d0*range
 110  format(I5,5F12.6)
 120  format(6E13.6)
      write(ifout,110) 2*nx+1, range*1.0d0/nx, 0.0d0, 0.0d0
      write(ifout,110) 2*ny+1, 0.0d0,range*1.0d0/ny, 0.0d0
      write(ifout,110) 2*nz+1, 0.0d0,0.0d0,range*1.0d0/nz
      write(ifout,110) 1, 0.0d0, 0.0d0, 0.0d0, 0.0d0
      write(ifout,110) 2, 0.0d0, 0.0d0, 0.0d0, 1.0d0
      write(ifout,110) 3, 0.0d0, 2.0d0, 0.0d0, 0.0d0
      write(ifout,110) 4, 0.0d0, 0.0d0, 3.0d0, 0.0d0
c      write(ifout,'(3x2i4)') 1 , 1

      

      do ix=-nx,nx
      do iy=-ny,ny
c         write(ifout,120) val(ix,iy,:,id)
         write(ifout,120) rotvalL(ix,iy,:,im)
      enddo
      enddo

      close(ifout)
      enddo
      enddo 
      
      end program test

