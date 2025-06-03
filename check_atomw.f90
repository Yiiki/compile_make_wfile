      program check_atomw
      implicit none
      character*256 :: file_w
      integer*4 :: iiatom,nrr,i
      real*8 :: rho0,a0,sum1,sum2,pi,rad_box
      real*8,allocatable,dimension(:) :: rr,w00,w
      integer*4 :: m,midx(1)
      real*8 :: r,f1,f2
      real*8,allocatable,dimension(:) :: w11,w22,nwok
      integer*4 :: id

      pi = 4.d0*datan(1.d0)
      open(10,file="motif.input")
      rewind(10)
      read(10,*) file_w
      read(10,*) rad_box
      close(10)

      id=index(file_w,"_W")
      if(id.lt.2) then
        write(6,*) "*_W file_W filename expected"
        stop
      end if

      open(10,file=trim(adjustl(file_w)))
      rewind(10)
      read(10,*) iiatom,nrr,rho0,a0
      allocate(rr(nrr))
      allocate(w00(nrr),w(nrr))
      read(10,*)
      read(10,*) (rr(i),i=1,nrr)
      read(10,*)
      read(10,*)
      read(10,*) (w00(i),i=1,nrr)

      sum1=0.d0
      sum2=0.d0
      do i=2,nrr-1
      sum1=sum1+W00(i)*(rr(i+1)-rr(i-1))/2
      w00(i)=w00(i)/(4*pi*rr(i)**2)
      W(i)=w00(i)+rho0*exp(-(rr(i)/a0)**2)
      sum2=sum2+W(i)*4*pi*rr(i)**2*(rr(i+1)-rr(i-1))/2
      if(sum2/=sum2) then
              write(6,*) "NaN met !!!!!!!!!!!!!!!!!!!!"
              write(6,*) "W(i),rr(i),rr(i+1),rr(i-1),i"
              write(6,*) W(i),rr(i),rr(i+1),rr(i-1),i
              stop
      endif
      enddo
      write(6,*) "atom Q",iiatom,sum1,sum2
      w00(1)=w00(2)
      w00(nrr)=w00(nrr-1)
      w(1)=w(2)
      w(nrr)=w(nrr-1)

      do i=1,nrr
      rr(i)=rr(i)*0.529177d0   ! input r is in Bohr unit
      if(rr(i).lt.rad_box*1.01) then
!     w(i,ia)=w(i,ia)**2*cos(rr(i,ia)/rad_box*pi/2)**2
!     w(i)=w(i)*cos(rr(i)/(rad_box*1.01)*pi/2)**2
      w(i)=w(i)*cos(rr(i)/(rad_box*1.01)*pi/2)
      else
      w(i)=0.d0
      endif
      enddo

!      if(abs(rr(nrr)/nrr-rr(nrr/2)/(nrr/2)) &
!      .gt.1.E-2*rr(nrr)/nrr) then
!      write(6,*) "atom_W, not a linear 1D grid,stop"
!      endif

      close(10)

!   vwr use a non-linear mesh, while pbe use a linear mesh.
!   however, gen_motif.f90 and gen_motall_list.f both relies on that.
!   so, here we check what if one intepolate the w(i) 
!   the actual w-function would look ?
      allocate(w11(nrr))
      do i=1,nrr
      r=rr(i)
      m=r*(nrr-1)/rr(nrr)+1
      ! for nonlinear mesh, m is totally different with i !!!!!
      if(m.eq.nrr) then
              w11(i)=w(nrr)
      else
              f1=(rr(m+1)-r)/(rr(m+1)-rr(m))
              f2=(r-rr(m))/(rr(m+1)-rr(m))
              w11(i)=f1*w(m)+f2*w(m+1)
      endif

      enddo

! so, how can we fix this error ?
! the key point is to correctly find m index.
      allocate(w22(nrr),nwok(nrr))
      nwok=[(i,i=1,nrr)]
      !allocate(midx(1))
      do i=1,nrr
      r=rr(i)
      if(r.ge.rr(nrr)) then
              m=nrr
      else
      !m=minval(nwok,mask=(rr>r))-1
      midx=minloc(rr,mask=(rr>r))-1
      m=midx(1)
      endif
      ! for nonlinear mesh, m is totally different with i !!!!!
      if(m.eq.nrr) then
              w22(i)=w(nrr)
      else
              f1=(rr(m+1)-r)/(rr(m+1)-rr(m))
              f2=(r-rr(m))/(rr(m+1)-rr(m))
              w22(i)=f1*w(m)+f2*w(m+1)
      endif

      enddo

     

      open(11,file="atom_W."//file_W(1:id-1))
      rewind(11)
      write(11,"(5(A15,1x))") "r(A)","old(e/Bohr3)","new(e/Bohr3)","debug-1","debug-2"
      do i=1,nrr
      write(11,"(5(E15.7,1x))") rr(i)/0.529177d0,w00(i),w(i),w11(i),w22(i)
      enddo
      close(11)


      stop
      end
