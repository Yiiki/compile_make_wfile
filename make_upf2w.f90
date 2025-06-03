      program make_upf2w
      implicit none
      character*256 :: fpp,fww
      ! fpp  : should be upf file, such as vwr.H.UPF, etc
      ! fww  : should be the output file,  H_W, etc
      integer*4 :: i,ii
      integer*4 :: tatom,nrr,rr_col,rho_col
      real*8 :: rho0,a0
      character*256 :: c_rho0,c_a0,c_nrr
      real*8,allocatable,dimension(:) :: rr,w00
 
      character*1024 :: tmp_words,head_words,tmp_word
      integer*4 :: id1,id2,ilen
      logical :: status

      character*256 :: c_tatom,cw_r,cw_h 

      call getarg(1,fpp)
      write(6,*) "reading ",trim(adjustl(fpp))
      open(12,file=trim(adjustl(fpp)))
      rewind(12)
      i=0
      do 
      i=i+1
      read(12,'(a1024)',end=1) tmp_word
!      read(tmp_words,*) head_words
!      write(6,'(A)') trim(adjustl(head_words))
      call notabs(tmp_word, tmp_words, ilen)
      call check_tag(trim(adjustl(tmp_words)),"PP_R",status)
      if(status) then
      write(6,*) "line ",i
      write(6,'(A)') trim(adjustl(tmp_words))

              id1=scan(tmp_words,"i")
              id2=scan(tmp_words,"c")
              read(tmp_words(id1+5:id2-3),*) nrr
              write(6,*) "found nrr=",nrr

              id1=scan(tmp_words,"n")
              id2=scan(tmp_words,">")
              read(tmp_words(id1+4:id2-2),*) rr_col
              write(6,*) "found rr_col=",rr_col

              allocate(rr(nrr))
              read(12,*) (rr(ii),ii=1,nrr)
              write(6,*) "maxval rr",maxval(rr)

              exit
      endif
      enddo
1     continue


      do 
      i=i+1
      read(12,'(a1024)',end=2) tmp_word
!      read(tmp_words,*) head_words
!      write(6,'(A)') trim(adjustl(head_words))
      call notabs(tmp_word, tmp_words, ilen)
      call check_tag(trim(adjustl(tmp_words)),"PP_RHOATOM",status)
      if(status) then
      write(6,*) "line ",i
      write(6,'(A)') trim(adjustl(tmp_words))

              id1=scan(tmp_words,"i")
              id2=scan(tmp_words,"c")
              read(tmp_words(id1+5:id2-3),*) nrr
              write(6,*) "found nrr=",nrr

              id1=scan(tmp_words,"n")
              id2=scan(tmp_words,">")
              read(tmp_words(id1+4:id2-2),*) rho_col
              write(6,*) "found rr_col=",rho_col

              allocate(w00(nrr))
              read(12,*) (w00(ii),ii=1,nrr)
              write(6,*) "maxval rho0",maxval(w00)

              exit
      endif
      enddo

2     continue      

      close(12)     
      call getarg(2,fww)
      call getarg(3,c_tatom)
      call getarg(4,c_rho0)
      call getarg(5,c_a0)
!     character*256 :: c_tatom,cw_r,cw_h 
!     integer*4 :: tatom,nrr
!     real*8 :: rho0,a0
!     character*256 :: c_rho0,c_a0,c_nrr
!     integer*4 :: i
!     real*8,allocatable,dimension(:) :: rr,w00
      read(c_tatom,*) tatom
      read(c_rho0,*) rho0
      read(c_a0,*) a0
      write(6,*) "rho0",rho0
      write(6,*) "a0",a0
      write(cw_r,*) rr_col
      write(cw_h,*) rho_col
      write(c_nrr,*) nrr
      open(11,file=fww)
      rewind(11)
      write(11,'(I6,A1,I5,A1,E24.15,A1,E24.15)') tatom,",",nrr,",",rho0,",",a0
      write(11,'(A)') "  <PP_R type=""real"" size="""//trim(adjustl(c_nrr))//""" columns="""//trim(adjustl(cw_r))//""">"
      write(11,100) (rr(i),i=1,nrr)
      write(11,'(A)') " </PP_R>"
      write(11,'(A)') "<PP_RHOATOM type=""real"" size="""//trim(adjustl(c_nrr))//""" columns="""//trim(adjustl(cw_h))//""">"
      write(11,200) (w00(i),i=1,nrr)
      write(11,'(A)') " </PP_RHOATOM>"
      write(11,'(A)') "</UPF>"
      close(11)

!100   format(<rr_col>(f10.4))      
100   format(<rr_col>(e24.15))      
!200   format(<rho_col>(e20.10))      
200   format(<rho_col>(e24.15))      
      contains
    !--------------------------------
    !@subroutine notabs(instr,outstr,ilen)
    !>convert one tab to eight spaces
    !$character(*),in instr the original string
    !$character(*),out outstr the converted string
    !$integer,out ilen  the len of converted string
    subroutine notabs(INSTR,OUTSTR,ILEN)
            use ISO_FORTRAN_ENV, only : ERROR_UNIT 
            character(len=*),intent(in)   :: INSTR     
            character(len=*),intent(out)  :: OUTSTR   
            integer,intent(out)           :: ILEN     
            integer,parameter             :: TABSIZE=8
            character(len=1)              :: c        
            integer                       :: ipos     
            integer                       :: lenin    
            integer                       :: lenout   
            integer                       :: i10      
            !---------------------------------------
            IPOS=1
            lenin=len(INSTR)
            lenin=len_trim(INSTR(1:lenin))
            lenout=len(OUTSTR)                      
            OUTSTR=" "                              
            do i10=1,lenin                         
                c=INSTR(i10:i10)
                if(ichar(c) == 9)then             
                    IPOS = IPOS + (TABSIZE - (mod(IPOS-1,TABSIZE)))
                else                             
                    if(IPOS > lenout)then
                        write(ERROR_UNIT,*)"*notabs* output string overflow"
                        exit
                    else
                        OUTSTR(IPOS:IPOS)=c
                        IPOS=IPOS+1
                    endif
                endif
            enddo
            ILEN=len_trim(OUTSTR(:IPOS))  
            return
    end subroutine notabs
    !----------------------------------
    !@subroutine check_tag(line,key,stat)
    !>check if a line begins with <key\space or <key\>
    !$character(*),in line the string 
    !$character(*),in key the checked tag
    !$logical,out stat True/False
 
      subroutine check_tag(line,key,stat)
            implicit none
            integer m,n
            character(len=*),intent(in) :: line
            character(len=*),intent(in) :: key
            logical,intent(out) :: stat
            m=len(adjustL(trim(line)))
            n=len(adjustL(trim(key)))
            stat=.false.
!            write(*,*) "----"
!            write(*,*) line(1:1)
!            write(*,*) line(2:2+n-1)
!            write(*,*) key(1:n)
!            write(*,*) line(2+n:2+n)
            if(line(1:1)=='<') then
                if(line(2:2+n-1)==key(1:n) .and. (line(2+n:2+n)==' ' .or. line(2+n:2+n)=='' .or. line(2+n:2+n)=='>')) stat=.true.
            endif
    endsubroutine check_tag
 
      end
