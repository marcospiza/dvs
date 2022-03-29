program read_files_python
implicit none

integer,parameter                :: maxL = 100
integer,parameter                :: maxL2 = 1000
integer,parameter                :: maxD = 1000

real*8,allocatable,dimension(:)  :: Tmed
real*8,allocatable,dimension(:)  :: t1900
integer,allocatable,dimension(:) :: dia,mes,ano
character(len=10),allocatable, &
          &dimension(:)          :: date



character(len=40)               :: dir_file
real*8,allocatable,dimension(:) :: dtsmtb  
real*8,dimension(maxL)          :: dummy1
real*8,dimension(maxL2)         :: dummy2

real*8                          :: tbasem
real*8                          :: tsumem
real*8                          :: tsum1
real*8                          :: tsum2
real*8                          :: iemtsum
real*8                          :: dateplant1900
real*8                          :: tav
real*8                          :: dvr
real*8                          :: idvs

character(len=10)               :: dateplant
integer                         :: i
integer                         :: iplant
integer                         :: dap = 0


print*,"Before starting, verify if the following files are in the same &
       & directoy of this program:"
print*,"cassava.w41"
print*,"dados_meteo.csv"
print*,""
print*,"Press Enter to continue"
read(*,*)




! ---- reading crop file ----------------------

dir_file = "cassava.w41" 
call readcropfile(dir_file)



! read meteofile
call readmeteodata("dados_meteo.csv")


! Startting the main computations

open(10,file="dvs_fortran.out",status="replace")

write(10,"(A)")"*DSV program by Marcos Alex"
write(10,"(A)")"*Find below the values of dvs calculated with information taken from the following files"
write(10,"(A)")"*meteorological file name : dados_meteo.csv"
write(10,"(A)")"*crop file name           : cassava.w41"

write(10,"(A)")"****************************************"
write(10,"(A12,A5,A10)")"Data","DAP","DVS"
write(10,"(A)")"****************************************"

i = 1
do while(t1900(i) < dateplant1900  )
 i = i + 1
enddo

iemtsum = 0.d0
idvs = 0.d0
do while (idvs < 2.0 .and. i < size(Tmed)) 

  iemtsum = iemtsum + (Tmed(i) - tbasem)
  if (iemtsum < tsumem) then
     idvs = 0d0
  else
     tav = afgen(dtsmtb,Tmed(i))
     if (idvs <= 1.0) then
         dvr = tav/tsum1
     else
         dvr = tav/tsum2
     endif
     
     idvs = idvs + dvr
  endif
  dap = dap + 1
  print*,date(i+1)
  write(10,"(A12,I5,F10.3 )")date(i+1),dap,idvs
  i = i + 1
enddo


close(10)

print*,""
print*,"OK"
print*,"file dvs_fortran.out was created"
print*,""
print*,"Press Enter to close the program"
read(*,*)



contains 

subroutine readcropfile(filename)
implicit none
character(len=*) :: filename
integer         :: ilc
integer,dimension(6):: datea



call RDINIT (30,40,filename)

call RDADOU ("dtsmtb",dummy1,maxL,ilc)
allocate(dtsmtb(ilc))
dtsmtb = dummy1(1:ilc)

call RDSDOU ("TBASEM",tbasem)
call RDSDOU ("tsum1",tsum1)
call RDSDOU ("tsum2",tsum2)
call RDSDOU ("tsumem",tsumem)
call RDSCHA ("dateplant",dateplant)

! convert dateplant to 1900 time
read(dateplant(7:10),"(I4)")  datea(1)
read(dateplant(4:5),"(I2)")   datea(2)
read(dateplant(1:2),"(I2)")   datea(3)
do i = 4,6
  datea(i) = 0
enddo
call DTARDP(datea,0.0,dateplant1900)
!----------------------------------------




close(30)
call RDDTMP(10)

end subroutine readcropfile


subroutine chartoint(ch,varint)
implicit none
character(len=*),intent(in)   :: ch
integer,intent(out)           :: varint

if (ch == "") then
   print*,"Warning: there are missing data in the meteofile"
   varint = -999
else
  read(ch,'(I10)')varint
endif

end subroutine chartoint

subroutine chartoR8(ch,varout)
implicit none
character(len=*),intent(in)   :: ch
real*8,intent(out)            :: varout

if (ch == " ") then
   print*,"Warning: there are missing data in the meteofile"
   varout = -999.9D0
else
  read(ch,'(F10.5)')varout
endif

end subroutine chartoR8

subroutine sepchar(char1,varch,fildsep)
implicit none
character(len=*),intent(in):: fildsep
character(len=*),intent(in):: char1
character(len=*),dimension(4),intent(out):: varch

integer n1,i
character(len = 5)              ::ch

 n1 = 0
 ch = ''
 do i=1,len(char1)
   if(char1(i:i) == fildsep .and. n1 <=4) then
      n1 = n1 + 1
      varch(n1) = ch
      ch =""
   else
      ch = trim(ch)//char1(i:i)
   endif
 enddo
end subroutine sepchar

! -------------------------------------------
function datefmtFchar(dd,mm,yyyy)
implicit none
character(len=10)          :: datefmtFchar
character(len=*),intent(in)::dd,mm,yyyy
character(len=2)           :: idd,imm

if (len(trim(dd)) == 1) then
  idd = "0"//trim(dd)
else
  idd = trim(dd)
endif

if (len(trim(mm)) == 1) then
   imm = "0"//trim(mm)
else
  imm = trim(mm)
endif

datefmtFchar = idd//"-"//imm//"-"//trim(yyyy)


end function datefmtFchar

subroutine readmeteodata(fn)
implicit none 
character(len=*),intent(in):: fn
! locals
integer,allocatable,dimension(:) :: id1,id2,id3
real*8,allocatable,dimension(:)  :: rd1
character(len=10),allocatable, &
          &dimension(:)          :: dumdate
character(len=10)                :: itimefrm
character(len=5),dimension(4)    :: varch
character(len=16)                :: char1
real*8,allocatable,dimension(:)  :: t1900D
integer,dimension(6)             :: datea

integer                          :: IO = 0
integer                          :: n,i

open(10,file=trim(fn),status="old")

! skip two lines
read(10,*) ; read(10,*)

! Allocating  dummy 1-D arrays
allocate(id1(maxD))
allocate(id2(maxD))
allocate(id3(maxD))
allocate(rd1(maxD))
allocate(dumdate(maxD))
allocate(t1900D(maxD))

n = 0
do while (IO == 0)
 read(10,"(A16)",iostat= io)char1
 call sepchar(char1,varch,",")   ! Gets a char varriable, separete by "," and stores into 
                                 ! char1 character 1-D array
 n = n + 1
 dumdate(n) = datefmtFchar(varch(1),varch(2),varch(3))

 call chartoint(varch(1),id1(n))
 call chartoint(varch(2),id2(n))
 call chartoint(varch(3),id3(n))
 call chartoR8(varch(4),rd1(n))

!converting date to t1900 real time
 datea(1) = id3(n)
 datea(2) = id2(n)
 datea(3) = id1(n)
 datea(4) = 0; datea(5) = 0; datea(6) = 0
 call DTARDP(datea,0.0,t1900D(n))
 
enddo

!-- Allocating the variables day,month and year ,and Tmed
allocate(dia(n))
allocate(mes(n))
allocate(ano(n))
allocate(Tmed(n))
allocate(date(n))
allocate(t1900(n))

dia  = id1(1:n)
mes  = id2(1:n)
ano  = id3(1:n)
Tmed = rd1(1:n)
date = dumdate(1:n)
t1900 = t1900D(1:n)

! Deallocating the dummy 1d arrays
deallocate(id1);deallocate(id2);deallocate(id3);deallocate(rd1)
deallocate(dumdate); deallocate(t1900D)
close(10)

end subroutine readmeteodata


function afgen(array,x)
! Interpolates linearly an 1-D array values in the format x1,y1,x2,y2,...xn,yn
implicit none
real*8 afgen
real*8,dimension(:),intent(in) :: array
real*8,intent(in)              :: x

! locals
real*8  :: slope
integer :: i,n

n = size(array)

if ( x < array(1)) then
    afgen = array(2)
    return
else
   do i = 3,n-1,2
      if ( x <= array(i)) then
         slope = (array(i+1) - array(i-1))/(array(i) - array(i-2))
         afgen = array(i-1) + slope * (x - array(i-2))
         return
      endif
   enddo
endif 

! It means x is greater than last x in the array
afgen = array(n)

end function afgen


end program read_files_python
