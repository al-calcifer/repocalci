program questao3
implicit none

integer ::LWORK, INFO, i, j
!real(kind=8) :: h(10,10)
real(kind=8) :: A(10,10), W(10)
real(kind=8), allocatable :: WORK(:)


do i=1, 10
	do j=1, 10
	if (j .eq. i+1) then
	A(i,j)=1.d0
	else if (j .eq. i-1) then
	A(i,j)=1.d0
	else if (j .eq. i) then
	A(i,j)=1.d0
	else if (j .eq. 8 .and. i .eq. 3) then
	A(i,j)=1.d0
	else if (j .eq. 3 .and. i .eq. 8) then
	A(i,j)=1.d0
	else if (j .eq. 1 .and. i .eq. 10) then
	A(i,j)=1.d0
	else if (j .eq. 10 .and. i .eq. 1) then
	A(i,j)=1.d0
	else
	A(i,j)=0.d0
	end if
	end do
end do



!1ª chamada
allocate(WORK(1))
call DSYEV ('V', 'U', 10, A, 10, W, WORK, -1, INFO)
LWORK=WORK(1)


deallocate(WORK)
allocate(WORK(LWORK))
!2ª chamada
call DSYEV ('V', 'U', 10, A, 10, W, WORK, LWORK, INFO)
do i=1, 10
	write(*,*) 'Autovalor', i, '=', W(i)
end do

do i=1, 10
	write(*,*) 'Autovetor', i, '=', A(:,i)
end do
deallocate(WORK)

end program 
