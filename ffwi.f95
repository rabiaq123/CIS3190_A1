program ffwi
! program no.: f-95
! fortran 95 program to calculate canadian forest
! fire weather index for a DEC PDP-11 computer.
! reads data and prints out in metric units.
implicit none

    ! variables for file reading
    character(len=20) :: fname
    logical :: lexist
    
    write(*,1004)
    1004  format(5x,'FOREST FIRE WEATHER INDEX')

    ! error checking and reading input file
    write(*,*) 'Enter the filename:'
    read (*,'(A)') fname
    inquire(file=fname, exist=lexist)
    if (lexist) then
        write(*,*) 'reading the file'
        call read_file(fname);
    else
        write (*,*) 'Invalid input file name.'
    end if 
    
end program ffwi


! read input file
subroutine read_file(fname)
    implicit none 
    
    character (len=20), intent(in) :: fname
    integer :: month, index
    integer, dimension(12) :: len_month
    real, dimension(12) :: day_length_dmc, day_length_dc
    character (len = 50) :: format

    format = '(2x,i2,2x,f4.1,2x,f4.1)'

    ! read length of months and day-length factors
    open(unit=20,file=fname,status='old',action='read')
    do month=1,12
        read(20,'(i2,f4.1,f4.1)') len_month(month), day_length_dmc(month), day_length_dc(month)
    end do
    close(20, status='keep')

    do index=1,12
        write(*,format) len_month(index), day_length_dmc(index), day_length_dc(index)
    end do 

    return
end subroutine
