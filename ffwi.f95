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
    integer :: stat, num_daily_entries, index = 1

    ! variables used for calculations
    integer, dimension(12) :: len_month
    real, dimension(12) :: day_length_dmc, day_length_dc
    real :: prev_ffmc, prev_dmc, prev_dc
    integer :: month, start_month, days_of_data
    real, dimension(365) :: temp_arr, rain_arr
    integer, dimension(365) :: humidity_arr, wind_arr

    ! opening file to read; unit is used to represent file - can be anything but the value of 6
    open(unit=20,file=fname,status='old',action='read')

    ! read length of months and day-length factors
    do month=1,12
        read(20,'(i2,f4.1,f4.1)') len_month(month), day_length_dmc(month), day_length_dc(month)
    end do

    ! read initial moisture code values, starting month, and # of days weather data is provided that month
    read(20,'(f4.1,f4.1,f5.1,i2,i2)') prev_ffmc, prev_dmc, prev_dc, start_month, days_of_data

    ! read daily weather data 
    do
        read(20,'(f4.1,i4,i4,f4.1)',IOSTAT=stat) temp_arr(index), humidity_arr(index), wind_arr(index), rain_arr(index)
        if (IS_IOSTAT_END(stat)) exit
        index = index + 1
    end do
    num_daily_entries = index-1

    ! close file after all data has been stored
    close(20, status='keep')

    return
end subroutine
