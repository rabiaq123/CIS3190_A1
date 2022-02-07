program ffwi
! program no.: f-95
! fortran 95 program to calculate canadian forest
! fire weather index for a DEC PDP-11 computer.
! reads data and prints out in metric units.
    use FFWIndices
    implicit none 

    ! variables for file reading
    character(len=20) :: input_fname, output_fname
    logical :: input_exists = .false.
    ! variables used for calculations
    integer, dimension(12) :: len_month
    real, dimension(12) :: day_length_dmc, day_length_dc
    real :: prev_ffmc, prev_dmc, prev_dc
    integer :: start_month, days_of_data, num_daily_entries
    real, dimension(366) :: temp_arr, rain_arr
    integer, dimension(366) :: humidity_arr, wind_arr

50  format(//,1x,'FOREST FIRE WEATHER INDEX',/)

    write(*,50)
    call display_unit_note()
 
    ! get input and output filenames and perform error checking
    call get_filenames(input_exists, input_fname, output_fname)

    ! storing values from input file
    open(unit=20,file=input_fname,status='old',action='read')
    call read_section1(len_month, day_length_dmc, day_length_dc)
    call read_section2(prev_ffmc, prev_dmc, prev_dc, start_month, days_of_data)
    call read_section3(num_daily_entries, temp_arr, rain_arr, humidity_arr, wind_arr)
    close(20, status='keep')
    
    ! performing calculations to print values to output file
    call perform_calcs(output_fname, len_month, day_length_dmc, day_length_dc, prev_ffmc, prev_dmc, prev_dc, & 
    start_month, days_of_data, num_daily_entries, temp_arr, rain_arr, humidity_arr, wind_arr)

    write(*,*)
    write(*,*) 'Your output file is ready to be viewed!'
    write(*,*) 'You will find a legend accompanying the results compiled for you within your file.'

contains 


! print a message on console showing user the units needed for accurate calculations
subroutine display_unit_note()
    implicit none 

    write(*,*) ' Please note the units this program uses for daily weather data:'
    write(*,*) ' temperature: °C'
    write(*,*) ' relative humidity: %'
    write(*,*) ' wind speed: km/h'
    write(*,*) ' rainfall: mm'
    write(*,*)

    return 
end subroutine display_unit_note


! get input and output filenames, prompt user to re-enter filename if error
subroutine get_filenames(input_exists, input_fname, output_fname)
    implicit none 

    character(len=20), intent(out) :: input_fname, output_fname
    logical, intent(inout) :: input_exists

    ! reading input file and error checking
    write(*,*) 'Enter the filename to read from:'
    do while (input_exists .eqv. .false.)
        read (*,'(A)') input_fname
        inquire(file=input_fname, exist=input_exists)
        if (input_exists .eqv. .false.) write (*,*) 'An input file with this name does not exist. Please try again.'
    end do

    ! getting output file name
    write(*,*) 'Enter the name of the file to output to:'
    read (*,'(A)') output_fname

    return
end subroutine get_filenames


! read length of months and day-length factors
subroutine read_section1(len_month, day_length_dmc, day_length_dc)
    implicit none

    integer :: index
    integer, dimension(12), intent(out) :: len_month
    real, dimension(12), intent(out) :: day_length_dmc, day_length_dc

    do index=1,12
        read(20,'(i2,f4.1,f4.1)') len_month(index), day_length_dmc(index), day_length_dc(index)
    end do

    return
end subroutine read_section1


! read initial moisture code values, starting month, and # of days weather data is provided that month
subroutine read_section2(prev_ffmc, prev_dmc, prev_dc, start_month, days_of_data)
    implicit none

    real, intent(out) :: prev_ffmc, prev_dmc, prev_dc
    integer, intent(out) :: start_month, days_of_data

    read(20,'(f4.1,f4.1,f5.1,i2,i2)') prev_ffmc, prev_dmc, prev_dc, start_month, days_of_data

    return
end subroutine read_section2


! read daily weather data 
subroutine read_section3(num_daily_entries, temp_arr, rain_arr, humidity_arr, wind_arr)
    implicit none

    integer :: stat, index = 1
    integer, intent(out) :: num_daily_entries
    real, dimension(366), intent(out) :: temp_arr, rain_arr
    integer, dimension(366), intent(out) :: humidity_arr, wind_arr

    do
        read(20,'(f4.1,i4,i4,f4.1)',IOSTAT=stat) temp_arr(index), humidity_arr(index), wind_arr(index), rain_arr(index)
        if (IS_IOSTAT_END(stat)) exit
        index = index + 1
    end do
    num_daily_entries = index-1

    return
end subroutine read_section3


end program ffwi
