
function check_lt(x) result(b)
    character, intent(in) :: x ! input
    logical               :: b ! output
    if ((x == '1') .or. (x == '3') .or. (x == '5') .or. (x == '7')) then
        b = .TRUE.
    else
        b = .FALSE.
    end if
end function check_lt

function check_ul(x) result(b)
    character, intent(in) :: x ! input
    logical               :: b ! output
    if ((x == '2') .or. (x == '3') .or. (x == '6') .or. (x == '7')) then
        b = .TRUE.
    else
        b = .FALSE.
    end if
end function check_ul

function check_up(x) result(b)
    character, intent(in) :: x ! input
    logical               :: b ! output
    if ((x == '4') .or. (x == '5') .or. (x == '6') .or. (x == '7')) then
        b = .TRUE.
    else
        b = .FALSE.
    end if
end function check_up

character(len=20) function str(k)
    integer, intent(in) :: k
    write (str, *) k
    str = adjustl(str)
end function str

program zadaniec
    implicit none
    integer :: rmod = 999979
    CHARACTER(:), allocatable :: pre, tab
    integer :: m, n, size, i, k, iost, last_inc
    integer, dimension(:), allocatable :: count_pre, count_tab
    logical :: check_lt, check_ul, check_up
    character(20) :: str

    read *, m, n

    !print *, "m = ", m
    !print *, "n = ", n

    allocate (count_pre(n+1))
    allocate (count_tab(n+1))

    k = n+2
    pre = (repeat('X', k))
    !allocate (CHARACTER(k)::pre)
    tab = (repeat('X', k))
    !allocate (CHARACTER(k)::tab)

    !READ (*, "(A)", ADVANCE='NO', SIZE=size, EOR=10, END=10, IOSTAT=iost) pre
    READ *, pre
    10 continue

    count_pre(1) = 1
    do i = 2, n+1
        if (check_lt(pre((i-1):(i-1)))) then
            count_pre(i) = count_pre(i-1)
        else
            count_pre(i) = 0
        end if
    end do

    do k = 1, m
        !print *, "s1"
        !READ (*, "(A)", ADVANCE='NO', SIZE=size, EOR=20, END=20, IOSTAT=iost) tab
        READ (*, "(A)", ADVANCE='NO', SIZE=size, END=202, IOSTAT=iost) tab
        ! 201 continue
        !read *, tab
        ! 20 continue
        ! print *, "Executing loop"
        !print *, "Iostatus = ", iost
        if ((iost /= 0) .AND. (k /= m)) then
            !call EXIT(0)
        end if

        do i = 1, n+1
            count_tab(i) = 0

            if (i /= 1) then
                if (check_lt(tab((i-1):(i-1)))) then
                    count_tab(i) = count_tab(i) + count_tab(i-1)
                    !count_tab(i) = MOD(count_tab(i), rmod)
                end if

                if (check_ul(pre((i-1):(i-1)))) then
                    count_tab(i) = count_tab(i) + count_pre(i-1)
                    !count_tab(i) = MOD(count_tab(i), rmod)
                end if
            end if

            if (check_up(pre(i:i))) then
                count_tab(i) = count_tab(i) + count_pre(i)
                !count_tab(i) = MOD(count_tab(i), rmod)
            end if

            count_tab(i) = MOD(count_tab(i), rmod)
        end do
        !print *, "s2"

        do i = 1, n+1
            count_pre(i) = count_tab(i)
            pre = tab
        end do

        !print *, "There was no 202..."
        last_inc = k
        202 continue
        if (last_inc /= k) then
            print *, "dupa"
        end if
    end do

    !print *, "Pre: ", pre(1:1)
    !if (check_lt(pre(1:1))) then
    !    print *, "Nieparzysta"
    !else
    !    print *, "Parzysta"
    !end if
    !print *, "Tab: ", tab
    ! print *, "X: ", (pre(2))
    ! print *, "Wynik: ", (count_tab(n+1))
    !print *, "Last inc: ", last_inc
    write(*,"(A)") str(count_tab(n+1))
    call SLEEP(30)
    !close(10)
    call EXIT(0)
end
