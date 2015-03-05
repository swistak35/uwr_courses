
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
    !character, dimension(:), allocatable :: pre, tab
    integer :: m, n, size, i, k, iost, last_inc
    integer, dimension(:), allocatable :: count_pre, count_tab
    logical :: check_lt, check_ul, check_up
    character :: c
    character(20) :: str

    read *, m, n

    allocate (count_pre(n+1))
    allocate (count_tab(n+1))

    k = n+2
    !allocate (pre(k))
    !allocate (tab(k))
    pre = (repeat('X', k))
    !allocate (CHARACTER(k)::pre)
    tab = (repeat('X', k))
    !allocate (CHARACTER(k)::tab)

    READ *, pre
    !READ (*, "(A1)", ADVANCE='NO', SIZE=size, EOR=20, END=20, IOSTAT=iost) pre
    !20 continue

    count_pre(1) = 1
    do i = 2, n+1
        !c = pre((i-1):(i-1))
        !if ('0' == c) then
        if (check_lt(pre((i-1):(i-1)))) then
            count_pre(i) = count_pre(i-1)
        else
            count_pre(i) = 0
        end if
    end do

    do k = 1, m
        read *, tab

        do i = 1, n+1
            count_tab(i) = 0

            if (i /= 1) then
                if (check_lt(tab((i-1):(i-1)))) then
                    count_tab(i) = count_tab(i) + count_tab(i-1)
                end if

                if (check_ul(pre((i-1):(i-1)))) then
                    count_tab(i) = count_tab(i) + count_pre(i-1)
                end if
            end if

            if (check_up(pre(i:i))) then
                count_tab(i) = count_tab(i) + count_pre(i)
            end if

            count_tab(i) = MOD(count_tab(i), rmod)
        end do

        do i = 1, n+1
            count_pre(i) = count_tab(i)
            pre = tab
        end do
    end do

    write(*,"(A)") str(count_tab(n+1))
    !call SLEEP(30)
    call EXIT(0)
end
