program zadaniec
    implicit none
    integer :: rmod = 999979
    CHARACTER(:), allocatable :: pre, tab
    integer :: m, n, size, i, k, iost, last_inc
    integer, dimension(:), allocatable :: count_pre, count_tab
    character(20) :: str

    read *, m, n

    allocate (count_pre(n+1))
    allocate (count_tab(n+1))

    k = n+2
    pre = (repeat('X', k))
    tab = (repeat('X', k))
    READ *, pre
    READ *, tab
    count_tab(0) = 5
    count_tab(n) = 5
    count_pre(0) = 5
    count_pre(n) = 5

    call SLEEP(30)
    call EXIT(0)
end