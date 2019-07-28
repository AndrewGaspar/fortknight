module foo
  implicit none
  private

  public add
contains
  function add(x, y)
    integer, intent(in) :: x, y
    integer :: add

    add = x + y
  end function
end module foo