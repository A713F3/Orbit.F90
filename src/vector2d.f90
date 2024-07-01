! vector2d.f90
!
! 2D Vector definition and utils.
!
! Author:  Ali Efe Karagul
! GitHub:  https://github.com/A713F3/Orbit.F90

module vector2d_mod 
implicit none
    type :: vector2d
        real :: x, y
    contains
        procedure :: distance
        procedure :: add
        procedure :: add_constant
    end type

contains

    function distance(self, other)
        class(vector2d), intent(in) :: self
        class(vector2d), intent(in) :: other
        real                        :: distance

        distance = sqrt(real( (self%x - other%x)**2 +  (self%y - other%y)**2 ))
    end function distance

    subroutine add(self, other)
        class(vector2d), intent(inout) :: self
        class(vector2d), intent(in)    :: other

        self%x = self%x + other%x
        self%y = self%y + other%y
    end subroutine add

    subroutine add_constant(self, constant)
        class(vector2d), intent(inout) :: self
        real, intent(in)               :: constant

        self%x = self%x + constant
        self%y = self%y + constant
    end subroutine add_constant

end module vector2d_mod