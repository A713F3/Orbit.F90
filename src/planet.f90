! planet.f90
!
! Planet definitions.
!
! Author:  Ali Efe Karagul
! GitHub:  https://github.com/A713F3/Orbit.F90

module planet_mod
use :: sdl2
use :: circle_mod
use :: vector2d_mod
implicit none

    type :: planet
        real :: mass
        type(sdl_circle) :: object
        type(vector2d) :: velo
    contains
        procedure :: get_pos
        procedure :: get_pos_pointer
        procedure :: get_velo
        procedure :: get_velo_pointer
        procedure :: get_r
        procedure :: render
        procedure :: move
        procedure :: attract
        procedure :: collide
    end type

    real :: G = 10.0

contains

    function get_pos(self) result(pos)
        class(planet), intent(in) :: self
        type(vector2d)            :: pos
    
        pos = self%object%pos
    end function get_pos

    function get_pos_pointer(self) result(pos)
        class(planet), target, intent(in) :: self
        type(vector2d), pointer           :: pos
    
        pos => self%object%pos
    end function get_pos_pointer

    function get_velo(self) result(velo)
        class(planet), intent(in) :: self
        type(vector2d)            :: velo
    
        velo = self%velo
    end function get_velo

    function get_velo_pointer(self) result(velo)
        class(planet), target, intent(in) :: self
        type(vector2d), pointer           :: velo
    
        velo => self%velo
    end function get_velo_pointer

    function get_r(self) result(r)
        class(planet), intent(in) :: self
        real                      :: r
    
        r = self%object%r
    end function get_r

    function render(self, renderer) result(rc)
        class(planet), intent(in) :: self
        type(c_ptr), intent(out)  :: renderer

        integer :: rc

        rc = self%object%render_fill(renderer)
    end function

    subroutine move(self, EULER_COEFF)
        class(planet), intent(in) :: self
        real, intent(in)          :: EULER_COEFF

        type(vector2d), pointer :: pos, velo
        type(vector2d) :: tmp_velo

        pos => self%get_pos_pointer()
        velo => self%get_velo_pointer()

        tmp_velo = velo

        velo = velo%mul(EULER_COEFF)
        pos = pos%add(velo)

        velo = tmp_velo

    end subroutine

    subroutine attract(self, other_planet, EULER_COEFF)
        class(planet), intent(in) :: self
        class(planet), intent(in) :: other_planet
        real, intent(in)          :: EULER_COEFF

        type(vector2d), pointer :: pos1, pos2, velo1, velo2
        type(vector2d) :: acc
        real :: force, r, cosa, sina, fx, fy

        pos1 => self%get_pos_pointer()
        pos2 => other_planet%get_pos_pointer()

        velo1 => self%get_velo_pointer()
        velo2 => other_planet%get_velo_pointer()

        r = pos1%distance(pos2)

        force = EULER_COEFF * G * (self%mass * other_planet%mass) / (r*r)

        cosa = (pos1%x - pos2%x) / r
        sina = (pos1%y - pos2%y) / r

        fx = cosa * force
        fy = sina * force

        acc%x = fx / other_planet%mass
        acc%y = fy / other_planet%mass

        velo2 = velo2%add(acc)

    end subroutine attract

    subroutine collide(self, other_planet)
        class(planet), intent(inout)  :: self
        class(planet), intent(inout)  :: other_planet

        logical :: is_collided
        type(vector2d), pointer :: pos1, pos2
        type(vector2d) :: v1a, v2a, displacement, direction
        real :: r, overlap

        pos1 => self%get_pos_pointer()
        pos2 => other_planet%get_pos_pointer()

        r = pos1%distance(pos2)

        is_collided = r .le. (self%get_r() + other_planet%get_r())

        if (is_collided) then
            overlap = (self%get_r() + other_planet%get_r()) - r

            direction = pos2%sub(pos1)
            direction = direction%mul(1 / r)

            displacement = direction%mul(overlap / 2.0)

            !pos1 = pos1%sub(displacement) 
            !pos2 = pos2%add(displacement)

            !self%object%pos = pos1
            !other_planet%object%pos = pos2

            v1a = velocity_after_collision(self, other_planet)
            v2a = velocity_after_collision(other_planet, self)

            self%velo = v1a
            other_planet%velo = v2a
        end if

    end subroutine collide

    function velocity_after_collision(self, other) result(v)
        type(planet), intent(in)  :: self
        type(planet), intent(in)  :: other

        type(vector2d) :: v, x1, x2, v1, v2, tmp1, tmp2
        real :: numerator, denominator, division
        real :: m1, m2, first

        x1 = self%get_pos()
        x2 = other%get_pos()

        v1 = self%get_velo()
        v2 = self%get_velo()
        
        m1 = self%mass
        m2 = other%mass

        first = (2.0 * m2) / (m1 + m2)

        tmp1 = v1%sub(v2)
        tmp2 = x1%sub(x2)
        numerator = tmp1%dot(tmp2)

        tmp1 = x1%sub(x2)
        denominator = tmp1%norm() ** 2

        division = numerator / denominator

        tmp1 = x1%sub(x2)
        tmp1 = tmp1%mul(first * division)

        v = v1%sub(tmp1)

        write(*,*) v%x, v%y

    end function velocity_after_collision

end module planet_mod