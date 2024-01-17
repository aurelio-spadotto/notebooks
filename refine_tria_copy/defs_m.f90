module defs 
implicit none

!===============================
! TYPE DEFINITIONS
!===============================

!===============================
! Static Grid: info stored on arrays
type static_grid_t
        ! Dimension of the grid
        integer :: nnode, nelem, npair
        ! Connectivities
        ! One should initialize connectivities to -1 if the 
        ! number of actual pairs/nodes is lower than the max
        integer, dimension(:,:),allocatable :: elem2node
        integer, dimension(:,:),allocatable :: elem2pair
        integer, dimension(:),allocatable   :: pair2node1 
        integer, dimension(:),allocatable   :: pair2node2
        ! Coordinates
        real,    dimension(:,:),allocatable :: x_node
end type static_grid_t
!===============================

!===============================
! Dynamic node (list): allocated dynamically
type dyn_node_t
        ! 2D Coordinates
        real, dimension(2) :: coord
        ! Index of static version 
        integer :: static_idx
        ! Pointer to next in the list
        type(dyn_node_t),pointer :: next 
end type dyn_node_t 
!===============================

!===============================
! Type for pointers to dyn_node_t (necessary to have arrays of pointers)
type dyn_node_ptr_t
        type(dyn_node_t), pointer :: ptr
end type dyn_node_ptr_t
!===============================
! Dynamic Pair (list of binary trees): dyn. alloc.
type dyn_pair_t
        ! Pointers to nodes (dynamically represented)
        type(dyn_node_ptr_t), dimension(2) :: nodes
        ! Activation
        logical :: activate
        ! Index of static version
        integer :: static_idx
        ! Position in list 
        integer :: pos_in_list
        ! Pair barycenter coordinates
        real, dimension(2) :: barycenter
        ! Pointer to children pairs
        type(dyn_pair_t), pointer :: child1
        type(dyn_pair_t), pointer :: child2
        ! Pointer to next in the list 
        type(dyn_pair_t), pointer :: next
end type dyn_pair_t
!===============================
!===============================
! Type for pointers to dyn_pair_t (necessary to have arrays of pointers)
type dyn_pair_ptr_t
        type(dyn_pair_t), pointer :: ptr
end type dyn_pair_ptr_t
!
!===============================
! Dynamic Element (list of Quaternary trees): dyn. alloc.
type dyn_elem_t
        ! Pointers to pairs
        type(dyn_pair_ptr_t), dimension(3) :: pairs
        ! Pointers to vertices
        ! Notice: Vertices are only three,
        !  but an element can have more than three nodes   
        type(dyn_node_ptr_t), dimension(3) :: vertices
        ! Activation
        logical :: activate
        ! Index of static version
        integer :: static_idx
        ! Position in list 
        integer :: pos_in_list
        ! Pointer to children elements
        ! Barycenter
        real, dimension(2) :: barycenter
        type(dyn_elem_t), pointer :: child1
        type(dyn_elem_t), pointer :: child2
        type(dyn_elem_t), pointer :: child3
        type(dyn_elem_t), pointer :: child4
        ! Pointer to next in list
        type(dyn_elem_t), pointer :: next
end type dyn_elem_t 
!===============================
!===============================
! Type for pointers to dyn_elem_t (necessary to have arrays of pointers)
type dyn_elem_ptr_t
        type(dyn_elem_t), pointer :: ptr
end type dyn_elem_ptr_t
!
!===============================
! Dynamic Grid: inherits static data and adds dynamic data
type dynamic_grid_t
        ! Dimension of the grid (count also inactive pairs and elements)
        integer :: nnode, nelem, npair
        ! Dimension of active grid
        integer :: nelem_active, npair_active
        ! Static Connectivities
        integer, dimension(:,:),allocatable :: elem2node
        integer, dimension(:,:),allocatable :: elem2pair
        integer, dimension(:),allocatable   :: pair2node1 
        integer, dimension(:),allocatable   :: pair2node2
        ! Static Node Coordinates
        real,    dimension(:,:),allocatable :: x_node
        ! Activation of static objects
        ! Pointer to position in dyn lis if deactivated
        integer, dimension(:),allocatable :: elem_activate
        integer, dimension(:),allocatable :: pair_activate
        integer, dimension(:),allocatable :: node_activate
        ! Lists of dynamical objects
        type(dyn_node_t), pointer :: first_dyn_node
        type(dyn_pair_t), pointer :: first_dyn_pair
        type(dyn_elem_t), pointer :: first_dyn_elem
end type dynamic_grid_t
!===============================

!===============================
! SUBROUTINES
!===============================
contains
!===============================
! LIST MANIPULATION ROUTINES
!===============================
! New insert in list of dynamic nodes
subroutine insert_in_node_list(list_ptr, new_node)
        ! INPUT
        type(dyn_node_t), pointer, intent(in) :: list_ptr
        type(dyn_node_t), pointer, intent(in) :: new_node
        do while (associated(list_ptr%next))
                list_ptr = list_ptr%next
        end do
        list_ptr%next = new_node
end subroutine insert_in_node_list 
!===============================
!===============================
! New insert in list of dynamic pairs
subroutine insert_in_pair_list(list_ptr, new_pair)
        ! INPUT
        type(dyn_pair_t), pointer, intent(in) :: list_ptr
        type(dyn_pair_t), pointer, intent(in) :: new_pair
        ! LOCAL VARS
        integer :: k
        !=======================
        k = 1
        do while (associated(list_ptr%next))
                list_ptr = list_ptr%next
                k = k + 1
        end do
        list_ptr%next = new_pair
        new_pair%pos_in_list = k 
end subroutine insert_in_pair_list 
!===============================
!===============================
! New insert in list of dynamic elements
subroutine insert_in_elem_list(list_ptr, new_elem)
        ! INPUT
        type(dyn_elem_t), pointer, intent(in) :: list_ptr
        type(dyn_elem_t), pointer, intent(in) :: new_elem
        ! LOCAL VARS
        integer :: k
        !=======================
        k = 1
        do while (associated(list_ptr%next))
                list_ptr = list_ptr%next
                k = k +1
        end do
        list_ptr%next = new_elem
        new_elem%pos_in_list = k + 1
end subroutine insert_in_elem_list 
!===============================

!===============================
function go_to_index_pair (dyn_pair_head, i) result (pointer_to_i)
        ! INPUT
        implicit none
        integer, intent(in) :: i
        type(dyn_pair_t), pointer, intent(in) :: dyn_pair_head
        ! LOCAL VARS
        integer :: k
        type(dyn_pair_t), pointer :: pointer_to_i
        !=======================
        k = 1
        allocate(pointer_to_i)
        pointer_to_i = dyn_pair_head
        do while (k<i)
                pointer_to_i = pointer_to_i%next
        end do
end function go_to_index_pair  
!===============================
!===============================
function go_to_index_elem (dyn_elem_head, i) result (pointer_to_i)
        ! INPUT
        implicit none
        integer, intent(in) :: i
        type(dyn_elem_t), pointer, intent(in) :: dyn_elem_head
        ! LOCAL VARS
        integer :: k
        type(dyn_elem_t), pointer :: pointer_to_i 
        !=======================
        k = 1
        pointer_to_i = dyn_elem_head
        do while (k<i)
                pointer_to_i = pointer_to_i%next
        end do
end function go_to_index_elem  
!===============================



! ==============================
! Create a dynamic version of an element and its subordinate objects
! Deactivate the static counterpart 
subroutine make_dynamic (stat_grid_ptr, dyn_grid_ptr, elem_idx)
        implicit none
        !INPUT
        type(static_grid_t), pointer, intent(in) :: stat_grid_ptr
        type(dynamic_grid_t), pointer, intent(in) :: dyn_grid_ptr
        integer, intent(in) :: elem_idx
        !=======================
        !LOCAL VARS
        integer ::  j, k
        integer :: node_idx, pair_idx
        type(dyn_node_ptr_t), dimension(3) :: new_node
        type(dyn_pair_ptr_t), dimension(3) :: new_pair
        type(dyn_elem_t), allocatable :: new_elem
        !=======================
        ! Deactivate every static object concerned
        ! Allocate dynamical counterparts
        ! Put them in lists
       allocate(new_elem)
        do j = 1,3
                ! nodes
                allocate (new_node(j)%ptr)
                node_idx = stat_grid_ptr%elem2node(elem_idx,j)
                new_node(j)%ptr%coord(1:2) = stat_grid_ptr%x_node(node_idx, 1:2)
                new_node(j)%ptr%static_idx = node_idx
                nullify(new_node(j)%ptr%next) 
                call insert_in_node_list(dyn_grid_ptr%first_dyn_node, new_node(j)%ptr)
                ! pairs (let alone association with nodes)
                pair_idx = stat_grid_ptr%elem2pair(elem_idx, j)
                allocate(new_pair(j)%ptr)
                new_pair(j)%ptr%activate = .true.
                new_pair(j)%ptr%static_idx = pair_idx
                new_pair(j)%ptr%barycenter(1:2) = 0.5*(stat_grid_ptr%x_node(stat_grid_ptr%pair2node1(pair_idx), 1:2) +&   
                                                       stat_grid_ptr%x_node(stat_grid_ptr%pair2node2(pair_idx), 1:2))
                nullify(new_pair(j)%ptr%child1)
                nullify(new_pair(j)%ptr%child2)
                nullify (new_pair(j)%ptr%next)
                call insert_in_pair_list(dyn_grid_ptr%first_dyn_pair,  new_pair(j)%ptr)
                ! Link static pair with dynamic
                dyn_grid_ptr%pair_activate(pair_idx) = new_pair(j)%ptr%pos_in_list
                new_elem%pairs(j)%ptr = new_pair(j)%ptr
                new_elem%vertices(j)%ptr = new_node(j)%ptr
        end do
        ! Associate node and pairs
        do j = 1,3
                new_pair(j)%ptr%nodes(1)%ptr = new_node(j)%ptr
                new_pair(j)%ptr%nodes(2)%ptr = new_node(modulo(j,3)+1)%ptr
        end do 
        ! Complete dynamic element
        new_elem%activate = .true.
        new_elem%static_idx = elem_idx
        new_elem%barycenter(1:2) = 1/3*(stat_grid_ptr%x_node(stat_grid_ptr%elem2node(elem_idx,1), 1:2)& 
                                      + stat_grid_ptr%x_node(stat_grid_ptr%elem2node(elem_idx,2), 1:2)&
                                      + stat_grid_ptr%x_node(stat_grid_ptr%elem2node(elem_idx,3), 1:2))
        nullify(new_elem%child1)
        nullify(new_elem%child2)
        nullify(new_elem%child3)
        nullify(new_elem%child4)
        nullify(new_elem%next)
        call insert_in_elem_list(dyn_grid_ptr%first_dyn_elem, new_elem)
        ! Link static element dynamic one
        dyn_grid_ptr%elem_activate(elem_idx) = new_elem%pos_in_list
end subroutine make_dynamic 
!===============================

! ==============================
! Refine a Pair
subroutine refine_pair (dyn_grid_ptr, pair_ptr)
        implicit none
        !INPUT
        type(dynamic_grid_t), pointer, intent(in) :: dyn_grid_ptr
        type(dyn_pair_t),pointer,intent(in) :: pair_ptr
        !LOCAL VARS
        integer :: j, k
        type(dyn_node_t), pointer  :: new_node
        type(dyn_pair_ptr_t), dimension(2) :: new_pair
        !=======================
        !Deactivate pair
        pair_ptr%activate = .false.
        dyn_grid_ptr%npair_active =  dyn_grid_ptr%npair_active -1
        ! Define new node
        dyn_grid_ptr%nnode = dyn_grid_ptr%nnode + 1
        allocate (new_node)
        new_node%static_idx = dyn_grid_ptr%nnode
        new_node%coord(1:2) = 0.5*(pair_ptr%nodes(1)%ptr%coord(1:2) + pair_ptr%nodes(2)%ptr%coord(1:2))
        call insert_in_node_list(dyn_grid_ptr%first_dyn_node, new_node)
        nullify(new_node%next)
        ! Define new pairs
        do j = 1,2
                dyn_grid_ptr%npair = dyn_grid_ptr%npair + 1
                allocate(new_pair(j)%ptr)
                new_pair(j)%ptr%activate = .true.
                dyn_grid_ptr%npair_active =  dyn_grid_ptr%npair_active +1
                new_pair(j)%ptr%static_idx = dyn_grid_ptr%npair
                nullify(new_pair(j)%ptr%child1)
                nullify(new_pair(j)%ptr%child2)
                nullify(new_pair(j)%ptr%next)
                call insert_in_pair_list(dyn_grid_ptr%first_dyn_pair, new_pair(j)%ptr)
        end do 
        ! Set new pairs as children of old pair
        pair_ptr%child1 = new_pair(1)%ptr
        pair_ptr%child2 = new_pair(2)%ptr
        ! Associate nodes to new pairs
        new_pair(1)%ptr%nodes(1)%ptr = pair_ptr%nodes(1)%ptr
        new_pair(1)%ptr%nodes(2)%ptr = new_node
        new_pair(2)%ptr%nodes(1)%ptr = new_node
        new_pair(2)%ptr%nodes(2)%ptr = pair_ptr%nodes(2)%ptr
        ! Calculate barycenters
        do j = 1,2
                new_pair(j)%ptr%barycenter(1:2) =&
                        0.5*(new_pair(j)%ptr%nodes(1)%ptr%coord(1:2) + new_pair(j)%ptr%nodes(2)%ptr%coord(1:2))
        end do 
end subroutine refine_pair
!===============================

! ==============================
! Refine an Element 
subroutine refine_elem (dyn_grid_ptr, elem_ptr)
        !INPUT
        type(dynamic_grid_t), pointer,intent(in) :: dyn_grid_ptr
        type(dyn_elem_t), pointer,intent(in)     :: elem_ptr
        !LOCAL VARS
        integer :: i, j, k
        type(dyn_node_ptr_t),dimension(3) :: midpoints
        type(dyn_pair_ptr_t),dimension(3) :: new_pair
        type(dyn_elem_ptr_t), dimension(4) :: new_elem 
        type (dyn_pair_t), pointer :: current_child
        !=======================
        ! Deactivate element
!        elem_ptr%activate = .false.
!        dyn_grid_ptr%nelem_active =  dyn_grid_ptr%nelem_active - 1
!        ! Refine pairs (if needed)
!        do j = 1,3
!                if (elem_ptr%pairs(j)%ptr%activate) then 
!                         call refine_pair(dyn_grid_ptr, elem_ptr%pairs(j)%ptr)
!                end if
!                midpoints(j)%ptr = elem_ptr%pairs(j)%ptr%child1%nodes(2)%ptr
!        end do 
!        ! Allocate new pairs connecting midpoints
!        do j = 1,3
!                dyn_grid_ptr%npair = dyn_grid_ptr%npair + 1 
!                allocate (new_pair(j)%ptr)
!                new_pair(j)%ptr%static_idx = dyn_grid_ptr%npair
!                new_pair(j)%ptr%activate = .true.
!                dyn_grid_ptr%npair_active =  dyn_grid_ptr%npair_active + 1
!                do k = 1,2
!                       if (k == 1) then
!                                current_child = new_pair(j)%ptr%child1
!                       else 
!                                current_child = new_pair(j)%ptr%child2
!                       end if 
!                       nullify (current_child%next)
!                end do
!                nullify(new_pair(j)%ptr%next)
!                call insert_in_pair_list(dyn_grid_ptr%first_dyn_pair ,new_pair(j)%ptr)
!        end do         
!        do k = 1,3 
!                new_pair(k)%ptr%nodes(1)%ptr = midpoints(k)%ptr
!                new_pair(k)%ptr%nodes(2)%ptr = midpoints(modulo(k,3)+1)%ptr
!        end do 
!        ! Allocate new elements        
!        do j = 1,4
!                dyn_grid_ptr%nelem = dyn_grid_ptr%nelem + 1
!                allocate (new_elem(j)%ptr)
!                new_elem(j)%ptr%static_idx = dyn_grid_ptr%nelem
!                new_elem(j)%ptr%activate = .true.   
!                dyn_grid_ptr%nelem_active =  dyn_grid_ptr%nelem_active + 1
!                nullify(new_elem(j)%ptr%child1)
!                nullify(new_elem(j)%ptr%child2)
!                nullify(new_elem(j)%ptr%child3)
!                nullify(new_elem(j)%ptr%child4)
!                nullify(new_elem(j)%ptr%next)
!                elem_ptr%child1 = new_elem(1)%ptr
!                elem_ptr%child2 = new_elem(2)%ptr
!                elem_ptr%child3 = new_elem(3)%ptr
!                elem_ptr%child4 = new_elem(4)%ptr
!                call insert_in_elem_list(dyn_grid_ptr%first_dyn_elem, new_elem(j)%ptr)
!        end do 
!        ! Associate Pairs and Vertices to new elements (3 external children)
!        do i = 1,3
!                j = 1
!                ! Add Vertex in common with parent element
!                new_elem(i)%ptr%vertices(j)%ptr = elem_ptr%vertices(j)%ptr
!                ! Find and add the 2 pairs lying on the border 
!                do k = 1,3
!                ! Use 2nd argument of associated() function
!                if (associated(elem_ptr%pairs(k)%ptr%child1%nodes(1)%ptr,  elem_ptr%vertices(i)%ptr)) then
!                                 new_elem(i)%ptr%pairs(j)%ptr = elem_ptr%pairs(k)%ptr%child1
!                                 j = j + 1  
!                                 new_elem(i)%ptr%vertices(j)%ptr = elem_ptr%pairs(k)%ptr%child1%nodes(2)%ptr
!                        end if   
!                        if (associated(elem_ptr%pairs(k)%ptr%child2%nodes(2)%ptr , elem_ptr%vertices(i)%ptr)) then
!                                 new_elem(i)%ptr%pairs(j)%ptr = elem_ptr%pairs(k)%ptr%child2
!                                 j = j + 1  
!                                 new_elem(i)%ptr%vertices(j)%ptr = elem_ptr%pairs(k)%ptr%child2%nodes(1)%ptr
!                        end if 
!                end do
!                ! Find and add the pair lying in the element
!                do k = 1,3
!                if (associated(new_pair(k)%ptr%nodes(1)%ptr, new_elem(i)%ptr%vertices(2)%ptr) .and.&
!                    associated(new_pair(k)%ptr%nodes(2)%ptr, new_elem(i)%ptr%vertices(3)%ptr)) then
!                                new_elem(i)%ptr%pairs(j)%ptr = new_pair(k)%ptr 
!                        end if
!                        if (associated(new_pair(k)%ptr%nodes(2)%ptr,  new_elem(i)%ptr%vertices(2)%ptr) .and.&
!                            associated(new_pair(k)%ptr%nodes(1)%ptr,  new_elem(i)%ptr%vertices(3)%ptr)) then
!                                new_elem(i)%ptr%pairs(j)%ptr = new_pair(k)%ptr 
!                        end if
!                end do 
!        end do 
!        ! Associate Pairs and Vertices to central element
!        do j = 1,3
!                new_elem(4)%ptr%vertices(j)%ptr = midpoints(j)%ptr
!                new_elem(4)%ptr%pairs(j)%ptr = new_pair(j)%ptr
!        end do 
end subroutine refine_elem
!===============================
! ==============================
! Create a static grid from a dynamic one
subroutine make_static (dyn_grid_ptr, old_stat_grid_ptr, stat_grid_ptr)
!        !INPUT
!        type(dynamic_grid_t), pointer, intent(in) :: dyn_grid_ptr 
!        type(static_grid_t), pointer, intent(in) :: old_stat_grid_ptr 
!        !LOCAL VARS
!        integer :: j, k, i, idx, idx_node
!        integer, dimension(:,:),allocatable  :: new_elem2node
!        integer, dimension(:,:), allocatable :: new_elem2pair
!        integer, dimension(:), allocatable   :: new_pair2node1 
!        integer, dimension(:), allocatable   :: new_pair2node2
!        real,    dimension(:,:), allocatable :: new_x_node
!        type(dyn_node_t), pointer :: dyn_node_ptr
!        type(dyn_elem_t), pointer :: dyn_elem_ptr
!        type(dyn_pair_t), pointer :: dyn_pair_ptr
!        !OUTPUT
!        type(static_grid_t), pointer, intent(inout) :: stat_grid_ptr
!        !=======================
!        !Set Dimension of the grid
!        stat_grid_ptr%nnode = dyn_grid_ptr%nnode
!        stat_grid_ptr%npair = dyn_grid_ptr%npair
!        stat_grid_ptr%nelem = dyn_grid_ptr%nelem
!        !Fill x_node
!        allocate(new_x_node(2, stat_grid_ptr%nnode))
!        j = 1
!        do k = 1, old_stat_grid_ptr%nnode
!                new_x_node (j,1:2) = old_stat_grid_ptr%x_node(j,1:2)  
!                j = j +1
!        end do 
!        dyn_node_ptr = dyn_grid_ptr%first_dyn_node
!        do while (associated(dyn_node_ptr))
!                new_x_node (j,1:2) = dyn_node_ptr%coord(1:2)
!                j = j +1
!        end do 
!        ! Fill pair2node
!        allocate(new_pair2node1(stat_grid_ptr%npair))
!        allocate(new_pair2node2(stat_grid_ptr%npair))
!        j = 1
!        do k = 1, old_stat_grid_ptr%npair
!                if (dyn_grid_ptr%pair_activate(j) == -1) then
!                        new_pair2node1(j) = dyn_grid_ptr%pair2node1(j)
!                        new_pair2node2(j) = dyn_grid_ptr%pair2node2(j)
!                        j = j + 1
!                else    
!                        dyn_pair_ptr%static_idx = dyn_grid_ptr%pair_activate(k)
!                        j = register_pair_of_tree(j, dyn_pair_ptr, new_pair2node1, new_pair2node2)
!                end if
!        end do
!       ! Fill elem2pair and elem2node
!        allocate(new_elem2pair(1,stat_grid_ptr%nelem))
!        j = 1 ! element index
!        do k = 1,old_stat_grid_ptr%nelem
!                ! case element is activated   
!                idx = 1 ! elem2pair index
!                idx_node = 1 ! elem2node index
!                if (dyn_grid_ptr%elem_activate(j) == -1) then
!                       do i =1,3
!                                if (dyn_grid_ptr%pair_activate(old_stat_grid_ptr%elem2pair(k,i)) == -1) then 
!                                         new_elem2pair(j, idx) = old_stat_grid_ptr%elem2pair(k,i)
!                                         idx_node = check_and_insert_nodes_of_stat_pair(new_elem2node, old_stat_grid_ptr,&
!                                                    old_stat_grid_ptr%elem2pair(j,i), j, idx_node)
!                                         idx = idx + 1
!                                else     
!                                         dyn_pair_ptr = go_to_index_pair(dyn_grid_ptr%first_dyn_pair,&
!                                                        dyn_grid_ptr%pair_activate(old_stat_grid_ptr%elem2pair(j,k)))
!                                         idx_node = check_and_insert_nodes_of_dyn_pair(new_elem2node, dyn_pair_ptr, j, idx_node)
!                                         idx = insert_in_elem2pair (new_elem2pair, dyn_pair_ptr , j, idx)
!                                end if  
!                        end do
!                        j = j + 1
!                ! case element is not activated 
!                else    
!                        dyn_elem_ptr =  go_to_index_elem(dyn_grid_ptr%first_dyn_elem, dyn_grid_ptr%elem_activate(j))
!                        j = make_element_insertions(dyn_elem_ptr, j, new_elem2pair, new_elem2node)
!                end if 
!        end do
!
!        ! Assign reconstructed static structures to static grid 
!        stat_grid_ptr%elem2node = new_elem2node
!        stat_grid_ptr%elem2pair = new_elem2pair
!        stat_grid_ptr%pair2node1= new_pair2node1
!        stat_grid_ptr%pair2node2= new_pair2node2
!        stat_grid_ptr%x_node    = new_x_node
!
end subroutine make_static 

!===============================
!===============================
! RECURSIVE FUNCTIONS 

! Navigate a tree of pairs to fill static array pair2node
recursive function register_pair_of_tree(j, dyn_pair_ptr, new_pair2node1, new_pair2node2) result (next_pair)
        !INPUT
        integer, intent(in) :: j ! index of next element to fill in the array
        type(dyn_pair_t), pointer, intent(in) :: dyn_pair_ptr ! Pair to explore
        integer, dimension(:), allocatable :: new_pair2node1, new_pair2node2
        !LOCAL VARS
        integer :: next_pair
        !======================
        next_pair = j
        if (dyn_pair_ptr%activate) then
                new_pair2node1(j) = dyn_pair_ptr%nodes(1)%ptr%static_idx
                new_pair2node2(j) = dyn_pair_ptr%nodes(2)%ptr%static_idx
                ! j becomes the position of the pair in the reconstructed pair2node arrays
                dyn_pair_ptr%static_idx = j
                next_pair = j + 1
        else 
                next_pair = register_pair_of_tree&
                                       (next_pair, dyn_pair_ptr%child1, new_pair2node1, new_pair2node2)
                next_pair = register_pair_of_tree&
                                       (next_pair, dyn_pair_ptr%child2, new_pair2node1, new_pair2node2)
        end if 
end function register_pair_of_tree
!===============================

!===============================
! Navigate a tree of pairs in to fill static array elem2pair
recursive function  insert_in_elem2pair(new_elem2pair, dyn_pair_ptr,  j, idx) result (next_pair)
        !INPUT
        ! element position and current position in elem2array
        integer, intent(in) :: j, idx 
        type(dyn_pair_t), pointer, intent(in) :: dyn_pair_ptr ! Pair to explore
        integer, dimension(:,:), allocatable :: new_elem2pair
        !LOCAL VARS
        integer :: next_pair
        !======================
        next_pair = idx
        if (dyn_pair_ptr%activate) then
                new_elem2pair(j,idx) = dyn_pair_ptr%static_idx
                next_pair = next_pair + 1
        else 
                next_pair = insert_in_elem2pair(new_elem2pair, dyn_pair_ptr%child1, j, next_pair)
                next_pair = insert_in_elem2pair(new_elem2pair, dyn_pair_ptr%child2, j, next_pair)
        end if 
end function insert_in_elem2pair

!===============================
! Check if node is already inserted and if not insert it (reading static data)
integer function check_and_insert_nodes_of_stat_pair(elem2node, stat_grid_ptr,  pair_idx, elem_idx, idx_last_node)
        !=======================
        !INPUT
        integer, dimension(:,:), allocatable, intent(inout) :: elem2node
        type(static_grid_t), pointer, intent(in) :: stat_grid_ptr
        integer, intent(in) :: pair_idx, elem_idx, idx_last_node
        !
        integer :: next_node
        integer :: k
        logical :: ispresent
        !=======================
        next_node = idx_last_node
        ispresent = .false.
        do k = 1,next_node-1
                if (elem2node(elem_idx, k) == stat_grid_ptr%pair2node1(pair_idx)) then
                        ispresent = .true.
                end if    
        end do
        if (.not. ispresent) then
                 elem2node(elem_idx, idx_last_node) = stat_grid_ptr%pair2node1(pair_idx)
                 next_node = next_node + 1 
        end if
        do k = 1, next_node-1
                 if (elem2node(elem_idx, k) == stat_grid_ptr%pair2node2(pair_idx)) then
                        ispresent = .true.
                 end if
        end do  
        if (.not. ispresent) then
                 elem2node(elem_idx, idx_last_node) = stat_grid_ptr%pair2node2(pair_idx)
                 next_node = next_node + 1 
        end if
        check_and_insert_nodes_of_stat_pair = next_node
end function check_and_insert_nodes_of_stat_pair

!===============================
! Check if node is already inserted and if not insert it (navigating  tree)
recursive function check_and_insert_nodes_of_dyn_pair(elem2node, dyn_pair_ptr, elem_idx, idx_last_node)&
        result (next_node)          
        !INPUT
        integer, intent(in) :: idx_last_node, elem_idx
        type(dyn_pair_t), pointer, intent(in) :: dyn_pair_ptr
        integer, dimension(:,:) :: elem2node
        !LOCAL VARS
        integer :: k
        logical :: ispresent
        integer :: next_node
        !======================
        next_node = idx_last_node
        if (dyn_pair_ptr%activate) then    
                  ispresent = .false.
                  do k = 1, next_node-1
                          if (elem2node(elem_idx, k) == dyn_pair_ptr%nodes(1)%ptr%static_idx) then
                                  ispresent = .true.
                          end if    
                  end do
                  if (.not. ispresent) then
                           elem2node(elem_idx, idx_last_node) = dyn_pair_ptr%nodes(1)%ptr%static_idx
                           next_node = next_node + 1 
                  end if
                  do k = 1, next_node-1
                           if (elem2node(elem_idx, k) == dyn_pair_ptr%nodes(2)%ptr%static_idx) then
                                  ispresent = .true.
                           end if
                  end do  
                  if (.not. ispresent) then
                           elem2node(elem_idx, idx_last_node) = dyn_pair_ptr%nodes(2)%ptr%static_idx
                           next_node = next_node + 1 
                  end if
        else 
                  next_node  =&
                         check_and_insert_nodes_of_dyn_pair(elem2node, dyn_pair_ptr%child1, elem_idx, next_node)
                  next_node  =&
                         check_and_insert_nodes_of_dyn_pair(elem2node, dyn_pair_ptr%child2, elem_idx, next_node)
        end if 
end function check_and_insert_nodes_of_dyn_pair 

!=========================================
! Navigate a tree of elements to fill elem2pair and elem2node with the referenced elements
recursive function make_element_insertions(dyn_elem_ptr,  last_elem_idx,  elem2pair, elem2node)&
        result (next_elem)
        !=================================
        !INPUT
        integer :: last_elem_idx
        type(dyn_elem_t), pointer :: dyn_elem_ptr
        integer, dimension(:,:), allocatable :: elem2pair
        integer, dimension(:,:), allocatable :: elem2node
        !LOCAL VARS
        integer :: idx_node, idx_pair, i
        type(dyn_pair_t), pointer :: dyn_pair_ptr
        integer :: next_elem
        !
        next_elem = last_elem_idx
        if (dyn_elem_ptr%activate) then
                idx_node = 1
                idx_pair = 1
                do i =1,3
                        dyn_pair_ptr = dyn_elem_ptr%pairs(i)%ptr
                        idx_node = check_and_insert_nodes_of_dyn_pair(elem2node, dyn_pair_ptr, last_elem_idx, idx_node)
                        idx_pair = insert_in_elem2pair (elem2pair, dyn_pair_ptr , last_elem_idx, idx_pair)
                end do
                next_elem = next_elem + 1
        ! RESTART FROM HERE BY COPYING DYNAMIC CASE ABOVE
        else 
               next_elem = make_element_insertions(dyn_elem_ptr%child1,&
                   next_elem, elem2pair, elem2node)
               next_elem = make_element_insertions(dyn_elem_ptr%child2,&
                   next_elem, elem2pair, elem2node)
               next_elem = make_element_insertions(dyn_elem_ptr%child3,&
                   next_elem, elem2pair, elem2node)
               next_elem = make_element_insertions(dyn_elem_ptr%child4,&
                   next_elem, elem2pair, elem2node)
        end if
end function make_element_insertions


end module defs 












