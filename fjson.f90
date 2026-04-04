! Fortran JSON AST
module fjson
    use fjson_tokenizer
    implicit none

    type, public :: json_node
        integer :: value_int = 0
        real :: value_float = 0.0
        logical :: value_bool = .false.
        character(len=:), pointer :: value_string => null()
        character(len=:), pointer :: node_type => null() ! INT|FLOAT|BOOL|STRING|NULL|OBJECT|ARRAY|ERROR
        character(len=:), pointer :: name => null() ! Object property name
        integer :: child_nodes_count = 0
        type(json_node), dimension(:), allocatable :: child_nodes
        contains
            procedure init_node
            procedure destroy_node
            procedure copy_from
            procedure to_string
            procedure append_child_node
            procedure create_number_node
            procedure create_string_node
            procedure create_bool_node
            procedure create_array_node
            procedure create_object_node
    end type json_node

    contains
        subroutine assign_string(ptr,s)
            character(len=:), pointer :: ptr
            character(len=*) :: s
            if(associated(ptr)) deallocate(ptr)
            allocate(character(len=len(s)) :: ptr)
            ptr = s
        end subroutine assign_string

        function get_node(base, target) result(res)
            character(len=*), intent(in) :: target
            type(json_node) :: base, res
            call res%init_node()
            call assign_string(res%node_type, "ERROR")
            call assign_string(res%value_string, "Node not found")
            call search_ast(res, base, target, "")
        end function

        recursive subroutine search_ast(res, base, target, prefix)
            character(len=*), intent(in) :: target, prefix
            type(json_node) :: base, res
            integer :: i
            if (target == "." .or. target == "" .or. target == prefix) then
                call res%copy_from(base)
            else if (base%child_nodes_count > 0) then
                do i = 1, base%child_nodes_count
                    call search_ast(res, base%child_nodes(i), target, prefix // "." // base%child_nodes(i)%name)
                end do
            end if
        end subroutine search_ast

        function parse_json(s) result(res)
            character(len=*), intent(in) :: s
            type(json_node) :: res
            integer :: pos
            type(token_t) :: tok

            pos = 1
            call parse_value(res, s, pos, "")

            call next_token(s, pos, tok)
            if (tok%kind /= token_type_eof) then
                call assign_string(res%node_type,"ERROR")
                call assign_string(res%value_string,"Trailing characters after valid JSON")
                res%child_nodes_count = 0
                if (allocated(res%child_nodes)) deallocate(res%child_nodes)
            end if
        end function parse_json

        recursive subroutine parse_value(node, s, pos, key)
            character(len=*), intent(in) :: s, key
            integer, intent(inout) :: pos
            type(json_node) :: node
            type(token_t) :: tok

            call next_token(s, pos, tok)

            select case (tok%kind)
                case (token_type_string)
                    call node%create_string_node(tok%text)

                case (token_type_number)
                    call node%create_number_node(tok%text)

                case (token_type_bool)
                    call node%create_bool_node(tok%text)

                case (token_type_null)
                    call node%create_bool_node(tok%text)

                case (token_type_lbracket)
                    call parse_array_after_open(node, s, pos)

                case (token_type_lbrace)
                    call parse_object_after_open(node, s, pos)

                case default
                    call assign_string(node%node_type,"ERROR")
                    call assign_string(node%value_string,tok%text)
                    node%child_nodes_count = 0
            end select
            call assign_string(node%name,key)
        end subroutine parse_value

        recursive subroutine parse_array_after_open(this, s, pos)
            class(json_node), intent(inout) :: this
            character(len=*), intent(in) :: s
            integer, intent(inout) :: pos

            type(token_t) :: tok
            type(json_node) :: child
            integer :: save_pos

            call assign_string(this%node_type,"ARRAY")
            this%child_nodes_count = 0
            if (allocated(this%child_nodes)) deallocate(this%child_nodes)

            ! Check for empty array: []
            save_pos = pos
            call next_token(s, pos, tok)
            if (tok%kind == token_type_rbracket) return

            ! Not empty
            pos = save_pos

            do
                call parse_value(child, s, pos, "")
                call this%append_child_node(child)

                call next_token(s, pos, tok)
                select case (tok%kind)
                case (token_type_comma)
                    cycle
                case (token_type_rbracket)
                    exit
                case default
                    call assign_string(this%node_type,"ERROR")
                    call assign_string(this%value_string,"Expected ',' or ']' in array")
                    if (allocated(this%child_nodes)) deallocate(this%child_nodes)
                    this%child_nodes_count = 0
                    return
                end select
            end do
        end subroutine parse_array_after_open

        recursive subroutine parse_object_after_open(this, s, pos)
            class(json_node), intent(inout) :: this
            character(len=*), intent(in) :: s
            integer, intent(inout) :: pos

            type(token_t) :: tok
            type(json_node) :: child
            character(len=:), allocatable :: key
            integer :: save_pos

            call assign_string(this%node_type,"OBJECT")
            this%child_nodes_count = 0
            if (allocated(this%child_nodes)) deallocate(this%child_nodes)

            ! Check for empty object: {}
            save_pos = pos
            call next_token(s, pos, tok)
            if (tok%kind == token_type_rbrace) return

            ! Not empty
            pos = save_pos

            do
                ! Key must be a string
                call next_token(s, pos, tok)
                if (tok%kind /= token_type_string) then
                    call assign_string(this%node_type,"ERROR")
                    call assign_string(this%value_string,"Expected string key in object")
                    if (allocated(this%child_nodes)) deallocate(this%child_nodes)
                    this%child_nodes_count = 0
                    return
                end if

                key = tok%text(2:len(tok%text)-1)

                ! Must have colon
                call next_token(s, pos, tok)
                if (tok%kind /= token_type_colon) then
                    call assign_string(this%node_type,"ERROR")
                    call assign_string(this%value_string,"Expected ':' after object key")
                    if (allocated(this%child_nodes)) deallocate(this%child_nodes)
                    this%child_nodes_count = 0
                    return
                end if

                call parse_value(child, s, pos, key)
                call this%append_child_node(child)

                call next_token(s, pos, tok)
                select case (tok%kind)
                case (token_type_comma)
                    cycle
                case (token_type_rbrace)
                    exit
                case default
                    call assign_string(this%node_type,"ERROR")
                    call assign_string(this%value_string,"Expected ',' or '}' in object")
                    if (allocated(this%child_nodes)) deallocate(this%child_nodes)
                    this%child_nodes_count = 0
                    return
                end select
            end do
        end subroutine parse_object_after_open

        recursive subroutine destroy_node(this)
            class(json_node), intent(inout) :: this
            integer :: i

            if (allocated(this%child_nodes)) then
                do i = 1, this%child_nodes_count
                    call this%child_nodes(i)%destroy_node()
                end do
                deallocate(this%child_nodes)
            end if

            if (associated(this%name)) then
                deallocate(this%name)
                nullify(this%name)
            end if

            if (associated(this%value_string)) then
                deallocate(this%value_string)
                nullify(this%value_string)
            end if

            if (associated(this%node_type)) then
                deallocate(this%node_type)
                nullify(this%node_type)
            end if

            this%value_int = 0
            this%value_float = 0.0
            this%value_bool = .false.
            this%child_nodes_count = 0
        end subroutine destroy_node

        subroutine init_node(this)
            class(json_node) :: this
            call this%destroy_node()
        end subroutine init_node

        recursive subroutine copy_from(this, src)
            class(json_node), intent(inout) :: this
            type(json_node), intent(in) :: src
            integer :: i, n

            call this%destroy_node()

            this%value_int = src%value_int
            this%value_float = src%value_float
            this%value_bool = src%value_bool
            this%child_nodes_count = src%child_nodes_count

            if (associated(src%name)) call assign_string(this%name, src%name)
            if (associated(src%node_type)) call assign_string(this%node_type, src%node_type)
            if (associated(src%value_string)) call assign_string(this%value_string, src%value_string)

            n = src%child_nodes_count
            if (n > 0) then
                allocate(this%child_nodes(n))
                do i = 1, n
                    call this%child_nodes(i)%copy_from(src%child_nodes(i))
                end do
            end if
        end subroutine copy_from

        subroutine to_string(this, s)
            class(json_node), intent(in) :: this
            character(len=:), allocatable :: s
            call build_json(this, s)
        end subroutine to_string

        recursive subroutine build_json(node, s)
            type(json_node), intent(in) :: node
            character(len=:), allocatable :: s, tmp
            integer :: i, n

            select case (node%node_type)
                case ("OBJECT")
                    if (node%child_nodes_count == 0) then
                        s = s // "{}"
                    else
                        tmp = "{"
                        do i = 1, node%child_nodes_count
                            tmp = tmp // '"' // node%child_nodes(i)%name // '":'
                            call build_json(node%child_nodes(i), tmp)
                            tmp = tmp // ","
                        end do
                        tmp = tmp(1:len(tmp)-1) // "}"
                        s = s // tmp
                    end if
                case ("ARRAY")
                    if (node%child_nodes_count == 0) then
                        s = s // "[]"
                    else
                        tmp = "["
                        do i = 1, node%child_nodes_count
                            call build_json(node%child_nodes(i), tmp)
                            tmp = tmp // ","
                        end do
                        tmp = tmp(1:len(tmp)-1) // "]"
                        s = s // tmp
                    end if
                case ("STRING")
                    s = s // '"' // node%value_string // '"'
                case default
                    s = s // node%value_string
            end select
        end subroutine build_json

        subroutine append_child_node(this, child)
            class(json_node), intent(inout) :: this
            type(json_node), intent(in) :: child
            type(json_node), allocatable :: temp(:)
            integer :: i, old_size, new_size

            if (.not. allocated(this%child_nodes)) then
                allocate(this%child_nodes(256))
            elseif (size(this%child_nodes) == this%child_nodes_count) then
                old_size = size(this%child_nodes)
                new_size = old_size * 2
                allocate(temp(new_size))

                do i = 1, this%child_nodes_count
                    call temp(i)%copy_from(this%child_nodes(i))
                end do

                do i = 1, this%child_nodes_count
                    call this%child_nodes(i)%destroy_node()
                end do
                deallocate(this%child_nodes)

                call move_alloc(temp, this%child_nodes)
            end if

            this%child_nodes_count = this%child_nodes_count + 1
            call this%child_nodes(this%child_nodes_count)%copy_from(child)
        end subroutine append_child_node

        subroutine create_number_node(this,s)
            class(json_node) :: this
            character(len=*) :: s
            integer :: err
            call this%init_node()
            if(index(s,".") /= 0) then
                call assign_string(this%node_type,"FLOAT")
                call assign_string(this%value_string,s)
                read(s,*,iostat=err) this%value_float
                if (err /= 0) call assign_string(this%node_type,"ERROR")
            else
                call assign_string(this%node_type,"INT")
                call assign_string(this%value_string,s)
                read(s,*,iostat=err) this%value_int
                if (err /= 0) call assign_string(this%node_type,"ERROR")
            end if
        end subroutine create_number_node

        subroutine create_string_node(this,s)
            class(json_node) :: this
            character(len=*) :: s
            character :: first, last
            call this%init_node()
            first = s(1:1)
            last = s(len(s):len(s))
            if(first == '"' .and. last == '"') then
                call assign_string(this%node_type,"STRING")
                call assign_string(this%value_string,s(2:len(s)-1))
            else
                call assign_string(this%node_type,"ERROR")
                call assign_string(this%value_string,s)
            end if
        end subroutine create_string_node

        subroutine create_bool_node(this,s)
            class(json_node) :: this
            character(len=*) :: s
            call this%init_node()
            if(s == "true" .or. s == "false") then
                call assign_string(this%node_type,"BOOL")
                this%value_bool = (s == "true")
                call assign_string(this%value_string,s)
            elseif(s == "null") then
                call assign_string(this%node_type,"NULL")
                call assign_string(this%value_string,s)
            else
                call assign_string(this%node_type,"ERROR")
                call assign_string(this%value_string,s)
            end if
        end subroutine create_bool_node

        subroutine create_array_node(this,s)
            class(json_node) :: this
            character(len=*) :: s
            integer :: pos
            type(token_t) :: tok
            call this%init_node()

            pos = 1
            call next_token(s, pos, tok)
            if (tok%kind /= token_type_lbracket) then
                call assign_string(this%node_type,"ERROR")
                call assign_string(this%value_string,"Array must start with '['")
                this%child_nodes_count = 0
                if (allocated(this%child_nodes)) deallocate(this%child_nodes)
                return
            end if

            call parse_array_after_open(this, s, pos)
        end subroutine create_array_node

        subroutine create_object_node(this,s)
            class(json_node) :: this
            character(len=*) :: s
            integer :: pos
            type(token_t) :: tok
            call this%init_node()

            pos = 1
            call next_token(s, pos, tok)
            if (tok%kind /= token_type_lbrace) then
                call assign_string(this%node_type,"ERROR")
                call assign_string(this%value_string,"Object must start with '{'")
                this%child_nodes_count = 0
                if (allocated(this%child_nodes)) deallocate(this%child_nodes)
                return
            end if

            call parse_object_after_open(this, s, pos)
        end subroutine create_object_node
end module fjson