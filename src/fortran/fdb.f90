MODULE fdb
   use, intrinsic :: iso_c_binding
   use eccodes

   interface
      integer(kind=c_int) function fdb_key_add(key, param, value) bind(C, name='fdb_key_add')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char
         type(c_ptr), intent(in), value :: key

         character(kind=c_char, len=1), dimension(*), INTENT(in) :: param
         character(kind=c_char, len=1), dimension(*), INTENT(in) :: value
      end function fdb_key_add
   end interface

   interface
      integer(kind=c_int) function fdb_new_handle(fdb_handle) bind(C, name='fdb_new_handle')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char

         type(c_ptr), intent(inout) :: fdb_handle
      end function fdb_new_handle
   end interface
   interface
      integer(c_int) function fdb_new_key(key) bind(C,name='fdb_new_key')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char

         type(c_ptr), intent(inout) :: key

      end function fdb_new_key
   end interface

   interface
      integer(c_int) function fdb_archive(fdb_handle, key, data, length) bind(C, name='fdb_archive')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char
         type(c_ptr), intent(in), value :: fdb_handle
         type(c_ptr), intent(in), value :: key
         type(c_ptr), intent(in), value :: data
         integer(kind=c_int), intent(in), value :: length
      end function fdb_archive
   end interface

   interface
      integer(c_int) function fdb_initialise() bind(C, name='fdb_initialise')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char
      end function fdb_initialise
   end interface

   contains
   SUBROUTINE get_value_of_key(igrib, keyname, keyname_str, keyname_int)
      character(len=*), INTENT(IN)         :: keyname
      character(len=128), INTENT(INOUT)    :: keyname_str
      integer, INTENT(INOUT), OPTIONAL     :: keyname_int
      call codes_is_missing(igrib, trim(keyname), is_missing);
      if (is_missing /= 1) then
         ! key value is not missing so get values
         if (present(keyname_int)) then
            call codes_get_int(igrib, keyname, keyname_int)
            write (keyname_str, "(I4)") keyname_int
            keyname_str=trim(adjustl(keyname_str))
         else
            call codes_get(igrib, keyname, keyname_str)
         end if
         write (*, *) keyname, '=', keyname_str
      else
         write (*, *) keyname, ' is missing from data.'
      end if
   END SUBROUTINE

   SUBROUTINE copy_s2a(a,s)   ! copy s(1:Clen(s)) to char array
      CHARACTER(*),INTENT(IN) :: s
      CHARACTER :: a(LEN(s))
      INTEGER :: i
      DO i = 1,LEN(s)
         a(i) = s(i:i)
      END DO
      a(LEN(s)+1) = char(0)
   END SUBROUTINE copy_s2a
   
   SUBROUTINE add_key(key, igrib, keyname_str, keyvalue_str, keyvalue_int)  
      use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char
      type(c_ptr), INTENT(INOUT)                          :: key
      character(len=*), INTENT(IN)                        :: keyname_str
      character(len=128), INTENT(INOUT)                   :: keyvalue_str
      integer, INTENT(INOUT), OPTIONAL                    :: keyvalue_int
      ! Local vars
      character(kind=c_char), dimension(128)              :: keyname
      character(kind=c_char), dimension(128)              :: keyvalue

      call get_value_of_key(igrib, trim(keyname_str), keyvalue_str, keyvalue_int) 
      call copy_s2a(keyvalue, trim(keyvalue_str))
      call copy_s2a(keyname, trim(keyname_str))
      res = fdb_key_add(key, keyname, keyvalue)
   END SUBROUTINE add_key

end module fdb
