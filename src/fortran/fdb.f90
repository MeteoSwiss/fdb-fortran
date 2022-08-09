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

   interface
      integer(c_int) function fdb_new_request(req) bind(C,name='fdb_new_request')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char

         type(c_ptr), intent(inout) :: req

      end function fdb_new_request
   end interface

   interface
      integer(kind=c_int) function fdb_request_add(req, param, values, numValues) bind(C, name='fdb_request_add')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char, C_LONG
         type(c_ptr), intent(in), value :: req
         character(kind=c_char, len=1), dimension(*), INTENT(in) :: param
         type(c_ptr), intent(in) :: values(0:*)
         integer(kind=c_int), INTENT(in), value :: numValues
      end function 
   end interface

   interface
      integer(kind=c_int) function fdb_retrieve(fdb_handle, req, dr) bind(C, name='fdb_retrieve')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char
         type(c_ptr), intent(in), value :: fdb_handle
         type(c_ptr), intent(in), value :: req         
         type(c_ptr), intent(in), value :: dr
      end function fdb_retrieve
   end interface

   interface
      integer(c_int) function fdb_new_datareader(dr) bind(C,name='fdb_new_datareader')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char
         type(c_ptr), intent(inout) :: dr
      end function fdb_new_datareader
   end interface

   interface
      integer(c_int) function fdb_datareader_open(dr, size) bind(C,name='fdb_datareader_open')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char, c_long
         type(c_ptr), intent(in), value :: dr
         integer(kind=c_long), intent(inout) :: size
      end function fdb_datareader_open
   end interface

   interface
      integer(c_int) function fdb_datareader_read(dr, buf, count, read) bind(C,name='fdb_datareader_read')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char, c_long
         type(c_ptr), intent(in), value :: dr
         character(kind=c_char, len=1), dimension(*), intent(inout) :: buf
         integer(kind=c_long), intent(in), value  :: count
         integer(kind=c_long), intent(inout)      :: read
      end function fdb_datareader_read
   end interface

   interface
      integer(c_int) function fdb_datareader_tell(dr, pos) bind(C,name='fdb_datareader_tell')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char, c_long
         type(c_ptr), intent(in), value      :: dr
         type(c_ptr), intent(in), value      :: pos
      end function fdb_datareader_tell
   end interface



   CONTAINS
   SUBROUTINE get_value_of_key(igrib, keyname, keyvalue_str, keyvalue_int)
      character(len=*), INTENT(IN)         :: keyname
      character(len=128), INTENT(OUT)    :: keyvalue_str
      integer, INTENT(INOUT), OPTIONAL     :: keyvalue_int
      call codes_is_missing(igrib, trim(keyname), is_missing);
      if (is_missing /= 1) then
         ! key value is not missing so get values
         if (present(keyvalue_int)) then
            call codes_get_int(igrib, keyname, keyvalue_int)
            write (keyvalue_str, "(I4)") keyvalue_int
            keyvalue_str=trim(adjustl(keyvalue_str))
         else
            call codes_get(igrib, keyname, keyvalue_str)
         end if
         write (*, *) keyname, '=', keyvalue_str
      else
         write (*, *) keyname, ' is missing from data.'
      end if
   END SUBROUTINE

   SUBROUTINE copy_s2a(a,s)   ! copy s(1:Clen(s)) to char array
      CHARACTER(*),INTENT(IN) :: s
      CHARACTER :: a(LEN(s)+1)
      INTEGER :: i
      DO i = 1,LEN(s)
         a(i) = s(i:i)
      END DO
      a(LEN(s)+1) = char(0)
   END SUBROUTINE copy_s2a
   
   SUBROUTINE add_key(key, igrib, keyname_str, type)  
      use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char
      type(c_ptr), INTENT(INOUT)                  :: key
      character(len=*), INTENT(IN)                :: keyname_str
      character(len=*), INTENT(IN), OPTIONAL      :: type
      ! Local vars
      character(kind=c_char), dimension(128)      :: keyname
      character(kind=c_char), dimension(128)      :: keyvalue
      character(len=128)                          :: keyvalue_str
      integer                                     :: keyvalue_int

      if (type == 'integer') then
         call get_value_of_key(igrib, trim(keyname_str), keyvalue_str, keyvalue_int) 
      else
         call get_value_of_key(igrib, trim(keyname_str), keyvalue_str) 
      end if
      call copy_s2a(keyvalue, trim(keyvalue_str))
      call copy_s2a(keyname, trim(keyname_str))
      res = fdb_key_add(key, keyname, keyvalue)
   END SUBROUTINE add_key


   SUBROUTINE convertValues(numStrings, values_str_array, values_array, values_ptr)
      use, intrinsic :: iso_c_binding 
      integer(c_int) , INTENT(IN)              :: numStrings
      CHARACTER(len=*), INTENT(IN) :: values_str_array(numStrings)
      CHARACTER(kind=c_char), TARGET , INTENT(INOUT)  :: values_array(32,numStrings)
      TYPE(C_PTR), INTENT(OUT)                 :: values_ptr(numStrings)
      integer                                  :: ns, i
      character(len=32)                        :: value_str
      character(kind=c_char), dimension(32)    :: value
      character(len=5)                         :: ns_str
      DO ns = 1, numStrings
         ! cehck that values_str_array(ns) is not greater in size than 32, or hard coded value.
         call copy_s2a(values_array(:,ns), trim(values_str_array(ns)))
         values_ptr(ns) = C_LOC(values_array(:,ns))
      END DO
            
   END SUBROUTINE


   SUBROUTINE fdb_request_add_fortran(req, keyname_str, values_str_array)
      use, intrinsic :: iso_c_binding 
      type(c_ptr), INTENT(IN)                        :: req
      CHARACTER(len=*), INTENT(IN)                   :: keyname_str
      CHARACTER(len=*), INTENT(IN)                   :: values_str_array(:)
      CHARACTER(kind=c_char), TARGET, ALLOCATABLE    :: values_array(:,:)
      TYPE(C_PTR)                                    :: values_ptr(SIZE(values_str_array))
      integer(c_int)                                 :: numStrings
      character(kind=c_char), dimension(128)         :: keyname

      numStrings=SIZE(values_str_array)
      ALLOCATE(values_array(32,numStrings))
      call convertValues(numStrings, values_str_array, values_array, values_ptr)
      call copy_s2a(keyname, trim(keyname_str))
      res = fdb_request_add(req, keyname, values_ptr, numStrings);
      DEALLOCATE(values_array)
   END SUBROUTINE



end module fdb
