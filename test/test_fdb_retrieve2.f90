program test_fdb_retrieve
   use fdb
   use eccodes

   integer(c_int)                            :: res
   type(c_ptr)                               :: fdb_handle
   type(c_ptr)                               :: req
   type(c_ptr)                               :: dr
   integer(c_long)                           :: size, read, messageLength, marker

   integer                                   :: i, ifile, igrib, iret
   character(len=10)                         :: open_mode = 'r'

   character(kind=c_char, len=1), dimension(:), allocatable  :: buf
   character(kind=c_char, len=1), dimension(:), allocatable  :: message

   CHARACTER(len=32)                         :: date_values(1)
   CHARACTER(len=32)                         :: parameterNumber_values(2)
   CHARACTER(len=32)                         :: single_value(1)

   integer                                   :: msgid, status
   character(len=128)                        :: keyvalue_str

   integer                                   :: numberOfValues
   real, dimension(:), target, allocatable   :: values


   integer :: idx_a, idx_b, next_message_idx, next_grib
   ! grib api error messages
   character(len=24) :: gribErrorMsg = 'Error reading grib file'
   character(len=20) :: gribFunction = 'test'

   res = fdb_initialise()
   res = fdb_new_handle(fdb_handle)
   res = fdb_new_request(req)

   call fdb_request_add_fortran(req, "validityDate", ["20220703"])
   call fdb_request_add_fortran(req, "validityTime", ["0"])
   call fdb_request_add_fortran(req, "productionStatusOfProcessedData", ["255"])
   call fdb_request_add_fortran(req, "productDefinitionTemplateNumber", ["0"])
   parameterNumber_values(1)='0'
   parameterNumber_values(2)='254'
   call fdb_request_add_fortran(req, "parameterNumber", parameterNumber_values)
   call fdb_request_add_fortran(req, "parameterCategory", ['0','1','3'])
   call fdb_request_add_fortran(req, "discipline", ['0','2'])
   call fdb_request_add_fortran(req, "generatingProcessIdentifier", ["153"])
   call fdb_request_add_fortran(req, "typeOfFirstFixedSurface", ["G"])
   call fdb_request_add_fortran(req, "level", ["0"])

   res = fdb_new_datareader(dr);
   res = fdb_retrieve(fdb_handle, req, dr)
   res = fdb_datareader_open(dr, size)
   write (*, *) 'size of data =', size

   allocate(buf(size))

   res = fdb_datareader_tell(dr, read);
   write(*,*) 'read= ', read
   do while(read .LT. size)

      ! FIND LENGHT OF MESSAGE
      marker = 1000
      res = fdb_datareader_read(dr, buf, marker, read)
      call codes_new_from_message(msgid, buf, status)
      call grib_get(msgid,'totalLength', messageLength, iret)
      call grib_check(iret,gribFunction,gribErrorMsg)
      write (*, *) 'LENGTH OF MESSAGE (GRIB): ', messageLength
      ! GO BACK TO START OF MESSAGE
      res = fdb_datareader_skip(dr, -marker)
   
      ! READ WHOLE MESSAGE
      res = fdb_datareader_read(dr, buf, messageLength, read)
   
      call codes_new_from_message(msgid, buf, status)
      write (*, *) 'status=', status, ', msgid=', msgid
   
      call codes_get(msgid, "parameterNumber", keyvalue_str)
      write (*, *) 'parameterNumber=', keyvalue_str
   
      call codes_get(msgid, "parameterCategory", keyvalue_str)
      write (*, *) 'parameterCategory=', keyvalue_str
   
      call codes_get(msgid, "discipline", keyvalue_str)
      write (*, *) 'discipline=', keyvalue_str
   
      res = fdb_datareader_tell(dr, read)
      write(*,*) 'read= ', read
      write (*, *) '******** FINISHED WITH MESSAGE *****************'
   end do

   deallocate(buf)

   res = fdb_delete_datareader(dr);
   write(*,*) 'end of test_fdb_retrieve'
end program
