program test_fdb_retrieve
   use fdb
   use eccodes

   integer(c_int)                            :: res
   type(c_ptr)                               :: fdb_handle
   type(c_ptr)                               :: req
   type(c_ptr)                               :: dr
   integer(c_long)                           :: size

   integer                                   :: i, ifile, igrib, iret
   character(len=10)                         :: open_mode = 'r'

   character(kind=c_char, len=1), dimension(10000000)  :: string_buf
   type(c_ptr)                                    :: buf
   integer(c_long)                                :: count
   integer(c_long)                                :: read

   CHARACTER(len=32)                         :: date_values(1)
   CHARACTER(len=32)                         :: parameterNumber_values(3)
   CHARACTER(len=32)                         :: single_value(1)

   integer                                   :: msgid, status
   character(len=128)                        :: keyvalue_str

   integer                                   :: numberOfValues
   real, dimension(:), target, allocatable   :: values

   res = fdb_initialise()
   res = fdb_new_handle(fdb_handle)
   res = fdb_new_request(req)


   date_values(1)="202012040900"
   ! date_values(2)="202012041200"
   call fdb_request_add_fortran(req, "dateTime", date_values)
   call fdb_request_add_fortran(req, "productionStatusOfProcessedData", ["2"])
   call fdb_request_add_fortran(req, "productDefinitionTemplateNumber", ["1"])
   parameterNumber_values(1)='18'
   parameterNumber_values(2)='20'
   parameterNumber_values(3)='30'
   call fdb_request_add_fortran(req, "parameterNumber", parameterNumber_values)
   call fdb_request_add_fortran(req, "generatingProcessIdentifier", ["121"])
   call fdb_request_add_fortran(req, "typeOfLevel", ["surface"])
   call fdb_request_add_fortran(req, "level", ["0"])

   res = fdb_new_datareader(dr);
   res = fdb_retrieve(fdb_handle, req, dr)
   res = fdb_datareader_open(dr, size)
   write (*, *) 'size of data =', size

   count=10000000
   res = fdb_datareader_read(dr, string_buf, count, read)

   if (ALL(string_buf(:4) .eq. ['G','R','I','B'])) then 
      write (*, *) 'PASS, string_buf=', string_buf(1:count)
   ELSE 
      WRITE (*, *) 'FAIL, string_buf=', string_buf(1:count)
   END IF

   res = fdb_datareader_tell(dr, read);
   write (*, *) 'read=', read

   ! count=4
   ! res = fdb_datareader_read_2(dr, buf, count, read)
   ! write (*, *) 'buf=', buf

   call codes_new_from_message(msgid, string_buf, status)
   write (*, *) 'status=', status, ', msgid=', msgid

   call codes_get(msgid, "parameterNumber", keyvalue_str)
   write (*, *) 'parameterNumber=', keyvalue_str

   ! get the size of the values array
   call codes_get_size(msgid, 'values', numberOfValues)
   write (*, *) 'numberOfValues=', numberOfValues

   allocate (values(numberOfValues), stat=status)
   ! get data values
   call codes_get(msgid, 'values', values)
   write (*, *) 'values=', values(0:20)

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call codes_new_from_message(msgid, string_buf, status)
   write (*, *) 'status=', status, ', msgid=', msgid

   call codes_get(msgid, "parameterNumber", keyvalue_str)
   write (*, *) 'parameterNumber=', keyvalue_str


   ! call codes_index_get(indexid, key, values, status) 

   ! get the size of the values array
   call codes_get_size(msgid, 'values', numberOfValues)
   write (*, *) 'numberOfValues=', numberOfValues

   allocate (values(numberOfValues), stat=status)
   ! get data values
   call codes_get(msgid, 'values', values)
   write (*, *) 'values=', values(0:20)

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   res = fdb_datareader_seek(dr, read);
   write (*, *) 'read after seek=', read


   res = fdb_delete_datareader(dr);
   write(*,*) 'end of test_fdb_retrieve'
end program
