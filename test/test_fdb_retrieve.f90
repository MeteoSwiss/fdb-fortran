program test_fdb_retrieve
   use fdb
   use eccodes

   integer(c_int)                            :: res, err
   type(c_ptr)                               :: fdb_handle
   type(c_ptr)                               :: req ! Request 
   type(c_ptr)                               :: dr ! DataReader
   type(c_ptr)                               :: it ! ListIterator
   type(c_ptr)                               :: file ! FILE (eckit)

   integer(c_long)                           :: size, read, messageLength, marker, counter

   integer                                   :: i, ifile, igrib, iret
   character(len=10)                         :: open_mode = 'r'

   character(kind=c_char, len=1), dimension(:), allocatable  :: buf_message,buf_file
   character(kind=c_char, len=1), dimension(:), allocatable  :: message

   CHARACTER(len=32)                         :: date_values(1)
   CHARACTER(len=32)                         :: parameterNumber_values(2)
   CHARACTER(len=32)                         :: single_value(1)

   integer                                   :: msgid, status
   character(len=128)                        :: keyvalue_str

   integer                                   :: numberOfValues
   real, dimension(:), target, allocatable   :: values

   character(kind=c_char, len=1), dimension(100)  :: uri
   character(kind=c_char, len=1), dimension(5)  :: mode
   integer(kind=c_int)                            :: off, len, max_len
   logical(kind=c_bool)                           :: duplicates = .false.
   logical(kind=c_bool)                           :: delete_on_close = .true.
   logical(kind=c_bool)                           :: false = .false.
   logical(kind=c_bool)                           :: true = .true.

   integer :: idx_a, idx_b, next_message_idx, next_grib
   ! grib api error messages
   character(len=24) :: gribErrorMsg = 'Error reading grib file'
   character(len=20) :: gribFunction = 'test'

   res = fdb_initialise()
   res = fdb_new_handle(fdb_handle)

   ! Create request / query for fdb
   res = fdb_new_request(req)
   call fdb_request_add_values(req, "dataDate", ["20220703"])
   call fdb_request_add_values(req, "dataTime", ["0000"])
   call fdb_request_add_values(req, "endStep", ["3"])
   call fdb_request_add_values(req, "productionStatusOfProcessedData", ["255"])
   call fdb_request_add_values(req, "productDefinitionTemplateNumber", ["0"])
   parameterNumber_values(1)='0'
   parameterNumber_values(2)='254'
   call fdb_request_add_values(req, "parameterNumber", parameterNumber_values)
   call fdb_request_add_values(req, "parameterCategory", ['0','1','3'])
   call fdb_request_add_values(req, "discipline", ['0','2'])
   call fdb_request_add_values(req, "generatingProcessIdentifier", ["153"])
   call fdb_request_add_values(req, "typeOfFirstFixedSurface", ["sfc"])
   call fdb_request_add_values(req, "level", ["0"])

   ! Iterate through messages with listiterator, 
   ! to get maximum length of a GRIB message in the returned data.
   res = fdb_list(fdb_handle, req, it, duplicates)

   err = fdb_listiterator_next(it)

   do while(err == 0) 
      max_len=0
      res = fdb_listiterator_attrs(it, uri, off, len);
      write (*, *) 'len =', len, 'off =', off

      err = fdb_listiterator_next(it)

      if (len > max_len) then
         max_len = len
      end if
   end do


   res = fdb_new_datareader(dr);
   res = fdb_retrieve(fdb_handle, req, dr)
   res = fdb_datareader_open(dr, size)
   write (*, *) 'size of total data =', size
   write (*, *) 'max_len =', max_len

   allocate(buf_message(max_len)) ! max_len
   allocate(buf_file(max_len)) ! max_len

   res = fdb_datareader_tell(dr, read);
   write(*,*) 'read= ', read
   counter=read
   ! Iterate through messages and read data via eccodes
   do while(read .LT. size)
      ! ! Find length of current GRIB message
      ! marker = 2000
      ! res = fdb_datareader_read(dr, buf, marker, read)
      ! call codes_new_from_message(msgid, buf, status)
      ! call grib_get(msgid,'totalLength', messageLength, iret)
      ! call grib_check(iret,gribFunction,gribErrorMsg)
      ! write (*, *) 'LENGTH OF MESSAGE (GRIB): ', messageLength
      ! ! Put reader back to the start of the message
      ! res = fdb_datareader_skip(dr, -marker)

      ! call copy_s2a(mode, 'r', res)
      res = dr_openf(dr, delete_on_close, file)
      write (*, *) 'FILE: ', file
   
      res = fdb_datareader_tell(dr, read)
      write(*,*) 'read after dr_openf= ', read


      res = wmo_read_grib_from_file(file, buf_file, messageLength)
      write (*, *) 'LENGTH OF MESSAGE (GRIB): ', messageLength
      write (*, *) 'buf_file: ', buf_file(1:10)
      write (*, *) 'buf_file: ', buf_file(messageLength-10:messageLength)



      ! Read whole GRIB message      
      res = fdb_datareader_tell(dr, read)
      write(*,*) 'read after wmo_read_grib_from_file= ', read
      res = fdb_datareader_seek(dr, counter)
      res = fdb_datareader_tell(dr, read)

      res = fdb_datareader_read(dr, buf_message, messageLength, read)
      write (*, *) 'buf_message: ', buf_message(1:10)
      write (*, *) 'buf_message: ', buf_message(messageLength-10:messageLength)
      counter=counter+messageLength

      call codes_new_from_message(msgid, buf_message, status)
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

      res = fclose(file)

   end do

   deallocate(buf_message)
   deallocate(buf_file)
   res = fdb_delete_datareader(dr);
   write(*,*) 'end of test_fdb_retrieve'
end program
