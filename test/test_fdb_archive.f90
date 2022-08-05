program test_fdb_archive
   use fdb
   use eccodes

   integer(c_int)                            :: res
   type(c_ptr)                               :: fdb_handle
   type(c_ptr)                               :: key
   integer                                   :: i, ifile, igrib, iret
   character(len=10)                         :: open_mode = 'r'
   integer                                   :: numberOfValues
   real, dimension(:), target, allocatable   :: values
   type(c_ptr)                               :: values_ptr

   res = fdb_initialise()

   res = fdb_new_handle(fdb_handle)

   res = fdb_new_key(key)

   call codes_open_file(ifile, '/home/vcherkas/fdb-poc/build/fdb/fdb-fortran/lfff00000000c', open_mode)

   call codes_grib_new_from_file(ifile, igrib, iret)

   LOOP: DO WHILE (iret /= CODES_END_OF_FILE)

      is_missing = 0;

      call add_key(key, igrib, 'generatingProcessIdentifier')
      call add_key(key, igrib, 'productionStatusOfProcessedData')
      call add_key(key, igrib, 'dateTime')
      call add_key(key, igrib, 'typeOfLevel')
      call add_key(key, igrib, 'parameterNumber')
      call add_key(key, igrib, 'productDefinitionTemplateNumber')
      call add_key(key, igrib, 'level', 'integer')


      ! get the size of the values array
      call codes_get_size(igrib, 'values', numberOfValues)
      write (*, *) 'numberOfValues=', numberOfValues

      allocate (values(numberOfValues), stat=iret)
      ! get data values
      call codes_get(igrib, 'values', values)
      write (*, *) 'values=', values(0:20)
      values_ptr = transfer(c_loc(values), values_ptr)

      ret = fdb_archive(fdb_handle, key, values_ptr, numberOfValues);

      call codes_release(igrib)
      call codes_grib_new_from_file(ifile, igrib, iret)
   end do LOOP

   call codes_close_file(ifile)

   write(*,*) 'end'
end program
