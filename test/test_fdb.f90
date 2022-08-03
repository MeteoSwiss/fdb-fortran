program test_fdb
   use fdb
   use eccodes

   integer(c_int) :: res
   type(c_ptr) :: fdb_handle
   type(c_ptr) :: key
   integer :: i, ifile, igrib, iret
   character(len=10)                  ::  open_mode = 'r'
   character(kind=c_char), dimension(128) :: generatingProcessIdentifier, productionStatusOfProcessedData, dateTime, typeOfLevel, &
   &                    parameterNumber, level, productDefinitionTemplateNumber
   character(len=128) :: generatingProcessIdentifier_str, productionStatusOfProcessedData_str, dateTime_str, typeOfLevel_str, &
   &                    parameterNumber_str, level_str, productDefinitionTemplateNumber_str

   integer:: level_int
   integer :: numberOfValues
   real, dimension(:), target, allocatable ::  values
   type(c_ptr) :: values_ptr
   character(len=128) :: keyname_str
   character(kind=c_char), dimension(128) :: keyname

   res = fdb_initialise()

   res = fdb_new_handle(fdb_handle)

   res = fdb_new_key(key)

   call codes_open_file(ifile, '/home/vcherkas/fdb-poc/build/fdb/fdb-fortran/lfff00000000c', open_mode)

   call codes_grib_new_from_file(ifile, igrib, iret)

   LOOP: DO WHILE (iret /= CODES_END_OF_FILE)

      is_missing = 0;

      call add_key(key, igrib, 'generatingProcessIdentifier', generatingProcessIdentifier_str, keyname, generatingProcessIdentifier)
      call add_key(key, igrib, 'productionStatusOfProcessedData', productionStatusOfProcessedData_str, keyname, &
         &productionStatusOfProcessedData)
      call add_key(key, igrib, 'dateTime', dateTime_str, keyname, dateTime)
      call add_key(key, igrib, 'typeOfLevel', typeOfLevel_str, keyname, typeOfLevel)
      call add_key(key, igrib, 'parameterNumber', parameterNumber_str, keyname, parameterNumber)
      call add_key(key, igrib, 'productDefinitionTemplateNumber', productDefinitionTemplateNumber_str, keyname, &
         &productDefinitionTemplateNumber)
      call add_key(key, igrib, 'level', level_str, keyname, level, level_int)


      ! get the size of the values array
      call codes_get_size(igrib, 'values', numberOfValues)
      write (*, *) 'numberOfValues=', numberOfValues

      allocate (values(numberOfValues), stat=iret)
      ! get data values
      call codes_get(igrib, 'values', values)
      values_ptr = transfer(c_loc(values), values_ptr)

      ret = fdb_archive(fdb_handle, key, values_ptr, numberOfValues);

      call codes_release(igrib)
      call codes_grib_new_from_file(ifile, igrib, iret)
   end do LOOP

   call codes_close_file(ifile)

   write(*,*) 'end'
end program
