"! Interface for table flattening strategies
INTERFACE zif_excel_table_flattener
  PUBLIC.

  "! Method to flatten a table structure into a flat table
  "! @parameter io_data                 | Reference to the data to be flattened
  "! @parameter io_type_descr           | Reference to the type descriptor of the data
  "! @parameter iv_level                | Recursion level for nested structures, default is 0
  "! @parameter is_export_options       | Optional export options for the flattening process
  "! @parameter ro_flat_table           | Reference to the resulting flat table
  "! @raising   zcx_excel_dynamic_table | Exception raised for errors during flattening
  METHODS flatten
    IMPORTING io_data              TYPE REF TO data
              io_type_descr        TYPE REF TO cl_abap_typedescr
              iv_level             TYPE i                                          DEFAULT 0
              is_export_options    TYPE zif_excel_dynamic_table=>ty_export_options OPTIONAL
    RETURNING VALUE(ro_flat_table) TYPE REF TO data
    RAISING   zcx_excel_dynamic_table.
ENDINTERFACE.
