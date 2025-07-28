"! Interface for table flattening strategies
INTERFACE zif_excel_table_flattener
  PUBLIC.

  METHODS flatten
    IMPORTING io_data              TYPE REF TO data
              io_type_descr        TYPE REF TO cl_abap_typedescr
              iv_level             TYPE i                                          DEFAULT 0
              is_export_options    TYPE zif_excel_dynamic_table=>ty_export_options OPTIONAL
    RETURNING VALUE(ro_flat_table) TYPE REF TO data
    RAISING   zcx_excel_dynamic_table.
ENDINTERFACE.