"! Interface for type analysis operations
INTERFACE zif_excel_type_analyzer
  PUBLIC.

  TYPES:
    BEGIN OF ty_field_info,
      name       TYPE string,
      type       TYPE REF TO cl_abap_datadescr,
      is_key     TYPE abap_bool,
      is_numeric TYPE abap_bool,
    END OF ty_field_info,
    ty_field_catalog TYPE TABLE OF ty_field_info WITH DEFAULT KEY.

  "! Analyzes a given type descriptor and returns a catalog of its fields.
  "! @parameter io_type_descr           | Type descriptor to analyze
  "! @parameter rt_fields               | Table containing field information
  "! @raising   zcx_excel_dynamic_table | Exception if type analysis fails
  METHODS analyze_structure
    IMPORTING io_type_descr    TYPE REF TO cl_abap_typedescr
    RETURNING VALUE(rt_fields) TYPE ty_field_catalog
    RAISING   zcx_excel_dynamic_table.
ENDINTERFACE.
