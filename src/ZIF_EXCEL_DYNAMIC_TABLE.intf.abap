"! Main interface for dynamic table export functionality
"! Follows SOLID principles and provides clean API
INTERFACE zif_excel_dynamic_table
  PUBLIC.

  TYPES: BEGIN OF ty_field_mapping,
           abap_field       TYPE string,                   " ABAP field name
           excel_column     TYPE zexcel_cell_column_alpha, " Excel column (e.g., 'A', 'B')
           excel_field_name TYPE string,                   " Optional: Custom field name for Excel header
         END OF ty_field_mapping,
         ty_field_mappings TYPE TABLE OF ty_field_mapping WITH KEY abap_field.

  TYPES BEGIN OF ty_export_options.
          INCLUDE TYPE zexcel_s_table_settings.
  TYPES   field_mappings TYPE ty_field_mappings. " Custom field mappings
  TYPES END OF ty_export_options.

  METHODS export_to_xlsx
    IMPORTING io_data          TYPE REF TO data
              is_options       TYPE ty_export_options OPTIONAL
              iv_title         TYPE string            DEFAULT 'Title'
    RETURNING VALUE(rv_base64) TYPE string
    RAISING   zcx_excel_dynamic_table.
ENDINTERFACE.