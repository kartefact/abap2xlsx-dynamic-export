"! Main interface for dynamic table export functionality
"! Follows SOLID principles and provides clean API
INTERFACE zif_excel_dynamic_table
  PUBLIC.

  CONSTANTS version TYPE string VALUE '1.000.0'.
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

  "! Method to export data to XLSX format
  "! @parameter io_data                 | Reference to the data to be exported
  "! @parameter is_options              | Export options including field mappings
  "! @parameter iv_title                | Title for the Excel file
  "! @parameter rv_base64               | Base64 encoded string of the XLSX file
  "! @raising   zcx_excel_dynamic_table | Custom exception for export errors
  METHODS export_to_xlsx
    IMPORTING io_data          TYPE REF TO data
              is_options       TYPE ty_export_options OPTIONAL
              iv_title         TYPE string            DEFAULT 'Title'
    RETURNING VALUE(rv_base64) TYPE string
    RAISING   zcx_excel_dynamic_table.
ENDINTERFACE.
