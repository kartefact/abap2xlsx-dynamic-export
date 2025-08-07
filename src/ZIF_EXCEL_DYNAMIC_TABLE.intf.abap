"! Main interface for dynamic table export functionality
"! Follows SOLID principles and provides clean API
INTERFACE zif_excel_dynamic_table
  PUBLIC.

  CONSTANTS version TYPE string VALUE '1.000.0'.

  " Export format types
  TYPES: BEGIN OF ty_export_format,
           xlsx TYPE c LENGTH 1,
           xls  TYPE c LENGTH 1,
           csv  TYPE c LENGTH 1,
         END OF ty_export_format.

  " Indentation types for hierarchical data
  TYPES: BEGIN OF ty_indentation_type,
           spaces  TYPE c LENGTH 1,
           columns TYPE c LENGTH 1,
         END OF ty_indentation_type.

  " CSV-specific options
  TYPES: BEGIN OF ty_csv_options,
           delimiter   TYPE c LENGTH 1,
           enclosure   TYPE c LENGTH 1,
           line_ending TYPE string,
           indentation TYPE c LENGTH 1,
         END OF ty_csv_options.

  " Field mapping structure for custom column names
  TYPES: BEGIN OF ty_field_mapping,
           abap_field       TYPE string,                   " ABAP field name
           excel_column     TYPE zexcel_cell_column_alpha, " Excel column (e.g., 'A', 'B')
           excel_field_name TYPE string,                   " Optional: Custom field name for Excel header
         END OF ty_field_mapping.

  TYPES ty_field_mappings TYPE TABLE OF ty_field_mapping WITH KEY abap_field.

  "! Export options structure
  TYPES BEGIN OF ty_export_options.
          INCLUDE TYPE zexcel_s_table_settings.
  TYPES   field_mappings TYPE ty_field_mappings. " Custom field mappings
  TYPES   export_format  TYPE c LENGTH 1.
  TYPES   csv_options    TYPE ty_csv_options.
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

  "! Method to export data to XLS format
  "! @parameter io_data                 | Reference to the data to be exported
  "! @parameter is_options              | Export options including field mappings
  "! @parameter iv_title                | Title for the Excel file
  "! @parameter rv_base64               | Base64 encoded string of the XLS file
  "! @raising   zcx_excel_dynamic_table | Custom exception for export errors
  METHODS export_to_xls
    IMPORTING io_data          TYPE REF TO data
              is_options       TYPE ty_export_options OPTIONAL
              iv_title         TYPE string            DEFAULT 'Title'
    RETURNING VALUE(rv_base64) TYPE string
    RAISING   zcx_excel_dynamic_table.

  "! Method to export data to CSV format
  "! @parameter io_data                 | Reference to the data to be exported
  "! @parameter is_options              | Export options including CSV settings
  "! @parameter iv_title                | Title for the CSV file (used in filename)
  "! @parameter rv_base64               | Base64 encoded string of the CSV file
  "! @raising   zcx_excel_dynamic_table | Custom exception for export errors
  METHODS export_to_csv
    IMPORTING io_data          TYPE REF TO data
              is_options       TYPE ty_export_options OPTIONAL
              iv_title         TYPE string            DEFAULT 'Title'
    RETURNING VALUE(rv_base64) TYPE string
    RAISING   zcx_excel_dynamic_table.

  "! Generic export method that supports all formats
  "! @parameter io_data                 | Reference to the data to be exported
  "! @parameter is_options              | Export options including format selection
  "! @parameter iv_title                | Title for the file
  "! @parameter rv_base64               | Base64 encoded string of the file
  "! @raising   zcx_excel_dynamic_table | Custom exception for export errors
  METHODS export_data
    IMPORTING io_data          TYPE REF TO data
              is_options       TYPE ty_export_options OPTIONAL
              iv_title         TYPE string            DEFAULT 'Title'
    RETURNING VALUE(rv_base64) TYPE string
    RAISING   zcx_excel_dynamic_table.
ENDINTERFACE.