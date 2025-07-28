# Usage

The `ZCL_EXCEL_DYNAMIC_TABLE` class provides a method `EXPORT_TO_XLSX` to flatten hierarchical ABAP data and export it to Excel using abap2xlsx.

## Basic Usage

```abap
DATA: lo_dynamic_table TYPE REF TO zcl_excel_dynamic_table,
      lt_data TYPE TABLE OF ty_sales_data,
      ls_options TYPE zif_excel_dynamic_table=>ty_export_options,
      lv_base64 TYPE string.

TYPES: BEGIN OF ty_sales_data,
         id TYPE string,
         region TYPE string,
         amount TYPE p DECIMALS 2,
         sale_date TYPE d,
       END OF ty_sales_data.

APPEND VALUE #( id = '001' region = 'North' amount = '4999.95' sale_date = '20231201' ) TO lt_data.

ls_options-field_mappings = VALUE #(
  ( abap_field = 'ID' excel_column = 'B' excel_field_name = 'Sales ID' )
  ( abap_field = 'REGION' excel_column = 'A' excel_field_name = 'Area' )
).

CREATE OBJECT lo_dynamic_table.
lv_base64 = lo_dynamic_table->export_to_xlsx( io_data = REF #( lt_data )
                                             is_options = ls_options
                                             iv_title = 'Sales Export' ).
