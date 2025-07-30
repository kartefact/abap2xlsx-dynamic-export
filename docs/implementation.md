# Implementation Guide

## Basic Usage

### Simple Table Export

```abap
DATA: lt_data TYPE STANDARD TABLE OF your_structure,
      lo_exporter TYPE REF TO zcl_excel_dynamic_table,
      lv_base64 TYPE string.

" Prepare your data
lt_data = get_your_data( ).

" Create exporter instance
lo_exporter = NEW #( ).

" Export to Excel
GET REFERENCE OF lt_data INTO DATA(lo_data_ref).
lv_base64 = lo_exporter->export_to_xlsx( 
  io_data = lo_data_ref
  iv_title = 'My Export'
).
```
OR

```abap
TYPES: BEGIN OF ty_sales_data,
         id TYPE string,
         region TYPE string,
         amount TYPE p DECIMALS 2,
         sale_date TYPE d,
       END OF ty_sales_data.

DATA: lo_dynamic_table TYPE REF TO zcl_excel_dynamic_table,
      lt_data TYPE TABLE OF ty_sales_data,
      ls_options TYPE zif_excel_dynamic_table=>ty_export_options,
      lv_base64 TYPE string.

APPEND VALUE #( id = '001' region = 'North' amount = '4999.95' sale_date = '20231201' ) TO lt_data.

ls_options-field_mappings = VALUE #(
  ( abap_field = 'ID' excel_column = 'B' excel_field_name = 'Sales ID' )
  ( abap_field = 'REGION' excel_column = 'A' excel_field_name = 'Area' )
).

CREATE OBJECT lo_dynamic_table.
lv_base64 = lo_dynamic_table->export_to_xlsx( io_data = REF #( lt_data )
                                             is_options = ls_options
                                             iv_title = 'Sales Export' ).
```

### Hierarchical Data Export

For hierarchical structures with `NAME` and `NODES` fields:

```abap
TYPES: BEGIN OF ty_hierarchy,
         name TYPE string,
         value TYPE i,
         nodes TYPE STANDARD TABLE OF ty_hierarchy,
       END OF ty_hierarchy.

DATA: ls_root TYPE ty_hierarchy,
      lo_exporter TYPE REF TO zcl_excel_dynamic_table.

" Build hierarchical data
ls_root-name = 'Root Node'.
ls_root-value = 100.
APPEND VALUE #( name = 'Child 1' value = 50 ) TO ls_root-nodes.
APPEND VALUE #( name = 'Child 2' value = 75 ) TO ls_root-nodes.

" Export with automatic flattening
GET REFERENCE OF ls_root INTO DATA(lo_data_ref).
DATA(lv_excel) = lo_exporter->export_to_xlsx(
  io_data = lo_data_ref
  iv_title = 'Hierarchical Export'
).
```

## Advanced Features

### Custom Field Mappings

```abap
DATA: ls_options TYPE zif_excel_dynamic_table=>ty_export_options.

" Define field mappings
ls_options-field_mappings = VALUE #(
  ( abap_field = 'FIELD1' excel_column = 'Custom Name 1' )
  ( abap_field = 'FIELD2' excel_column = 'Custom Name 2' )
).

" Export with custom mappings
DATA(lv_excel) = lo_exporter->export_to_xlsx(
  io_data = lo_data_ref
  iv_title = 'Custom Export'
  is_options = ls_options
).
```

### Dependency Injection

For testing or custom implementations:

```abap
DATA: lo_custom_analyzer TYPE REF TO zif_excel_type_analyzer,
      lo_custom_flattener TYPE REF TO zif_excel_table_flattener,
      lo_exporter TYPE REF TO zcl_excel_dynamic_table.

" Create custom implementations
lo_custom_analyzer = NEW zcl_excel_type_analyzer( ).
lo_custom_flattener = NEW zcl_excel_table_flattener( lo_custom_analyzer ).

" Inject dependencies
lo_exporter = NEW #( 
  io_flattener = lo_custom_flattener
  io_analyzer = lo_custom_analyzer 
).
```
