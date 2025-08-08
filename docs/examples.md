# Usage Examples

## Example 1: Employee Hierarchy Export (Multi-Format)

```abap
REPORT z_employee_hierarchy_export.

TYPES: BEGIN OF ty_employee,
         name TYPE string,
         position TYPE string,
         salary TYPE p LENGTH 10 DECIMALS 2,
         nodes TYPE STANDARD TABLE OF ty_employee,
       END OF ty_employee.

DATA: ls_ceo TYPE ty_employee,
      lo_exporter TYPE REF TO zcl_excel_dynamic_table,
      ls_options TYPE zif_excel_dynamic_table=>ty_export_options,
      lv_excel_data TYPE string,
      lv_csv_data TYPE string.

" Build organization structure
ls_ceo-name = 'John CEO'.
ls_ceo-position = 'Chief Executive Officer'.
ls_ceo-salary = '150000.00'.

" Add managers
APPEND VALUE #( name = 'Jane Manager' position = 'IT Manager' salary = '80000.00' ) TO ls_ceo-nodes.
APPEND VALUE #( name = 'Bob Manager' position = 'Sales Manager' salary = '75000.00' ) TO ls_ceo-nodes.

" Export to XLSX with traditional hierarchy
lo_exporter = NEW #( ).
GET REFERENCE OF ls_ceo INTO DATA(lo_data).

lv_excel_data = lo_exporter->export_to_xlsx(
  io_data = lo_data
  iv_title = 'Organization Chart'
).

" Export to CSV with column-based hierarchy
ls_options-csv_options = VALUE #(
  delimiter = ','
  enclosure = '"'
  indentation = 'C'  " Column-based indentation
).

lv_csv_data = lo_exporter->export_to_csv(
  io_data = lo_data
  iv_title = 'Organization Chart CSV'
  is_options = ls_options
).
```

### Results

#### XLSX: Traditional NODE column with spaces

```
" NODE                    | POSITION               | SALARY
" John CEO               | Chief Executive Officer| 150000.00
"   Jane Manager         | IT Manager             | 80000.00  
"   Bob Manager          | Sales Manager          | 75000.00

" CSV: Separate level columns
" LEVEL_1     | LEVEL_2      | POSITION               | SALARY
" John CEO    |              | Chief Executive Officer| 150000.00
"             | Jane Manager | IT Manager             | 80000.00
"             | Bob Manager  | Sales Manager          | 75000.00
```

## Example 2: Financial Data with Multiple Export Formats

```abap
TYPES: BEGIN OF ty_financial,
         account_code TYPE string,
         account_name TYPE string,
         balance TYPE p LENGTH 15 DECIMALS 2,
         currency TYPE string,
       END OF ty_financial,
       tt_financial TYPE STANDARD TABLE OF ty_financial.

DATA: lt_accounts TYPE tt_financial,
      ls_options TYPE zif_excel_dynamic_table=>ty_export_options,
      lo_exporter TYPE REF TO zcl_excel_dynamic_table.

" Prepare financial data
lt_accounts = VALUE #(
  ( account_code = '1000' account_name = 'Cash' balance = '50000.00' currency = 'USD' )
  ( account_code = '1100' account_name = 'Accounts Receivable' balance = '25000.00' currency = 'USD' )
  ( account_code = '2000' account_name = 'Accounts Payable' balance = '15000.00' currency = 'USD' )
).

lo_exporter = NEW #( ).
GET REFERENCE OF lt_accounts INTO DATA(lo_data).

" Configure custom field mappings
ls_options-field_mappings = VALUE #(
  ( abap_field = 'ACCOUNT_CODE' excel_field_name = 'Account Code' )
  ( abap_field = 'ACCOUNT_NAME' excel_field_name = 'Account Description' )
  ( abap_field = 'BALANCE' excel_field_name = 'Balance (USD)' )
  ( abap_field = 'CURRENCY' excel_field_name = 'Currency' )
).

" Export to XLSX
ls_options-export_format = 'X'.
DATA(lv_xlsx) = lo_exporter->export_data(
  io_data = lo_data
  iv_title = 'Financial Report XLSX'
  is_options = ls_options
).

" Export to XLS for legacy systems
ls_options-export_format = 'L'.
DATA(lv_xls) = lo_exporter->export_data(
  io_data = lo_data
  iv_title = 'Financial Report XLS'
  is_options = ls_options
).

" Export to CSV with semicolon delimiter
ls_options-export_format = 'C'.
ls_options-csv_options = VALUE #(
  delimiter = ';'
  enclosure = '"'
  indentation = 'S'
).
DATA(lv_csv) = lo_exporter->export_data(
  io_data = lo_data
  iv_title = 'Financial Report CSV'
  is_options = ls_options
).
```

## Example 3: Product Catalog with Deep Hierarchy

```abap
TYPES: BEGIN OF ty_product,
         name TYPE string,
         price TYPE p LENGTH 10 DECIMALS 2,
         category TYPE string,
         nodes TYPE STANDARD TABLE OF ty_product,
       END OF ty_product.

DATA: ls_catalog TYPE ty_product,
      lo_exporter TYPE REF TO zcl_excel_dynamic_table,
      ls_options TYPE zif_excel_dynamic_table=>ty_export_options.

" Build product catalog hierarchy
ls_catalog-name = 'Electronics'.
ls_catalog-category = 'Root'.

" Add subcategories
APPEND VALUE #( name = 'Computers' category = 'Electronics' price = '0.00' ) TO ls_catalog-nodes.
APPEND VALUE #( name = 'Mobile Phones' category = 'Electronics' price = '0.00' ) TO ls_catalog-nodes.

" Add products to computers category
DATA(lr_computers) = REF #( ls_catalog-nodes[ 1 ] ).
APPEND VALUE #( name = 'Laptop Dell XPS' category = 'Computers' price = '1299.99' ) TO lr_computers->nodes.
APPEND VALUE #( name = 'Desktop HP Pavilion' category = 'Computers' price = '899.99' ) TO lr_computers->nodes.

lo_exporter = NEW #( ).
GET REFERENCE OF ls_catalog INTO DATA(lo_catalog_data).

" Export with column-based CSV hierarchy for better analysis
ls_options-csv_options = VALUE #(
  delimiter = ','
  enclosure = '"'
  indentation = 'C'  " Column-based for clear hierarchy levels
).

DATA(lv_hierarchical_csv) = lo_exporter->export_to_csv(
  io_data = lo_catalog_data
  iv_title = 'Product Catalog Hierarchy'
  is_options = ls_options
).
```

### Result shows clear hierarchy in separate columns

```
" LEVEL_1     | LEVEL_2      | LEVEL_3           | PRICE   | CATEGORY
" Electronics |              |                   | 0.00    | Root
"             | Computers    |                   | 0.00    | Electronics
"             |              | Laptop Dell XPS   | 1299.99 | Computers
"             |              | Desktop HP        | 899.99  | Computers
"             | Mobile Phones|                   | 0.00    | Electronics
```

## Example 4: Comparing Indentation Styles

### Same data exported with different indentation styles

```abap
DATA: ls_org_data TYPE ty_employee,
      lo_exporter TYPE REF TO zcl_excel_dynamic_table,
      ls_space_options TYPE zif_excel_dynamic_table=>ty_export_options,
      ls_column_options TYPE zif_excel_dynamic_table=>ty_export_options.

" Build sample organization data
ls_org_data-name = 'CEO'.
ls_org_data-position = 'Chief Executive'.
ls_org_data-salary = '200000.00'.

APPEND VALUE #( name = 'CTO' position = 'Chief Technology Officer' salary = '150000.00' ) TO ls_org_data-nodes.
APPEND VALUE #( name = 'CFO' position = 'Chief Financial Officer' salary = '140000.00' ) TO ls_org_data-nodes.

lo_exporter = NEW #( ).
GET REFERENCE OF ls_org_data INTO DATA(lo_org_ref).

" Space-based indentation (traditional)
ls_space_options-csv_options = VALUE #(
  delimiter = ','
  enclosure = '"'
  indentation = 'S'  " Space-based
).

DATA(lv_space_csv) = lo_exporter->export_to_csv(
  io_data = lo_org_ref
  iv_title = 'Organization Space Format'
  is_options = ls_space_options
).

" Column-based indentation (new feature)
ls_column_options-csv_options = VALUE #(
  delimiter = ','
  enclosure = '"'
  indentation = 'C'  " Column-based
).

DATA(lv_column_csv) = lo_exporter->export_to_csv(
  io_data = lo_org_ref
  iv_title = 'Organization Column Format'
  is_options = ls_column_options
).
```

### Space-based result:
```
" NODE        | POSITION                  | SALARY
" CEO         | Chief Executive           | 200000.00
"   CTO       | Chief Technology Officer  | 150000.00
"   CFO       | Chief Financial Officer   | 140000.00
```

### Column-based result:

```
" LEVEL_1 | LEVEL_2 | POSITION                  | SALARY
" CEO     |         | Chief Executive           | 200000.00
"         | CTO     | Chief Technology Officer  | 150000.00
"         | CFO     | Chief Financial Officer   | 140000.00
```

## Example 5: Error Handling and Validation

```abap
DATA: lo_exporter TYPE REF TO zcl_excel_dynamic_table,
      ls_options TYPE zif_excel_dynamic_table=>ty_export_options,
      lv_result TYPE string.

lo_exporter = NEW #( ).

TRY.
    " Test with invalid export format
    ls_options-export_format = 'Z'.  " Invalid format
    
    lv_result = lo_exporter->export_data(
      io_data = lo_data_ref
      iv_title = 'Test Export'
      is_options = ls_options
    ).
    
  CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
    " Handle export errors gracefully
    MESSAGE |Export failed: { lx_error->get_text( ) }| TYPE 'E'.
    
    " Log error details
    WRITE: / 'Error Code:', lx_error->get_error_code( ),
           / 'Error Message:', lx_error->get_text( ),
           / 'Timestamp:', sy-datum, sy-uzeit.
ENDTRY.

" Test with empty data
DATA: lt_empty_data TYPE STANDARD TABLE OF ty_financial.
GET REFERENCE OF lt_empty_data INTO DATA(lo_empty_ref).

TRY.
    lv_result = lo_exporter->export_to_xlsx(
      io_data = lo_empty_ref
      iv_title = 'Empty Data Test'
    ).
    " Should handle empty data gracefully
    
  CATCH zcx_excel_dynamic_table INTO lx_error.
    MESSAGE |Empty data export failed: { lx_error->get_text( ) }| TYPE 'I'.
ENDTRY.
```

## Performance Considerations

### For large datasets, consider memory management

```abap
DATA: lo_exporter TYPE REF TO zcl_excel_dynamic_table,
      lt_large_data TYPE STANDARD TABLE OF ty_financial,
      ls_options TYPE zif_excel_dynamic_table=>ty_export_options.

" Configure for large data export
ls_options-csv_options = VALUE #(
  delimiter = ','
  enclosure = '"'
  indentation = 'S'  " Space-based is more memory efficient for large datasets
).

" Monitor memory usage for large exports
GET REFERENCE OF lt_large_data INTO DATA(lo_large_ref).

TRY.
    DATA(lv_start_time) = sy-uzeit.
    
    DATA(lv_large_csv) = lo_exporter->export_to_csv(
      io_data = lo_large_ref
      iv_title = 'Large Dataset Export'
      is_options = ls_options
    ).
    
    DATA(lv_end_time) = sy-uzeit.
    DATA(lv_duration) = lv_end_time - lv_start_time.
    
    MESSAGE |Export completed in { lv_duration } seconds| TYPE 'S'.
    
  CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
    MESSAGE |Large export failed: { lx_error->get_text( ) }| TYPE 'E'.
ENDTRY.
```
