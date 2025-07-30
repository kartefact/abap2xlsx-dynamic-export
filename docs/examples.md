# Usage Examples

## Example 1: Employee Hierarchy Export

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
      lv_excel_data TYPE string.

" Build organization structure
ls_ceo-name = 'John CEO'.
ls_ceo-position = 'Chief Executive Officer'.
ls_ceo-salary = '150000.00'.

" Add managers
APPEND VALUE #( name = 'Jane Manager' position = 'IT Manager' salary = '80000.00' ) TO ls_ceo-nodes.
APPEND VALUE #( name = 'Bob Manager' position = 'Sales Manager' salary = '75000.00' ) TO ls_ceo-nodes.

" Export to Excel
lo_exporter = NEW #( ).
GET REFERENCE OF ls_ceo INTO DATA(lo_data).

lv_excel_data = lo_exporter->export_to_xlsx(
  io_data = lo_data
  iv_title = 'Organization Chart'
).

" The result will be a flattened table with NODE column showing hierarchy
" NODE          | POSITION              | SALARY
" John CEO      | Chief Executive Officer| 150000.00
"   Jane Manager| IT Manager            | 80000.00  
"   Bob Manager | Sales Manager         | 75000.00
```

## Example 2: Financial Data with Custom Formatting

```abap
TYPES: BEGIN OF ty_financial,
         account_code TYPE string,
         account_name TYPE string,
         balance TYPE p LENGTH 15 DECIMALS 2,
         currency TYPE string,
       END OF ty_financial,
       tt_financial TYPE STANDARD TABLE OF ty_financial.

DATA: lt_accounts TYPE tt_financial,
      ls_options TYPE zif_excel_dynamic_table=>ty_export_options.

" Prepare financial data
lt_accounts = VALUE #(
  ( account_code = '1000' account_name = 'Cash' balance = '50000.00' currency = 'USD' )
  ( account_code = '1100' account_name = 'Accounts Receivable' balance = '25000.00' currency = 'USD' )
  ( account_code = '2000' account_name = 'Accounts Payable' balance = '15000.00' currency = 'USD' )
).

" Configure export options
ls_options-field_mappings = VALUE #(
  ( abap_field = 'ACCOUNT_CODE' excel_column = 'Account Code' )
  ( abap_field = 'ACCOUNT_NAME' excel_column = 'Account Description' )
  ( abap_field = 'BALANCE' excel_column = 'Balance (USD)' )
  ( abap_field = 'CURRENCY' excel_column = 'Currency' )
).

" Export with custom column names
GET REFERENCE OF lt_accounts INTO DATA(lo_financial_data).
DATA(lv_excel) = NEW zcl_excel_dynamic_table( )->export_to_xlsx(
  io_data = lo_financial_data
  iv_title = 'Financial Report'
  is_options = ls_options
).
```
