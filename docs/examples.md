# Examples

## Hierarchical Data

```abap
TYPES: BEGIN OF ty_item,
         item_id TYPE string,
         description TYPE string,
       END OF ty_item,
       BEGIN OF ty_order,
         order_id TYPE string,
         customer TYPE string,
         items TYPE TABLE OF ty_item,
       END OF ty_order.

DATA lt_orders TYPE TABLE OF ty_order.
APPEND VALUE #( order_id = '001' customer = 'Cust1'
                items = VALUE #( ( item_id = 'I1' description = 'Item1' ) ) ) TO lt_orders.

ls_options-field_mappings = VALUE #(
  ( abap_field = 'ORDER_ID' excel_column = 'A' excel_field_name = 'Order' )
  ( abap_field = 'CUSTOMER' excel_column = 'B' excel_field_name = 'Client' )
  ( abap_field = 'ITEM_ID' excel_column = 'C' excel_field_name = 'Item' )
).

lv_base64 = lo_dynamic_table->export_to_xlsx( io_data = REF #( lt_orders )
                                             is_options = ls_options ).
