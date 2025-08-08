# Installation Guide  
  
## Prerequisites  

### Version Requirements

- SAP_ABA 731 or later - Tested on ABAP 7.4
- Can be downported to ABAP 7.02 using [abaplint](https://www.github.com/abaplint/abaplint)
  
### Required Libraries

- **abap2xlsx framework**: Core Excel generation capabilities  
- **abap2xlsx CSV writer**: Required for CSV export functionality (zcl_excel_writer_csv)  
- **SAP RTTS**: Runtime Type Services for dynamic type analysis  

## Installation Steps  

### Step 1: Install the base abap2xlsx framework

### Step 2: Verify Dependencies  
  
Ensure the following abap2xlsx classes are available:  

- `ZCL_EXCEL`  
- `ZCL_EXCEL_WORKSHEET`
- `ZCL_EXCEL_WRITER_2007`  
- `ZCL_EXCEL_COMMON`

### Step 3: Ensure CSV writer components are available  

- `ZCL_EXCEL_WRITER_CSV` class  
- Related CSV export dependencies  

### Step 4: Import the dynamic export add-on framework  

Import the following ABAP artefacts into your system:

#### Classes  

1. `ZCL_EXCEL_DYNAMIC_TABLE` - Main orchestrator class  
2. `ZCL_EXCEL_TABLE_FLATTENER` - Data flattening engine
3. `ZCL_EXCEL_TYPE_ANALYZER` - Type analysis with caching

#### Interfaces

4. `ZIF_EXCEL_DYNAMIC_TABLE` - Main interface  
5. `ZIF_EXCEL_TABLE_FLATTENER` - Flattener interface  
6. `ZIF_EXCEL_TYPE_ANALYZER` - Analyzer interface  
7. `ZCX_EXCEL_DYNAMIC_TABLE` - Exception class

### Verification
  
Test the installation with a simple multi-format export:  
  
```abap  
DATA: lo_exporter TYPE REF TO zcl_excel_dynamic_table,  
      lt_test_data TYPE STANDARD TABLE OF string,  
      ls_options TYPE zif_excel_dynamic_table=>ty_export_options.  
  
APPEND 'Test Data' TO lt_test_data.  
lo_exporter = NEW #( ).  
  
" Test XLSX export  
DATA(lv_xlsx) = lo_exporter->export_to_xlsx( io_data = REF #( lt_test_data ) ).  
  
" Test CSV export    
ls_options-csv_options-delimiter = ','.  
DATA(lv_csv) = lo_exporter->export_to_csv( io_data = REF #( lt_test_data )   
                                           is_options = ls_options ).  
  
" Test XLS export  
DATA(lv_xls) = lo_exporter->export_to_xls( io_data = REF #( lt_test_data ) ).
```

## Troubleshooting

### Common Issues

1. **Missing abap2xlsx dependency**: Install the main abap2xlsx framework first
2. **Authorization issues**: Ensure S_DEVELOP authorization for ABAP development
3. **Version compatibility**: Check SAP NetWeaver version compatibility
