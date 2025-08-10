# Changelog

## [1.1.0] - 2025-08-08
  
### Added

- **Multi-format export support**: Added XLS and CSV export capabilities alongside existing XLSX  
- **CSV column-based indentation**: New hierarchy visualization using separate LEVEL_X columns  
- **Enhanced interface methods**:
  - `export_to_xls()` for legacy Excel format support  
  - `export_to_csv()` for comma-separated values export  
  - `export_data()` for generic format-agnostic export  
- **CSV configuration options**: Configurable delimiter, enclosure, line ending, and indentation style  
- **Format constants**: Added export format codes ('X'=XLSX, 'L'=XLS, 'C'=CSV)  
- **Comprehensive test coverage**: Added test cases for all new export formats and features  
  
### Enhanced

- **ZIF_EXCEL_DYNAMIC_TABLE interface**: Extended with new export methods and CSV options  
- **ZCL_EXCEL_TABLE_FLATTENER**: Enhanced with configurable indentation types and maximum hierarchy detection  
- **ZCL_EXCEL_DYNAMIC_TABLE**: Updated with format-specific export logic and CSV writer integration  
  
### Changed

- **Export options structure**: Extended `ty_export_options` with `export_format` and `csv_options` fields  
- **Field catalog formatting**: Enhanced to handle LEVEL_X columns for CSV column-based indentation  
  
### Backward Compatibility

- All existing XLSX export functionality remains unchanged  
- Existing method signatures preserved with optional parameters  
- Default behavior maintains original XLSX export when no format specified  
  
## [1.0.0] - 2025-07-29
  
### Added

- Initial release with XLSX export functionality  
- Hierarchical data flattening with space-based indentation  
- Dynamic type analysis using RTTS  
- Custom field mappings support  
- Dependency injection architecture
