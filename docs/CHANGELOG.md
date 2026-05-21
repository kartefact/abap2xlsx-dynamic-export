# Changelog

## [1.001.0] - 2026-05-21

### Added

- **`ty_csv_options` — three new fields** (sync with upstream `abap2xlsx` `zcl_excel_writer_csv` additions):
  - `skip_hidden_rows` (abap_bool) — skip rows hidden by AutoFilter; maps to `zcl_excel_writer_csv=>set_skip_hidden_rows()`
  - `skip_hidden_cols` (abap_bool) — skip hidden columns; maps to `zcl_excel_writer_csv=>set_skip_hidden_columns()`
  - `initial_ext_date` (char10) — replacement value for empty/initial date cells; maps to `zcl_excel_writer_csv=>set_initial_ext_date()`
- All three fields default to `abap_false` / space — **fully backward compatible**, no changes required in existing code

### Documentation

- API reference updated with new `ty_csv_options` fields, types, defaults, and concurrency warning
- README CSV example extended to show `skip_hidden_rows` and `skip_hidden_cols`
- New API usage patterns for hidden-row filtering and empty-date formatting

### Notes

- `zcl_excel_writer_csv` stores all settings as `CLASS-DATA` (process-global). Concurrent CSV exports in parallel work processes will overwrite each other's settings. See the concurrency note in `docs/api.md` and the inline ABAP doc comments on `ty_csv_options` and `create_csv_file`.

---

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
  
---

## [1.0.0] - 2025-07-29
  
### Added

- Initial release with XLSX export functionality  
- Hierarchical data flattening with space-based indentation  
- Dynamic type analysis using RTTS  
- Custom field mappings support  
- Dependency injection architecture
