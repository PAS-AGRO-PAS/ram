
# [ram 0.0.0.9001](https://github.com/PAS-AGRO-PAS/ram/releases/tag/v0.0.0.9001) <small>2025-06-30</small>

## 🚀 Minor Release

### 🔧 New Features & Enhancements

- **Unified app launcher**: `run_app(mode = "solve")` or
  `run_app(mode = "builder")`, which sets `app_mode` via `{golem}` and
  ensures external resources are initialized correctly.
- **Modular UI/Server**:
  - Sidebar UI & logic extracted into `mod_solve_sidebar_ui()` and
    `mod_solve_sidebar_server()` for cleaner separation and easier
    maintenance.
  - Main panels refactored into `solve_panels()` and `builder_panels()`
    helper functions.
- **Branding & Theming**:
  - Introduced `{bslib}` theming with `_brand.yml` support.
  - Added live light/dark mode toggle via `input_dark_mode()` and
    dynamic switching of logo (dark or light variant).
  - Responsive theme customization now part of `app_ui.R`.
- **Logo Integration**:
  - Header now displays brand logo from `app/www/logo-[dark|light].svg`,
    auto-switching based on theme mode.
- **Improved Sidebar Layout**:
  - Sidebar now uses bslib `sidebarLayout()`, applies consistent design
    across themes, and toggles collapse automatically.
- **Documentation Updates**:
  - `README.Rmd` refreshed to document the modular architecture, unified
    launcher, theming options, and branding support.

------------------------------------------------------------------------

## 🧩 Previous (0.0.0.9000 – 2025-06-26)

- Core functions:
  - `define_resources()`, `define_activities()`, `create_ram_model()`,
    `solve_ram()`, `sensitivity_ram()`, `plot_ram()`
- Mediterranean Mixed Farm vignette.
- Go-modeled Shiny app (“solve” and “builder” scaffolding).

------------------------------------------------------------------------

*For full details and usage examples, see the documentation or the
[GitHub repo](https://github.com/PAS-AGRO-PAS/ram).*
