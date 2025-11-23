
# [ram 0.0.0.9003](https://github.com/PAS-AGRO-PAS/ram/releases/tag/v0.0.0.9003) <small>2025-11-10</small>

## 🛠 Bug Fixes & Stability

- Builder ➜ solver handoff now base64-encodes the payload so query
  parsing never truncates JSON; custom resources/activities reliably
  appear in the new solve tab.
- Solve-mode results table now derives resource usage columns
  dynamically from the activities data (no more hard-coded
  land/labor/nitrogen), so summaries always match user-defined
  resources.
- Builder activity columns populate safely even when the activity table
  is empty, preventing the “replacement has 1 row, data has 0” error
  when adding the first resource.
- Added test coverage for empty activity tables and base64 handoff
  parsing.

------------------------------------------------------------------------

# [ram 0.0.0.9002](https://github.com/PAS-AGRO-PAS/ram/releases/tag/v0.0.0.9002) <small>2025-11-07</small>

## 🔧 New Features & Enhancements

- **Builder ➜ Solver hand-off**: the builder sidebar now exposes an
  “Open In Solver” button that serializes the current tables to JSON,
  opens a solve-mode session, and auto-loads the structures via URL
  query parsing—no manual CSV round-trips.
- **Solver diagnostics & exports**: optimization results show live
  status badges, human-readable hints, and a download button for the raw
  `lpSolve` report so users can audit every run.
- **State-aware uploads/edits**: uploading templates or editing DT cells
  now clears stale solutions and updates a centralized solver status
  object, preventing accidental reuse of outdated answers.
- **Documentation & tests**: README/NEWS highlight the new workflow, and
  `testthat` coverage guards the CSV validator helpers plus builder
  column-alignment utilities.

------------------------------------------------------------------------

# [ram 0.0.0.9001](https://github.com/PAS-AGRO-PAS/ram/releases/tag/v0.0.0.9001) <small>2025-06-30</small>

## 🔧 New Features & Enhancements

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
  - Header now renders the logo via `uiOutput("app_logo")`, ensuring the
    dynamic server widget actually appears and can react to future theme
    toggles.
- **Improved Sidebar Layout**:
  - Sidebar now uses bslib `sidebarLayout()`, applies consistent design
    across themes, and toggles collapse automatically.
- **Inline Editing & Persistence**:
  - Edits made directly in the Resources/Activities DT tables now write
    back to the underlying `reactiveValues` via `DT::editData()`,
    guaranteeing the solver runs with the data users can see.
  - Any table edit or CSV upload clears stale solver results so users
    never see outdated solutions.
- **Validation & Error Surfacing**:
  - Uploaded CSVs are validated for required columns, numeric fields,
    and valid inequality directions; friendly notifications guide users
    when data needs fixing.
  - Solve attempts are wrapped in `tryCatch`, enforce at-least-one
    resource/activity, and surface lpSolve status codes instead of
    silently returning partial outputs.
- **Builder Mode Safeguards**:
  - Activities can only be added after at least one resource exists,
    coefficient inputs default to `0`, and builder tables stay
    column-synced even as resources are added or removed.
  - CSV/XLSX exports now always include the exact resource columns
    currently defined, preventing mismatched templates.
- **Documentation Updates**:
  - `README.Rmd` expanded with inline-edit persistence,
    validation/notification workflow, solver feedback, builder-to-solver
    handoff, and column-syncing notes alongside the modular architecture
    overview.
- **Testing**:
  - Added focused `testthat` coverage for the new validator helpers and
    builder column alignment utilities.

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
