# AIMPSY plot output naming proposal

## Decision summary

Use the full model run ID followed by a short analysis/change tag:

```text
<model_run_id>_<change-tag>
```

Example:

```text
2606171718_baseline-comparison
```

This proposal applies to new output only. Existing output directories and files remain unchanged.

## Current naming findings

The current `master/output` roots are:

| Folder | Files | Finding |
|---|---:|---|
| `global_260527` | 194 | global scope + 6-digit date; exact model run time is absent |
| `global_260527_AIMT` | 20 | adds a model tag, still only a 6-digit date |
| `global_260617` | 187 | global scope + 6-digit date; used by current code defaults |

Current code embeds 10-digit input run tokens:

- PSM: `2606171718` at [`master/code/ALLPLOT_ENERGY.R:35`](master/code/ALLPLOT_ENERGY.R#L35) and in trading-script `date` values such as [`plot_trade.R:37`](master/code/plot_trade.R#L37).
- AIM-Technology: `2606111707` at [`ALLPLOT_ENERGY.R:36`](master/code/ALLPLOT_ENERGY.R#L36).

The plot folder `global_260617` therefore truncates the primary PSM run ID from 10 to 6 digits. Current output names are not a reliable unique run key.

`plot_trade_AIMT.R` dynamically uses `global_` plus the first six characters of its `date` value ([`lines 23-30`](master/code/plot_trade_AIMT.R#L23-L30)). Its current configuration would target `global_260611_AIMT`, but the existing tracked folder is `global_260527_AIMT`; code configuration and historical output are not aligned.

## Model run ID format

Observed model tokens are exactly 10 ASCII digits and are consistent with:

```text
YYMMDDHHMM
```

Examples observed in code:

- `2606171718`
- `2606111707`

Folder validation rule:

```regex
^[0-9]{10}$
```

The plot workflow should copy the run ID from a producer path or manifest, never reconstruct it from the current clock and never truncate it. Producer-side confirmation that the token is always `YYMMDDHHMM` remains required.

When multiple model sources contribute to a plot, use the primary PSM run as `model_run_id` and list every supporting run in `input_data` within metadata. For an AIM-Technology-only output, use its AIM-Technology run token as `model_run_id`.

## Change-tag rules

Recommended validation rule:

```regex
^[a-z0-9]+(?:-[a-z0-9]+){1,4}$
```

Rules:

- use lowercase ASCII letters, digits, and hyphens only;
- use 2-5 meaningful words;
- keep the tag at or below 48 characters;
- name the changed feature, corrected component, or verification purpose;
- avoid spaces, Japanese characters, underscores, periods, and Windows-reserved characters;
- avoid ambiguous labels such as `test1`, `new`, `tmp`, `misc`, or `final`;
- do not encode the run ID, date, scenario list, year list, or region list again in the tag.

Good examples:

- `hydro-fix`
- `xoc-generation-check`
- `vre-cost-update`
- `region-map-revision`
- `storage-constraint-test`
- `baseline-comparison`
- `month-quarter-comparison`

## Same run analyzed more than once

1. A distinct analytical purpose receives a distinct tag.
2. Re-running the same purpose must not overwrite an existing directory.
3. If the rerun is intentionally retained, append an explicit revision word within the tag, normally `r2`, `r3`, and so on:

```text
2606171718_baseline-comparison
2606171718_baseline-comparison-r2
```

4. Record why the revision exists and which previous run it supersedes in `notes`.
5. If the first result was erroneous, keep it until deletion or archival is explicitly approved; mark its verification status accordingly.

## Scenarios, years, and regions

Do not concatenate long scenario/year/region lists into the run-root folder. Put them in `run_metadata.yaml` and organize figures beneath `figures/`:

```text
figures/
├─ <scenario>/
│  ├─ plot_all/
│  ├─ plot_power/
│  ├─ plot_trade_nh3/
│  └─ ...
└─ comparisons/
   ├─ scenario/
   ├─ year/
   └─ region/
```

Guidance:

- multiple scenarios: one run root; scenario subdirectories and metadata list;
- multiple years: include a year in the filename only when one artifact represents one year; list the complete selection in metadata;
- multiple regions: use region subdirectories only when artifact count requires them; otherwise use clear region tokens in filenames;
- global/world aggregates: record whether `World` was supplied by the model or summed by the plotting script;
- comparisons: include compared dimension and baseline in the filename or comparison subpath, not the root tag unless comparison is the run purpose.

## Recommended internal structure

```text
master/output/
└─ <model_run_id>_<change-tag>/
   ├─ run_metadata.yaml
   ├─ figures/
   ├─ tables/
   └─ logs/
```

- `figures/`: PNG/PDF/SVG plots.
- `tables/`: CSV/TSV/XLSX or other tabular results.
- `logs/`: console output, warnings, package/runtime summary, and validation results.
- Empty `tables/` or `logs/` directories need not be Git-tracked; their intended paths can still appear in metadata.

## Figure and table filenames

Use existing descriptive plot-family filenames during migration to reduce code churn. New filenames should:

- use ASCII letters, digits, hyphens, and underscores;
- include scenario and year when the artifact is specific to them;
- include the metric/variable or plot family;
- use a stable page suffix such as `_page_01` for pagination;
- avoid the words `new`, `latest`, and `final`;
- not repeat the run ID, which is already in the parent directory.

Example:

```text
figures/NPi500-ZF2050/plot_all/NPi500-ZF2050_regional_summary_page_01.png
```

## Windows path constraints

- Exclude `< > : " / \\ | ? *` from every path component.
- Avoid trailing periods/spaces and reserved device names such as `CON`, `PRN`, `AUX`, `NUL`, `COM1`, and `LPT1`.
- Keep the run-root tag short. Some R packages and external tools still behave poorly with long Windows paths even when long-path support is enabled.
- Aim to keep complete artifact paths below approximately 240 characters for compatibility with older tooling.
- Do not use symlinks as the default transfer mechanism; Windows/network permissions and link targets are harder to audit than an explicit immutable path or approved copy.

## Backward compatibility

Current scripts write directly below paths such as:

```text
master/output/global_260617/<scenario>/<plot-family>/
```

Do not move or rename these folders. During migration:

1. leave old output read-only in place;
2. add a configurable `base_dir` to one entry point at a time;
3. point `base_dir` to `master/output/<run-id>_<tag>/figures` for new work;
4. keep existing scenario and plot-family subpaths beneath `figures` initially;
5. create metadata and logs at the run root;
6. compare a small approved sample against an old output before enabling a bulk run.

The existing misspelling `scenario_anarysis` is part of old code paths. Preserve it for historical output; use `comparisons` or correctly spelled new paths only through an explicit migration.

## Collision and overwrite policy

Before execution:

- validate run ID and tag;
- resolve the absolute target path;
- fail if the run root already exists;
- print planned scenarios, years, regions, scripts, input files, and expected output count;
- require approval for bulk generation.

During execution, write only inside the selected new run root. Never fall back to `global_260617` or another old directory if configuration is missing.

## `run_metadata.yaml` template

```yaml
run_id: "2606171718_baseline-comparison"
model_run_id: "2606171718"
change_tag: "baseline-comparison"
created_at: "2026-07-15T00:00:00+09:00"
purpose: ""

source_repository: "C:/@Research_Workplace/300_AIMPSY-with-AI/AIMPSY_AI"
source_commit: "uncommitted"
plot_repository: "C:/@Research_Workplace/300_AIMPSY-with-AI/AIMPSY_plot"
plot_commit: "uncommitted"

input_data:
  - role: "psm-merged"
    run_id: "2606171718"
    path: ""
    file: "main/merged_output.gdx"
    symbols: ["iamc_gdx"]
  - role: "psm-primary-year"
    run_id: "2606171718"
    path: ""
    file_pattern: "gams_output/gdx_primary/<scenario>/<year>.gdx"
    symbols: ["vff_l", "vfb_l", "vfs_l", "vp_l", "pmax"]
  - role: "aim-technology"
    run_id: ""
    path: ""
    symbols: ["iamc_gdx", "trade_merged"]

plot_scripts:
  - "master/code/ALLPLOT_ENERGY.R"

scenarios: []
years: []
regions: []
units: {}
outputs:
  figures: []
  tables: []
  logs: []

verification_status: "unverified"
notes: ""
```

Allowed commit placeholders are an empty string or `uncommitted`. Prefer `uncommitted` when the repository exists but the relevant state is not committed; use an empty string only when the source commit is genuinely unavailable.

Recommended verification states:

- `unverified`
- `partially_verified`
- `verified`
- `failed`

