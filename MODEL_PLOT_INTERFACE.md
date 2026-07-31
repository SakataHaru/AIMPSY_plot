# AIMPSY model-to-plot interface

## Interface status

Status at 2026-07-15: **documented from code, but not operationally verified**.

The R scripts and auxiliary local map files are syntactically available, but the network paths embedded in current code for PSM run `2606171718`, AIM-Technology run `2606111707`, and the R272 mapping did not exist at audit time. This document describes the implemented contract and the checks required before a new run.

## Source and target repositories

| Role | Repository / location | Notes |
|---|---|---|
| Source model repository | `C:/@Research_Workplace/300_AIMPSY-with-AI/AIMPSY_AI` | Independent Git repository. Confirmed branch `develop`, commit `884e3898bd694f8021c98786a0238c528bc70683` during audit. Do not edit from plot-side tasks. |
| Implemented PSM output source | `\\SY_3600_01\atfs02\Individual\hsakata\AIMPSY\output` | Hard-coded in plot scripts; unavailable during audit. It is not the local `AIMPSY_AI/outputs` directory. |
| Supporting model source | `\\SY_3600_01\atfs02\Individual\hsakata\AIMEnduse\output` | AIM-Technology data; outside the provided local repositories. |
| Target plot repository | `C:/@Research_Workplace/300_AIMPSY-with-AI/AIMPSY_plot` | Independent Git repository; all plot-side edits and diff checks are scoped here. |
| Target R project | `AIMPSY_plot/master` | Main code/data/output working directory, not a Git boundary. |

There is no code-level mapping from the local `AIMPSY_AI` commit to the network output run. That association must come from producer metadata or a handoff.

## Implemented source output layout

The current PSM contract is:

```text
<aimpsy-output-root>/
└─ <scenario-group>/
   └─ <region>/
      └─ <10-digit-run-id>/
         ├─ main/
         │  └─ merged_output.gdx
         └─ gams_output/
            └─ gdx_primary/
               └─ <scenario>/
                  └─ <year>.gdx
```

Current literal dimensions are:

- `scenario-group`: `global_softlink_Sc`
- `region`: `Global`
- model run ID: `2606171718`
- main plotted year: `2050`
- scenarios in the integrated energy entry point: `NPi500`, `NPi500-ZF2050`, `NPi500-LimCCS_bio`

Evidence: [`master/code/ALLPLOT_ENERGY.R:35-46`](master/code/ALLPLOT_ENERGY.R#L35-L46), [`plot_trade.R:34-41`](master/code/plot_trade.R#L34-L41), and [`plot_trade.R:125-127`](master/code/plot_trade.R#L125-L127).

Supporting AIM-Technology data is implemented as:

```text
<aimenduse-output-root>/softlink_Sc/<10-digit-run-id>/main/merged_output.gdx
```

`plot_trade_AIMT.R` additionally expects a trade-merged GDX under one of two paths:

```text
gams_output/ghdx_merged/trade/merged.gdx
gams_output/gdx_merged/trade_merged.gdx
```

The first directory name `ghdx_merged` may be intentional or a historical typo; the fallback is explicit at [`plot_trade_AIMT.R:113-114`](master/code/plot_trade_AIMT.R#L113-L114).

## Plot input location and transfer mode

Current behavior:

- model GDX files are read directly from external absolute paths;
- no model output is copied into `master/data`;
- no symlink, junction, manifest, checksum, or completion marker is used;
- `master/data` contains only auxiliary node/route data;
- the local `AIMPSY_AI/outputs` directory was empty at audit time;
- the plot output folder is separately hard-coded, so the model run path and plot run path can diverge.

There is therefore no explicit transfer step to audit. The interface is a set of path assumptions in R source code.

## File formats

| Format | Direction | Purpose |
|---|---|---|
| GDX | model -> plot | `merged_output.gdx`, scenario/year primary GDX, and AIM-Technology trade GDX |
| CSV | auxiliary -> plot; one legacy script can also write | Node coordinates and R272 node-to-region mapping |
| GeoJSON | auxiliary -> plot | R31 representative port nodes, sea routes, and detailed node coordinates |
| PNG | plot output | All 401 current `master/output` artifacts |
| YAML | proposed plot metadata | Not currently generated; template in `OUTPUT_NAMING_PROPOSAL.md` |

No current script writes result tables or logs to files.

## Run ID format

Observed source-path run IDs are 10 ASCII digits:

```text
2606171718
2606111707
```

The intended interpretation is consistent with `YYMMDDHHMM`. The plotting layer must treat the token as an opaque, producer-assigned identifier and validate only `^[0-9]{10}$` until producer documentation confirms the timestamp semantics.

Current plot folders use only `YYMMDD` (for example `global_260617`). New output must retain the full token:

```text
<model_run_id>_<change-tag>
```

## GDX symbol contract

| Symbol | Expected file | Plot use | Key R references |
|---|---|---|---|
| `iamc_gdx` | `main/merged_output.gdx` | Model summary data for energy, power, demand, emissions, capacity, and map fills. | [`ALLPLOT_ENERGY.R:59-77`](master/code/ALLPLOT_ENERGY.R#L59-L77), [`plot_power.R:83-105`](master/code/plot_power.R#L83-L105) |
| `vff_l` | `<scenario>/<year>.gdx` | Forward directional flow. | [`plot_trade.R:140-144`](master/code/plot_trade.R#L140-L144) |
| `vfb_l` | `<scenario>/<year>.gdx` | Backward directional flow. | [`plot_trade.R:140-144`](master/code/plot_trade.R#L140-L144) |
| `vfs_l` | `<scenario>/<year>.gdx` | Installed/available link or transport capacity for maps and differences. | [`plot_trade_diff.R:230-246`](master/code/plot_trade_diff.R#L230-L246), [`plot_link.R:327-337`](master/code/plot_link.R#L327-L337) |
| `vp_l` | `<scenario>/<year>.gdx` | Activity/use, including biomass power use and CCS storage use. | [`ALLPLOT_ENERGY.R:722-747`](master/code/ALLPLOT_ENERGY.R#L722-L747), [`plot_CCS.R:153-185`](master/code/plot_CCS.R#L153-L185) |
| `pmax` | `<scenario>/<year>.gdx` | CCS storage potential. | [`ALLPLOT_ENERGY.R:837-867`](master/code/ALLPLOT_ENERGY.R#L837-L867), [`plot_CCS.R:121-151`](master/code/plot_CCS.R#L121-L151) |
| `trade_merged` | AIM-Technology trade GDX | AIM-Technology transport/trade values. | [`plot_trade_AIMT.R:214-243`](master/code/plot_trade_AIMT.R#L214-L243) |

### `iamc_gdx` representation

The implemented reader assumes at least these fields:

| GDX field | R field | Handling |
|---|---|---|
| `Sc` | `Scenario` | converted to character |
| `Sr` | `RawRegion`, then `Region` | rows beginning `R31-` are dropped in integrated IAMC readers |
| `Sv` | `Variable` | converted to character |
| `Y5` | `Year` | converted to numeric |
| `iamc_gdx` | `Value` | converted to numeric, then summed by scenario/region/variable/year/model |

See [`ALLPLOT_ENERGY.R:59-77`](master/code/ALLPLOT_ENERGY.R#L59-L77).

The reader does not retain a model-provided unit field. If the producer adds or changes unit representation, the plot interface must be updated explicitly.

## Scenario representation

- Scenario names are strings in `Sc` for merged data.
- Scenario names are directory names for primary year GDX files.
- Scripts use hard-coded vectors such as [`ALLPLOT_ENERGY.R:40`](master/code/ALLPLOT_ENERGY.R#L40), [`plot_power.R:47`](master/code/plot_power.R#L47), and [`plot_trade.R:39`](master/code/plot_trade.R#L39).
- Difference plots use a hard-coded baseline and target scenario vector at [`plot_trade_diff.R:39-40`](master/code/plot_trade_diff.R#L39-L40).

Required compatibility rule: every requested scenario must be present both in merged `Sc` values and in any scenario/year directory needed by direct-symbol plots. Missing scenarios must fail preflight rather than silently producing an incomplete verified run.

## Region representation

Three region layers are used:

1. IAMC `Sr` values, converted to plain region strings. `R31-*` rows are removed from several integrated readers.
2. Primary-GDX node names (`N0`, `N1`, and related dimensions), converted to R31 regions using the external `M_R272.csv` mapping or string-prefix inference.
3. Map geometry, where ISO3 countries are assigned to 31 region codes in R code and then dissolved.

The integrated region list contains `World` plus 31 regional codes at [`ALLPLOT_ENERGY.R:48-55`](master/code/ALLPLOT_ENERGY.R#L48-L55). Some scripts construct `World` by summing regions rather than using a producer-supplied row ([`ALLPLOT_ENERGY.R:86-97`](master/code/ALLPLOT_ENERGY.R#L86-L97)). Metadata must record this distinction.

The R272 map path embedded in code was unavailable during the audit. Prefix-based fallback can mask mapping omissions; a verified run must report unmatched nodes.

## Year and time representation

- Merged IAMC years are read from `Y5` and converted to numeric.
- Primary GDX year is encoded in filename, for example `2050.gdx`.
- Current main year is `2050`.
- Transition plots use `seq(2020, 2050, by = 5)`.
- Flow annualization uses a hard-coded `time_slice_hours <- 6` in integrated/trading code.

Any change in model time slices or year dimension requires explicit notification; otherwise flow units and annualization may be wrong without a structural error.

## Unit representation

Current unit handling is implicit and plot-specific:

- `gwh_to_ej <- 3.6e-6`;
- `twh_to_ej <- 3.6 / 1000`;
- `time_slice_hours <- 6` is used as an annual flow multiplier in trading code;
- map legends and y-axis labels embed units such as GW, EJ/yr, or TWh/yr;
- `iamc_gdx` readers do not preserve a unit column.

The interface cannot guarantee units solely from GDX structure. For every producer run, a manifest must state the unit for each exported variable/symbol and whether values are power, energy per time slice, annual energy, capacity, or cumulative potential.

## Variables used by plotting scripts

The authoritative variable names are code-based rather than manifest-based:

- general IAMC catalog: [`master/code/dataframe.R`](master/code/dataframe.R#L1), containing primary energy, electricity, hydrogen, final-energy sectors, emissions, sequestration, cost/investment, capacity, and technology-share groups;
- dispatch/capacity legacy catalog: [`master/code/define/var.R`](master/code/define/var.R#L1);
- integrated plot-specific specs and carrier patterns: [`ALLPLOT_ENERGY.R`](master/code/ALLPLOT_ENERGY.R#L1);
- power/capacity/curtailment/capacity-factor/transmission/hydrogen specs: [`plot_power.R`](master/code/plot_power.R#L1);
- NH3 and biomass demand/trade variables: [`plot_trade.R:146-160`](master/code/plot_trade.R#L146-L160);
- AC/H2 link and demand variables: [`plot_link.R`](master/code/plot_link.R#L1);
- CCS variables and node patterns: [`plot_CCS.R`](master/code/plot_CCS.R#L1).

Because these lists are large and evolve independently, the producer should export a symbol/variable manifest and the plot preflight should compute the requested-versus-available difference before plotting.

## Auxiliary geographic interface

| Data | Current preferred path | Repository fallback |
|---|---|---|
| Detailed node centroids | User-specific OneDrive `nodes_centroid.geojson` | `master/data/AIM_R31_NH3_trade_nodes.csv` for reduced node coordinates |
| Sea routes | User-specific OneDrive `searoute.geojson` | A tracked `master/data/searoute.geojson` exists but is not wired as default |
| Representative sea nodes | User-specific OneDrive `searoute_r31_representative.geojson` | A tracked copy exists but is not wired as default |
| R272 node mapping | Network `M_R272.csv` | No local fallback |

Node fallback behavior is explicit in [`plot_link.R:293-318`](master/code/plot_link.R#L293-L318). Trading routes have no local fallback in the main orchestrator even though repository copies exist ([`ALLPLOT_TRADING.R:40-44`](master/code/ALLPLOT_TRADING.R#L40-L44)).

## Current copy, link, and conversion procedure

Current procedure: none. Scripts read absolute source paths directly and convert GDX parameters into in-memory tibbles using `gdxrrw::rgdx.param`. Spatial files are read with `sf::st_read`. Outputs are immediately saved as PNG.

Recommended minimal procedure for a new run, without reorganizing old files:

1. Receive a model handoff containing the canonical run path, full run ID, source commit, completion status, scenarios, years, symbols, dimensions, and units.
2. Run a read-only preflight against the exact external files. Do not create output yet.
3. Confirm plot scripts, scenarios, years, regions, units, and expected output count with the user.
4. Select a new non-existing `master/output/<run-id>_<change-tag>` root.
5. Create `run_metadata.yaml` with exact external input paths. A copy is not required when the external path is immutable and durable.
6. Run the smallest approved plot sample, verify it, then request/confirm approval for bulk generation if needed.
7. Write figures, tables, and logs only under the selected new run root.
8. Record warnings, missing variables/nodes, artifact list, and final verification status.

If external output is mutable or scheduled for deletion, use an explicitly approved immutable copy/staging procedure with checksums. Do not silently create symlinks or junctions.

## Manual steps currently required

- locate the current valid PSM and AIM-Technology run directories;
- confirm which run is the primary `model_run_id`;
- edit or override hard-coded path/scenario/year/output settings;
- ensure GDXRRW points to the available GAMS installation;
- verify the R272 map and geographic auxiliary data authority;
- choose a non-existing output directory;
- inspect warnings and representative figures;
- decide whether/because generated output is Git-tracked.

## Compatibility risks

| Risk | Failure mode |
|---|---|
| Input path layout changes | Immediate missing-file failure or selection of a stale run |
| GDX symbol rename/removal | `rgdx.param` failure or missing plot family |
| Dimension/label changes | Incorrect node, scenario, region, or year mapping |
| Unit/time-slice changes | Numerically plausible but incorrect axes and flow magnitudes |
| Scenario renamed only in one file family | Merged plots and primary-GDX plots cover different scenarios |
| R272 mapping changes | Nodes assigned by prefix fallback or left unmatched |
| Geometry authority changes | Different routes/centroids across machines |
| Package/GAMS drift | Read failures or changed rendering |
| Fixed output path | Tracked figures overwritten |
| Global source order | Shared objects from an earlier script alter later behavior |

## Required notifications when model output changes

The model-side handoff must notify plot maintainers of any change to:

- canonical output root or directory hierarchy;
- run ID format or run-completion semantics;
- `merged_output.gdx` or year-GDX filename/location;
- GDX symbol names;
- symbol dimensions, labels, or domain order;
- scenario names or availability;
- year/time-slice representation and weighting;
- region/node identifiers or R272 mapping;
- variable definitions, signs, aggregation rules, and units;
- presence/definition of `World` aggregates;
- backward/forward flow direction semantics;
- zero/missing-value conventions;
- source commit and validation status.

## Required plot-side preflight response

Before accepting a model run, the plot side should report:

```text
- input root and full run ID
- source commit / completion marker
- required files found or missing
- required GDX symbols found or missing
- requested scenarios/years/regions found or missing
- unit manifest present or missing
- unmatched nodes/regions
- plot scripts and expected artifact count
- output root and collision check
- verification status: ready / blocked
```

## Unknown items and handoff request

Still unknown:

- the current canonical network output location;
- why the configured `2606171718` and `2606111707` runs are absent;
- whether `ghdx_merged` is intentional;
- the authoritative unit manifest;
- the producer's run-complete signal;
- whether local tracked GeoJSON files should become canonical;
- whether historical PNGs should remain tracked.

Model-side handoff requested:

```text
1. Publish the current canonical PSM output root and a valid 10-digit run ID.
2. Provide a machine-readable run manifest with source commit, completion state,
   scenarios, years, regions, GDX files, symbols, dimensions, and units.
3. Confirm or replace the R272 node-to-region mapping location.
4. State any changes from the implemented iamc_gdx/vff_l/vfb_l/vfs_l/vp_l/pmax contract.
5. Preserve the current layout during transition or provide a versioned interface change notice.
```

