# Draft AGENTS.md for AIMPSY_plot

Status: proposed repository instructions. This file is a draft and does not become active until reviewed and installed as `AGENTS.md` at the `AIMPSY_plot` repository root.

## Scope and repository boundary

- The Git repository root is `AIMPSY_plot`.
- `master` is the principal R project and working directory, but it is not a separate Git repository.
- Run `git status`, `git diff`, branch checks, and history checks from `AIMPSY_plot` root, or use `git -C <AIMPSY_plot-root> ...`.
- The empty `master/.git/` directory is not a repository boundary and must not be populated.
- `../AIMPSY_AI` is an independent model repository. Do not edit it as part of plot-side work.
- If model changes appear necessary, prepare a handoff that identifies the candidate files/components, reason, expected output/interface change, and required validation.

## Active project and legacy areas

- Treat `master` as the default active plotting project.
- Treat `IntermReport`, `kyoeiken`, `softlink`, and `TR_world` as separate historical or specialized variants unless the user explicitly selects one.
- Do not copy fixes between variants without comparing their inputs, variables, scenarios, and output contracts.

## Required preflight before changing R code

Confirm and record all of the following before editing or executing plot code:

1. exact input file paths and model run ID;
2. whether inputs are PSM, AIM-Technology, or both;
3. GDX symbols and dimensions used;
4. scenario names;
5. target years or time slices;
6. region set and node-to-region mapping;
7. source and displayed units, including conversion factors;
8. intended plot/table family and expected artifact count;
9. a new output directory that does not already exist;
10. whether the task is lightweight or a bulk/long-running generation job.

Do not infer an unknown variable meaning or unit from its name alone. Mark it unknown and request model-side or domain-owner confirmation.

## Input-data safety

- Do not modify `master/data` source/auxiliary data casually.
- Read and validate file schemas before using them.
- `master/data/AIM_R31_NH3_trade_nodes.csv` is a fallback coordinate table.
- The tracked sea-route GeoJSON files are not currently the default paths in the main scripts; confirm the authoritative copy before changing path selection.
- `worldmap_plot.R` writes a node CSV and uses legacy working-directory logic. Do not run it automatically.
- Do not copy, move, link, or convert model outputs without explicit approval and a documented source/destination.

## R execution policy

Read-only checks that are normally safe:

- static `parse()` of R files;
- package/version inspection;
- input path and schema checks;
- GDX symbol/dimension inspection when it does not generate outputs;
- Git diff/status inspection;
- estimating output scope from loops and scenario lists.

Require human approval before:

- running integrated or multi-scenario entry points;
- generating a large number of figures or processing a large GDX run;
- executing any script whose configured output directory already exists;
- rewriting tracked data or existing figures;
- installing or upgrading R packages or GAMS;
- changing `.gitignore` treatment of output;
- deleting, moving, or renaming files/directories;
- committing or pushing.

Remember that most `plot_*.R` files execute work at top level. Sourcing them is equivalent to running them.

## Output policy

- Never overwrite existing output without explicit approval.
- New output root names must use `<model_run_id>_<change-tag>`.
- `model_run_id` must be the full producer run token, currently observed as 10 digits (`YYMMDDHHMM`). Do not truncate it to six digits.
- `change-tag` must use lowercase ASCII letters, digits, and hyphens, normally 2-5 meaningful words.
- Avoid weak names such as `test1`, `new`, or `final`.
- If the same run/purpose is repeated, create a distinct tag such as `baseline-comparison-r2`; do not overwrite the first result.
- Keep scenarios, years, and regions in metadata and figure subpaths; do not create an excessively long run-root name.
- New run output should use, when the selected script can support it without a disruptive rewrite:

```text
master/output/<model_run_id>_<change-tag>/
├─ run_metadata.yaml
├─ figures/
├─ tables/
└─ logs/
```

- Do not move or rename existing output to match this proposal.
- Preserve the existing `scenario_anarysis` spelling when interacting with old code; correct it only through an approved migration.

## Traceability requirements

For every generated figure or table, make it possible to identify:

- source repository/path and source commit, or `uncommitted`/unknown;
- full model run ID and exact input files;
- plot repository commit and whether the working tree was dirty;
- R script(s) and configuration used;
- scenarios, years, regions, variables, and units;
- output files;
- verification status and checks performed.

Create `run_metadata.yaml` before or together with new outputs. Do not label an artifact verified unless its inputs, dimensions/units, and rendered result were actually checked.

## Code-change rules

- Prefer small, reviewable changes that parameterize paths or add preflight validation.
- Preserve current input and output behavior unless the requested change explicitly alters it.
- Do not move or rename `master/code`, `master/data`, or `master/output` in routine work.
- Use paths resolved from the script/project location rather than the caller's working directory.
- Avoid adding new user-specific absolute paths.
- Do not introduce global-environment coupling when a function argument or configuration object is practical.
- Keep GDX symbol names, dimension mappings, unit conversions, and plot labels together or cross-referenced.
- Fail before writing if an input is missing, a required symbol is absent, a unit is unknown, or the output directory already exists.
- Do not silently skip scenarios/variables in a final verified run. Warnings must be captured in logs and summarized.

## Verification and completion

At the end of plot-side work:

1. run static parsing on every changed R file;
2. run only the smallest approved execution needed for confidence;
3. verify output existence, count, dimensions, labels, units, scenarios, years, and regions;
4. inspect representative figures visually when figures were generated;
5. run `git status --short` and `git diff` from `AIMPSY_plot` root;
6. confirm no unintended changes occurred under `master/data`, old `master/output`, or `../AIMPSY_AI`;
7. report inputs, run ID, scripts, outputs, warnings, and verification status.

When passing results to Research Hub or Knowledge & Writing, explicitly label them as one of:

- `verified`: inputs, units/dimensions, and artifacts checked;
- `partially_verified`: checks performed and remaining gaps listed;
- `unverified`: not suitable for factual reuse without further validation.

## Model-side handoff format

If a model change is needed, do not edit `AIMPSY_AI`. Report:

```text
Model-side handoff
- Candidate component/file:
- Current plot-side symptom:
- Required interface/output change:
- Affected GDX file/symbol/dimensions:
- Expected units and scenarios/years/regions:
- Backward-compatibility concern:
- Plot-side validation to run after delivery:
```

