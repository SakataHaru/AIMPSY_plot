# IntermReport Figure作成ガイド

この文書は、中間報告および論文用のFig.1–Fig.3を実装するときに参照する構造・データ・描画・保存の共通ルールをまとめたものである。現在のエントリーポイントは [`code/main.R`](code/main.R) とする。

## 1. 基本方針

- Figureはpanel単位で実装する。
- データ読込、共通描画設定、panel固有処理、Figure結合を別ファイルに分ける。
- panel関数はplot objectを返し、global environmentへ多数の中間変数を作らない。
- Figure固有の変数名、filter、集計方法、単位変換は、出典を確認できるまで推測しない。
- 未確定事項は `TODO` として残す。
- 既存解析コードと入力データを付随作業として削除、移動、改名しない。`main.R` が管理する同名のFigure出力は、保存設定が有効な再実行時に更新する。

## 2. ディレクトリ構造

```text
IntermReport/
├─ PLOTTING_GUIDE.md
├─ code/
│  ├─ main.R
│  ├─ data_import.R
│  ├─ plot_common.R
│  ├─ dataframe.R
│  ├─ fig1/
│  │  ├─ fig1_a.R
│  │  ├─ fig1_b.R
│  │  ├─ fig1_c.R
│  │  └─ fig1_d.R
│  ├─ fig2/
│  │  ├─ fig2_a.R
│  │  ├─ fig2_b.R
│  │  ├─ fig2_c.R
│  │  └─ fig2_d.R
│  └─ fig3/
│     ├─ fig3_a.R
│     └─ fig3_b.R
├─ data/
└─ output/
   ├─ fig1/
   ├─ fig2/
   └─ fig3/
```

`data/` は重いローカル入力を置く可能性があるため、ディレクトリ全体を `.gitignore` でGit管理外としている。

Fig.2はa–dの4 panelで構成する。b–dは2050年primary GDXを共有し、設備容量、carrier転換、地域間輸送を表示する。

## 3. ファイルごとの責任

### `code/main.R`

`main.R` は次だけを担当する。

1. ユーザーが変更するrun・出力設定を冒頭に置く。
2. AIM-TechnologyとPSMの入力パスをrun構成要素から生成する。
3. 共通コードとpanelファイルをsourceする。
4. `data_import.R` を通してデータを読み込む。
5. 各panel関数を呼び出す。
6. `cowplot::plot_grid()` で最終Figureを組み立て、全体を見ながら相対幅・相対高さ・凡例位置を調整する。
7. panelと最終Figureを、寸法がコード上で読める `ggsave()` 呼び出しで保存する。

冒頭の設定欄では、少なくとも次を管理する。

- `AIMT_SCENARIO`
- `AIMT_DATE`
- `AIMT_OUTPUT_ROOT`
- `PSM_SCENARIO`
- `PSM_REGION`
- `PSM_DATE`
- `PSM_OUTPUT_ROOT`
- `time_resol`
- `PSM_REGION_MAP_FILE`
- `OUTPUT_DIRECTORY`
- `target_scenarios`
- `BUILD_FIGURES`
- `SAVE_INDIVIDUAL_PANELS`
- `SAVE_FINAL_FIGURES`

run tokenは既存のproducer pathからそのまま転記し、現在時刻から生成しない。AIM-Technologyの標準パスは `<root>/<scenario>/<run-date>/main`、PSMは `<root>/<scenario>/<region>/<run-date>/main` とする。

panelがtemplateの間は `BUILD_FIGURES <- FALSE` を維持する。データ読込を有効にするときは、入力runと必要なGDXが確認できてから `TRUE` にする。

### `code/data_import.R`

`data_import.R` は次だけを担当する。

- GDXまたはファイルの読込
- 列名と型の最低限の共通化
- モデルの出所が分かる名前での格納
- 読み込んだpathとsymbolのmetadata保持

現在は、既存コードと同じく両モデルの `main/merged_output.gdx` から `iamc_gdx` を読み、次の列へ整形する。

| 出力列 | GDX dimension/field | 内容 |
|---|---|---|
| `Scenario` | `Sc` | scenario |
| `Region` | `Sr` | region |
| `Variable` | `Sv` | IAMC variable |
| `Year` | `Y5` | year |
| `Value` | `iamc_gdx` | value |
| `Model` | 付加列 | `AIM-Technology` または `PSM` |

返り値は次の構造とする。

```r
data <- list(
  aimt_iamc = ...,
  psm_iamc = ...,
  metadata = list(...)
)
```

Figure固有の `filter()`、`group_by()`、`summarise()`、地域集約、scenario選択、年選択、単位変換はここへ置かない。

Fig.2-b–dのprimary GDXは `load_psm_primary()` で必要になった時点で一度だけ読む。年別GDXから
`vfs_l`、`vff_l`、`vfb_l`だけを読み、リンク単位まで直ちに集約してpanel previewとcombined
Figureで共有する。`vfs_l`はGWの年次設備容量として時間重みを掛けない。`vff_l`と`vfb_l`は
`time_resol`に対応する時間数を掛け、`3.6e-6`でGWhからEJへ変換する。

### `code/plot_common.R`

`plot_common.R` はFigure間で共有する次の設定とhelperを担当する。

- `MyTheme`
- font、axis、legend、marginの共通値
- scenario、technology、carrierの色
- `dataframe.R` の変数・凡例・色定義の読込
- panel間で共有するデータ検証・整形helper

scenario色の現在値は暫定値である。

- ELE: yellow
- H2: red
- SYF: blue

正式な論文用色コードが決まったら `SCENARIO_COLORS` を更新する。technologyとcarrierの色は、名称が確定してから `TECHNOLOGY_COLORS` と `CARRIER_COLORS` にnamed vectorとして追加する。

既存の `dataframe.R` は削除せず、`load_variable_specs()` でisolated environmentへ読み込む。panelからは、たとえば次のように参照する。

```r
VARIABLE_SPECS$secele
VARIABLE_SPECS$finene
VARIABLE_SPECS$emisec
```

### panel scripts

各panel fileは、`data` を受け取ってplot objectを返す関数を1つ定義する。

```r
make_fig1_a <- function(data) {
  assert_panel_data(data)

  panel_data <- data$psm_iamc |>
    # TODO: panel固有のfilter・集計・単位変換
    dplyr::filter(...)

  p <- ggplot2::ggplot(panel_data, ...) +
    ... +
    MyTheme

  return(p)
}
```

panel固有の加工は関数内部に閉じる。データの出所、対象scenario、region、year/timeslice、GDX symbol/dimension、source/display単位、変換係数を確認してから実装する。

輸送リンクは暫定runの旧表記（`H2`、`NH3`、`SYF`）と本runの`TRD`表記
（`H2TRD`、`NH3TRD`、`SYFTRD`）を同じcarrierへ分類する。各runにはどちらか一方だけが
存在する前提とする。Fig.2-cはprimaryのlink activityを基準とし、model-sideの変換効率を
適用して出力とconversion lossを分ける。IAMC出力は照合に使い、現段階で分解しない水素発電等の
収支差はOtherとして表示する。

Fig.1は現在a–dの4 panelとする。aは世界最終エネルギー需要をscenario facetで横並び、bは
2050年部門別最終エネルギー需要をsector facet・scenario x軸で表示する。cは
`Emi_CO2_Ene_Sup_Ele`をPowerとして独立表示し、Other supplyを
`Emi_CO2_Ene_Sup - Emi_CO2_Ene_Sup_Ele`で計算する。dはPSMの発電・curtailment・
storage loss・transmission lossをscenario facetで表示する。Fig.1内のplot titleは置かず、
小文字panel labelとfacet見出しを使う。

## 4. 物理サイズの管理

ggplot object自体は物理的なwidth/heightを持たず、描画deviceで寸法が決まる。panel単体とcombined Figureの寸法は、`main.R` の各 `save_png_mm()` 呼び出しにmm単位で明示する。

```r
save_png_mm(fig1, file.path(fig1_output_dir, "fig1.png"), 174, 187.5)
```

次の原則を守る。

- `path`、`plot`、`width_mm`、`height_mm`を保存呼び出しに明示する。
- panel単体もcombined Figureも同じ書き方にし、それぞれ独立に寸法を調整できるようにする。
- PNGだけを保存し、PDF保存呼び出しは置かない。
- 寸法変更後はcombined Figureを確認し、文字、軸、凡例、panel比率を再調整する。

## 5. Figure layoutと凡例

最終レイアウトは `main.R` のFigure組み立て部分に `cowplot::plot_grid()` で明示する。入れ子のgridへ意味のある変数名を付け、その直前にASCII図を置く。`rel_widths` と `rel_heights` は最終Figureを見ながら調整する。

現在のFig.1は4 panelで構成し、各段の右側へ対応する縦1列の凡例を置く。中段はbと「c＋排出量凡例」の全体幅を6:4とする。

```text
|              a | final-energy legend |
|        b (60%) | c | emissions legend |
|              d | electricity legend  |
```

Fig.2は上段をa:b = 4:1、下段をc:d = 3:2とし、段間に共通凡例を置く。

```text
|       a       | b |
|     scenario legend |
|     c     |   d   |
```

凡例は、凡例を持つplotから `cowplot::get_plot_component()` で位置を指定して抽出する。抽出時に `legend.position`、`legend.direction`、`legend.key.size`、`legend.spacing`、`guides()`を設定する。panel grid内では元plotの凡例を `legend.position = "none"` で隠し、抽出した凡例grobsを `plot_grid()` の独立要素として配置する。

```r
scenario_legend <- cowplot::get_plot_component(
  fig2_a +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    ),
  "guide-box-bottom"
)

fig2 <- cowplot::plot_grid(
  top_row,
  scenario_legend,
  bottom_row,
  ncol = 1,
  rel_heights = c(1, 0.13, 1)
)
```

同じ行・列のaxisを揃え、意図しないplot areaの縮小を抑えるため、原則として次を `MyTheme` で共通化する。

- plot margin
- axis text size
- axis title size
- legend text/title size
- legend position
- panel tag size

凡例の共有や配置変更は、必ずcombined Figure上のplot areaへの影響を確認してFigure単位で調整する。

## 6. 保存

panel単体とcombined Figureは `save_png_mm()` で保存する。このhelperは一時PNGの描画完了後に同名出力を更新するため、再実行時に中途半端な画像を残さない。

```r
save_png_mm(preview_a, file.path(fig1_output_dir, "fig1_a.png"), 174, 62.5)
save_png_mm(fig1, file.path(fig1_output_dir, "fig1.png"), 174, 187.5)
```

`main.R` はpanel previewとcombined FigureをPNGで保存し、PDFは生成しない。出力は `output/fig1`、`output/fig2`、`output/fig3` に保存し、再実行時は同名のPNGを更新する。

## 7. 実装手順

panelを追加または更新するときは、次の順序で作業する。

1. 使用するmodel run、完全なrun token、入力pathを確認する。
2. AIM-Technology、PSM、または両方のどれを使うか決める。
3. GDX symbol、dimension/index順、単位を確認する。
4. 対象scenario、region、year/timesliceを確認する。
5. panel単体の `ggsave()` にwidth/height/units/dpiを直接設定する。
6. panel関数内にデータ加工とplot logicを実装する。
7. `main.R` からpanel関数を呼び出す。
8. `plot_grid()` でpanelと抽出した凡例を配置し、相対幅・相対高さを設定する。
9. panel単体を設定済み寸法で確認する。
10. 最終Figureを結合し、代表出力を視覚確認する。

## 8. 検証と記録

Rコード変更後は最低限、次を行う。

- 変更した全Rファイルをsourceせずに `parse()` する。
- `BUILD_FIGURES <- FALSE` で `main.R` のsource関係を確認する。
- `validate_figure_layout()` を全Figureに実行する。
- `git diff --check` を実行する。
- 対象外のrepository、raw input、既存outputが変わっていないことを確認する。

実データを使った生成時は、さらに次を記録する。

- AIM-TechnologyとPSMのrepository commitまたは `uncommitted` 状態
- scenario group、scenario、region mode、year、timeslice
- 完全なproducer run tokenと入力path
- GDX artifact、symbol/field、dimension、単位
- 実行scriptと生成したFigure/table path
- 数値・構造・視覚check
- warning、既知の制約、未解決事項
- `verified`、`partially_verified`、`unverified`、`failed` のverification status

GDXが存在する、scriptが終了する、画像が描画される、という事実だけでは `verified` にしない。

## 9. 実行前に承認が必要な操作

次の操作は、対象command、input、output、想定時間・規模、上書き有無を明示し、実行前に承認を得る。

- 大規模GDX処理、bulk plot、長時間処理、出力規模が不明な処理
- 既存run、reference data、Figure、table、logの上書き・削除・移動・改名
- model outputのcopy、link、convert、publish
- raw inputやupstream runの変更
- dependencyのinstall/upgrade
- outputのGit管理方針や `.gitignore` の変更

通常の構造確認、静的parse、未実行のpanel template作成はread-onlyまたは低影響の検証として扱う。

## 10. このガイドの更新

panel構成、共通データ契約、物理サイズ管理、保存方法を変更した場合は、コードと同じ作業でこの文書も更新する。コードと文書が食い違う場合は、実行コードと入力artifactを確認し、推測で文書を補完しない。
