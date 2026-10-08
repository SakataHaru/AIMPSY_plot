# fig3_bio 検証記録（2026-09-29）

Verification status: partially_verified（コード・集計・描画を確認。producer runのmodel statusと当時のcommitは未確認）。

## 入力と定義

- 可視化repo基点commit: 4d7f8ab59c726856ca82bc0c1b28c1d8c887cd59、今回の変更はuncommitted。
- 使用モデル: PSM。AIM-Technology IAMCの利用実績は、この図の入力に使わない。
- 完全な入力: `C:/@Research_Workplace/300_AIMPSY-with-AI/AIMPSY_plot/IntermReport/data/AIM-PSY/global_softlink_IntermReport/Global/2609121358/gams_output/gdx_primary/500C-H2/2050.gdx`
- producer token: `2609121358`、scenario: `500C-H2`（表示選択1.5C-H2）、year: 2050、time_resol: 6HOUR、31地域。
- 資源上限: `pmax(ME,MK)`、TWh/yr × 0.0036 → EJ/yr。EPSの制約を0として保持するため `squeeze=FALSE` で読む。
- 貿易: `vff_l(L,TS)` / `vfb_l(L,TS)`、GW × 6時間 × 3.6e-6 → EJ/yr。符号方向は既存Fig.3 helperに従う。
- 地理: `IntermReport/data/nodes_centroid.geojson`、`searoute_r31_representative.geojson`、`M_R272.csv`。
- 制約の根拠: 隣接モデルrepoの `core/prog/calc.gms` のEQ_PSMX、`data/parameter/enesup.gms`、`data/set/M_MK.gms` と実際のGDXを照合。
- 実GDXにCRNALL総量制約は存在しない。13種の国内資源上限を合算する。輸入上限を除外。

## 実施した確認

- 変更Rファイルのstatic parse、BUILD_FIGURES=FALSEでmain.Rのsource関係を確認。
- 検証時はGDX UELを使ってバイオマス貿易リンクだけをrgdxで読み込んだ。全figureの一括再生成は実施しない。
- 31地域の資源上限合計は約321 EJ/yr、日本は約2.15 EJ/yr。
- 描画閾値0.01 EJ/yrと既存の日付変更線フィルター適用後: 固体49リンク、約25.2 EJ/yr、液体35リンク、約12.9 EJ/yr。これらは描画対象リンクの合計であり世界全貿易量ではない。
- ggplot_buildが成功。300 × 135 mm PNGの地図、色、円、矢印、凡例を目視確認し、色凡例を下方へ調整。
- git diff --check。既存fig3_b.Rと既存図は変更していない。
- 実行スクリプト: 一時検証用 `verify_fig3_bio.R`。通常の再生成入口は `IntermReport/code/main.R`。
- 新規出力1点: `IntermReport/output/fig3/fig3_bio.png`。
- 起動時のR locale警告あり。図の集計・保存は成功。全シナリオと全Figureの再実行、producer model statusの検証は未実施。