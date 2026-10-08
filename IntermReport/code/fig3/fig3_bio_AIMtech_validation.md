# fig3_bio_AIMtech 検証記録（2026-09-29）

Verification status: partially_verified。入力定義・数値集計・描画を確認。producer当時のcommitとmodel statusは未確認。

- 入力モデル: AIM-Technologyのみ。producer token: 2609111525、scenario: 500C-H2、year: 2050、31地域。
- ポテンシャル入力: `//SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output/softlink_IntermReport/2609111525/gams_output/gdx_primary/500C-H2/2050.gdx`。
- 貿易入力: `//SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output/softlink_IntermReport/2609111525/gams_output/gdx_merged/trade_merged.gdx`（ユーザー指定）。
- `emax(ME,MK)`はEQ_ESCMXに使われる供給上限。`data/global/parameter/ene_sup.gms`の定義を確認し、国内資源13種をPJ/yr / 1000でEJ/yrへ変換。Infの世界集計WORを除外。地図外のDF系dummy地域を除外。
- 年別GDXのCRN1STゼロレコードはMEX/RUS/VNM/XCS/XENI/XME/XNF/XSA/ZAFで省略されている。疎パラメータの省略値0として補完。他12種は全31地域で存在を確認。
- `trade_merged(Sc,K_EXP,Sr,Sr,Y5)`はGDX説明文とiamc_mergeコードによりSrからSr.1へ流れるEJ/yr。K_EXP定義はCRNR2=solid biomass、CRNR1=liquid biomass。
- 静的parse、BUILD_FIGURES=FALSEでmain.Rのsourceを確認。既存fig3_bio.Rの地図・色・曲率・円サイズ・凡例の描画体系を継承。
- 入力貿易の合計: 液体25リンク・約12.1 EJ/yr、固体9リンク・約28.5 EJ/yr。
- 描画対象は閾値0.01 EJ/yrと日付変更線フィルター適用後、液体18リンク・約8.00 EJ/yr、固体7リンク・約28.4 EJ/yr。図の合計を全世界貿易量とみなさない。
- 日本の国内資源上限合計: 約2.05 EJ/yr。
- 出力1点: `IntermReport/output/fig3/fig3_bio_AIMtech.png`、300 × 135 mm。
- 実行: 一時検証スクリプトverify_fig3_bio_AIMtech.R。通常生成入口はmain.R。全Figureの一括再生成は実施しない。
- git diff --check、ggplot_build、画像の目視確認を実施。共有run、PSM版のコード・画像は変更していない。
- R起動時locale警告あり。集計・描画への影響は確認されなかった。