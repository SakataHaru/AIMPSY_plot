# バイオマス図の区分実装とH2利用可能量の調査（2026-09-29）

状態: `partially_verified`。区分集計・描画・需要照合・PSM run状態を確認済み。発電・水素向けの地域別純利用可能量の定義と描画方式はユーザー判断待ち。

## 実装した内容

- fig3_bio / fig3_bio_AIMtech の円を Energy crops（黄緑 #A9D65D）と Other biomass（緑 #238B45）に分割。
- 作物は CRNP01–08（PSMでは R_CRNP01–08）。その他は AGR / FRR / MAN / MSW / 1ST。1STは既存の第一世代バイオ燃料由来枠として「その他」に含める。作物8等級との二重計上を避ける。
- 固体輸送 #359EAA、液体輸送 #006D77 の青緑系に変更。
- 円の総量・サイズ・輸送量は変更していない。利用可能量の再定義は未実装。
- CRNALLが資源別合計より小さいrunは、区分への割当が未定義なのでエラーで停止し、勝手な比例配分をしない。今回H2にはCRNALLが存在しないため該当しない。

更新出力: `IntermReport/output/fig3/fig3_bio.png` と `fig3_bio_AIMtech.png`（各300×135 mm）。

## 入力・来歴

- PSM: `C:/@Research_Workplace/300_AIMPSY-with-AI/AIMPSY_plot/IntermReport/data/AIM-PSY/global_softlink_IntermReport/Global/2609121358/gams_output/gdx_primary/500C-H2/2050.gdx`。
- PSM producer token 2609121358、500C-H2 / 2050 / 6HOUR、資源上限は31地域。表示シナリオ選択は1.5C-H2。
- PSM `pmax(ME,MK)` はTWh/yr ×0.0036 → EJ/yr。国内13資源を合算し、輸入上限を除外。
- PSM貿易 `vff_l(L,TS)` / `vfb_l(L,TS)` は既存の年次集計を使用。6 ×3.6e-6 → EJ/yr。
- AIM-Technology: `//SY_3600_01/atfs02/Individual/hsakata/AIMEnduse/output/softlink_IntermReport/2609111525/gams_output/gdx_primary/500C-H2/2050.gdx`。`emax(ME,MK)`、PJ/yr ÷1000 → EJ/yr。
- AIM-Technology貿易: 同run `gams_output/gdx_merged/trade_merged.gdx`、`trade_merged(Sc,K_EXP,Sr,Sr,Y5)`、EJ/yr。
- AIM-Technology最終需要照合: ローカル `IntermReport/data/AIM-Tech/softlink_IntermReport/2609111525/main/merged_output.gdx` の `iamc_gdx(Sc,Sr,Sv,Y5)`。
- 地理: `IntermReport/data/nodes_centroid.geojson`、`searoute_r31_representative.geojson`、`M_R272.csv`。
- producer runの `modelstat/systemlog/systemlog.txt` はモデルcommit `a3fdf9efae35f418a37c4c46e39dc1fe1d73ba95`、branch develop_synfuelを記録。ローカルモデルHEADも一致しdirtyなし。ただしデータpinと実行時全設定を完全に復元したわけではない。
- 共有サーバー `AIMPSY/AIMPSY-master/scenario/global_softlink_IntermReport.gms` と `shell/pbs/pbs_softlink_IntermReport.sh` を照合。PBSは現在ELEを選択しており、H2の当時の起動コマンドそのものは保存確認できていない。
- runの `main/scenario_table.csv` はH2のscen_dpol / scen_epol =500C-H2、scen_tpol=H2、Status=Optimalを記録。`modelstat/summary.csv`も一致。
- 可視化repo基点 `4d7f8ab59c726856ca82bc0c1b28c1d8c887cd59`、作業はuncommitted。元からのmain.R / data_import.R / PLOTTING_GUIDE.md変更を保持。

## 調査結果

H2の設定は `tech_bio=default`、`sup_bio=on`、`tech_bioimp=on`、`tech_biotrade=on`。H2分岐は水素コストlowを設定し、バイオマス総量上限をAIM-Technology実績に切り替えない。

記憶されていた第1方式は `tech_bio=aimtR31` の分岐に対応する。`data/parameter/enesup.gms` の `IAMC_VAR_BIO` は `Prm_Ene_Bio_Ele` と `Prm_Ene_Bio_Hyd`。これらの和が `CRNALL` 上限になり、`EQ_PSMX`で制約する。今回のシナリオファイルではSYFがこの設定に切り替わる。今回H2の年別GDXにはCRNALLレコードがなく、第1方式の上限を図の円に採用する根拠はない。

H2では13資源の上限が `EQ_PSMX(ME,MK)` で国内一次資源供給を制約する。`pmax`自体に需要部門利用量を差し引く式はない。一方、`data/parameter/demand.gms`では固体 `Fin_Ene_SolidsBio` と液体 `Fin_Ene_Liq_Bio` をモデルの固定需要へ入れる。貿易・外部輸出入の設定に応じた調整式もある。`EQ_NPB`でこの需要と発電・水素等への供給を同時に満たす。

実GDXの `dmd(N,I,MT)` を全symbolから抽出し、31地域×2carrier、すべて年次bus `y00`を確認。6HOURの内部時間スケールを戻す係数6とGWh→EJ係数3.6e-6で集計すると、固体約54.9、液体約28.8 EJ/yr。AIM-Technologyの同シナリオ・年の最終需要と丸め誤差程度で一致した。最初のUELフィルターによる需要読込は一部レコードしか返さなかったため破棄し、最終scriptは `rgdx.param` からの抽出に修正済み。

つまり第2方式に近い運用だが、「総ポテンシャルから最終需要を引いた定数」が地域別上限として入力されているわけではない。需要を確保した残りを、輸送・変換・発電・水素の最適化の中で使う方式である。

### 数量の比較（31地域合計、EJ/yr）

| 量 | エネルギー作物 | その他 | 合計 |
|---|---:|---:|---:|
| PSM資源別上限 | 182.567 | 138.472 | 321.040 |
| AIM-Technology資源別上限 | 182.567 | 114.971 | 297.538 |

PSMとAIM-Technologyでその他のポテンシャルが一致していない。両図は各モデルの実際の上限を使っており、同じポテンシャルを使っているとは表現しない。個別資源の差の原因は今回の利用可能量判断とは分けた追加調査事項。

AIM-Technologyの発電向け一次バイオマスは58.443 EJ/yr、日本1.234 EJ/yr。今回のIAMCには `Prm_Ene_Bio_Hyd` レコードが見つからないため、参照する場合はprimaryまたはreporting mappingでゼロか未出力かを確認する必要がある。

PSMの国内資源上限は日本約2.15 EJ/yr、AIM-Technologyは約2.05 EJ/yr。

## 提案するプロット方針（判断用、未実装）

**推奨: 円はPSMの実際の国内資源上限を維持し、名称を Domestic biomass resource potential にする。需要部門への供給・発電・水素への利用を、別の参照表示または補助表で示す。** これなら今回H2の制約と円の2区分が一致し、総資源を「発電・水素だけが使える量」と誤読させない。

「発電・水素用の残余量」を円のサイズにする場合は、需要部門用の**一次資源換算量**と国内資源への帰属を変換リンク・貿易・外部輸入から定義する。固体・液体の最終エネルギーをそのまま足して引く方法は採用しない。液体バイオ燃料は変換損失があり、地域の需要には輸入も使える。資源を輸出する地域と需要の多い地域を単純に独立計算すると誤る。また、残余量を作物／その他へ割り当てるルールもユーザー判断が必要。

第1方式のAIM-Technology発電・水素利用量は参考量として併記できるが、今回H2のモデル上限として置き換えることは推奨しない。

この報告段階では、図の配色・区分だけを実装し、総量定義・凡例名・参照表示の変更を保留している。

## 検証

`IntermReport/code/verify_biomass_resource_split.R`をcode directoryから実行。対象は両図と小規模な監査CSVのみで、full figure一括実行・モデルsolveは実施していない。

- 変更Rのstatic parse、両図のggplot_build成功。
- 両モデル31地域について、2区分合計と元の円総量の一致を確認（許容差1e-10 EJ）。
- PNGを目視し、2色の円・凡例・青緑の輸送線を確認。
- 固定需要62地域carrierレコード、year/scenario・単位・AIM-Technology IAMCとの照合を確認。
- git diff --check成功。R起動時にlocale警告あり。計算・保存は正常終了。
- 監査CSV: `IntermReport/output/fig3/biomass_resource_audit_2609121358/`。資源区分CSV、PSM固定需要CSV、AIM-Technology需要・一次利用IAMC CSVを保存。
- AIM-Technology producer model status、全データpin、地域別一次資源の需要帰属は未確認。全研究結果がverifiedであるとは扱わない。
