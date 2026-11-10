# 水素系燃料の輸送網・時間解像度比較

## 方式別輸送量の解像度比較図：文字拡大（2026-11-10）

指定された `fuel_transport_by_mode_2050_12DAY_vs_6HOUR.png` の1枚だけ、タイトル・軸・凡例・注記を約1.4倍、キャリア見出しを16.8ptへ拡大した。320×180mm・300dpi、配色・積上げ棒・下凡例・数値は保持し、注記を3行へ折り返した。[専用入口](code/plot_fuel_transport_comparison.R)を追加し、既存の全図作成入口も比較図だけ同じ拡大を適用する。[更新範囲と確認](time_resolution_comparison.md#方式別輸送量の比較図の文字拡大2026-11-10追加)。

## 発電図の色対応・凡例順をテンプレートへ統一（2026-11-10）

ユーザーの追加指定に従い、棒グラフ・下側の凡例配置・既存寸法を保ち、色対応と凡例順だけを中間報告fig1-dへ揃えた。順序はHydrogen→Wind→Solar→Geothermal→Biomass→Hydro→Nuclear→Gas→Coal→Oil→Transmission loss→Storage loss→Curtailment。色は同report関数のfill scaleを直接参照する。

6HOUR推移、2050年12DAY/6HOUR比較、以前の4シナリオ2050年比較の発電3 PNGを更新。[発電図だけの実行入口](code/plot_generation_figures.R)と[テンプレート参照関数](code/generation_template_helpers.R)を追加し、既存の両作図エントリにも同じ適用を接続した。[検証・更新範囲](time_resolution_comparison.md#発電図のテンプレート適用2026-11-10追加)。

## 本数表示と12DAYのみの使用経路（2026-11-10）

既存の保持量表を、共通本数／6HOUR使用本数、および12DAYだけで1 PJ/年以上となるリンクの本数・12DAY輸送量・同方式の12DAY総量比で拡張した。管路は54本・1.989 EJ/年（13.6%）、NH3道路1本・0.00120（1.4%）、鉄道24本・0.966（10.0%）、船舶22本・1.761（47.6%）、合成燃料船舶6本・0.429（34.2%）。[拡張表・定義・検証](time_resolution_comparison.md#本数表示と12dayのみの使用経路2026-11-10追加)。

## 残す経路が担っていた6HOUR輸送量（2026-11-10）

ユーザーの考察を受け、12DAYで1 PJ/年以上のリンクを残したとき、そのリンクが6HOUR輸送量の何％を担うかを[集計コード](code/calculate_route_coverage.R)へ追加。残すリンクの6HOUR流量をそのまま足し、12DAY流量で上限を切らない。保持量割合はH2管路89.3%、NH3道路91.1%、鉄道78.9%、船舶46.7%、合成燃料船舶80.2%。管路の落ちる28本は1.446 EJ/年、全除外管路は1.452 EJ/年。これは再配分が必要な現行輸送量であり、費用増・実行可能性・僅差の代替性は未評価。[指標・分布・解釈](time_resolution_comparison.md#残す経路が担っていた6hour輸送量と経路の重要性2026-11-10追加)。

## 輸送手段別の使用経路率・カバー率（2026-11-10）

低解像度runで候補を絞る目的に合わせ、正逆をまとめた1モデルリンク＝1本で数え直した。[集計コード](code/calculate_route_coverage.R)は2050年の両runについて、準備済み `M_LL2` と有効domain `FL_LT` から候補を取り、閾値なしと0.001 EJ/年以上（既存地図の閾値）の2通りで利用率・共通本数・カバー率を算出する。方向別・キャリア合計だった前節とは指標が異なる。

閾値なしのカバー率は全方式100%だが、NH3道路以外は全候補が正値で、ほぼ候補削減にならない。閾値ありではH2管路73/101＝72.3%、NH3道路2/3＝66.7%、NH3鉄道55/72＝76.4%、NH3船舶23/45＝51.1%、合成燃料船舶8/14＝57.1%。[全候補・利用率・検証と絞込み上の含意](time_resolution_comparison.md#輸送手段別の使用経路率とカバー率2026-11-10追加)を参照。図・GDX・モデルは変更していない。

## 方向別の差分図と包含度（2026-11-10）

ユーザー依頼で、2050年の同名シナリオについて、各キャリアの円を外した方向別差分図を追加した。[出力4 PNG](output/global_H2_Transport_Global_2611061840_vs_2610281619_difference_2050/)はH2・NH3・合成燃料各1枚と縦並びの組図。[実行コード](code/plot_transport_difference.R)は `12DAY − 6HOUR` を同じ有向リンク・方式で計算し、赤を負、青を正、太さを差分絶対量、矢印を物理的な輸送方向とする。凡例は地図外。

6HOURを分母とする共通輸送量 `sum(min(F12,F6))/sum(F6)` はH2 78.3%、NH3 60.7%、合成燃料52.6%。0.001 EJ/年以上の方向別経路・方式が12DAYでも同じ閾値以上となる割合は、それぞれ71/101＝70.3%、78/120＝65.0%、8/14＝57.1%。輸送量の一致度と、利用経路の包含度を区別する。[定義・数値・検証](time_resolution_comparison.md#方向別差分図と包含度2026-11-10追加)。

## 凡例を地図外へ分離（2026-11-10）

ユーザー指示により、輸送方式・線幅・円の大きさ／内訳の凡例を、すべて地図下の専用領域へ移した。地図135mm相当＋輸送凡例18mm＋円凡例22mmの3段に分け、単独図は300×175mm、推移組図は600×350mm、解像度比較図は300×350mmへ変更。配色・輸送量・共通尺度・港座標は保持する。

6HOURの年別・推移・12DAY比較28枚と、以前の4シナリオの2050年16枚、計44枚の輸送網PNGを更新。各エントリの通常実行にもこのレイアウトを反映する。以下の過去記録の135mm等の寸法とコードhashは、修正前の履歴として扱う。

最終確認：44枚すべての更新・新寸法と全56 PNGの正常デコードを確認し、残り12枚の集計図は修正前とSHA256一致。統合・H2・NH3・合成燃料、空の初期年、推移・解像度比較の代表図を視覚確認した。表示中PNGへの直接上書きで5枚に警告が出たが、一時描画後の差し替えで解消し、旧寸法の輸送網図はゼロ。一時PNGの残存もゼロ。体裁修正の範囲でverified（Codex、2026-11-10）。

## 6HOURの推移と12DAY比較（2026-11-10）

ユーザー指定の `global_H2_Transport / Global / 2611061840` について、Baselineを除き、両トラック許可の1ケースを作図した。2020～2050年の7時点のテーマ別推移、4時点の輸送地図、同名の `2610281619 / 12DAY` との2050年比較を[新しい出力先](output/global_H2_Transport_Global_2611061840_2020_2050_vs_2610281619/)に36 PNGで保存する。

[結果・重み・来歴・比較上の留意点](time_resolution_comparison.md)と[実行コード](code/plot_transport_time_resolution.R)を参照。6HOURの6時間と12DAYの30.417時間は入力GDXのwtで照合し、y00にも適用、mainの年集計は再加重しない。2050年のH2管路−7.73%、NH3鉄道−24.8%、NH3船舶+5.41%、合成燃料船舶+15.1%などを確認した。

輸送は6HOURでもy00の年間流量であり、6時間別運行の比較ではない。また6HOURは年内の日数を増やす一方で日内を集約するので、一方向の細分化と呼ばない。年需要・主要費用等は一致するが、需要ピーク由来の配電初期容量には差がある。単純な一要因実験とは区別する。

旧図を保持し、地図関数を[共通ファイル](code/transport_network_functions.R)へ切り出した。旧エントリもそれを読む。初期年の疎なゼロH2・空の転換投入を扱うため円の読取りだけを拡張し、正の指標があるのに詳細が欠けた場合は引き続き停止する。過去のhashは当時の履歴であり、現在コードのhashではない。

## 以前の2050年比較（2026-10-29）

最新の対象は、ユーザー指示によりBaselineを除いた500Cの4ケース。円グラフ付き輸送網16枚と、テーマ別比較図4枚を[2050_500C出力先](output/global_H2_Transport_Global_2610281619_2050_500C/)に置く。[実行コード](code/plot_transport_network.R)と[円・比較図の関数](code/transport_summary.R)はこのprojectが所有する。既存analysisと共通描画・地理入力は保持する。

## 最新版：円グラフと2050年比較図（2026-10-29）

PSM producer tokenは`global_H2_Transport / Global / 2610281619`、sourceは`NH3_transportation / d73f5795dc70b8c5fad8c83ecc27c3d7823f8fd7`。入力は同じNAS runの`main/merged_output.gdx`（iamc_gdx(Sc,Sr,Sv,Y5)）と、4ケースの2050年primary。mainは14,121,971 bytes・1,140,143 recordsで、Baselineの収録は2020/2025のみだったが、ユーザーがBaseline除外を指示したため補完しない。4つの500Cには2050年の必要変数があり、scenario_table.csvでも全4ケースOptimal。モデルの再計算・GDXコピー/修正なし。以前の20図もそのまま保存する。

対象順とx軸ラベル：

| scen_id | 比較図のラベル |
| --- | --- |
| 500C-H2_Pipeline-NH3_Rail_Ship | No trucks |
| 500C-H2_Pipeline_Truck-NH3_Rail_Truck_Ship | H2 + NH3 trucks |
| 500C-H2_Pipeline_Truck-NH3_Rail_Ship | H2 trucks |
| 500C-H2_Pipeline-NH3_Rail_Truck_Ship | NH3 trucks |

全ケースで水素管路・NH3鉄道/船舶を許可する。x軸は輸送の実利用ではなく、トラックの利用許可の違いを示す。

- `generation_mix_2050.png`：mainのR31集計31地域を一度だけ合計。中間報告fig1-dの発電源・色・親変数の区分を使い、ガス水素混焼をGasへ足す。発電は正側、出力抑制・蓄電損失・送電/配電損失は負側。全4ケースで正側合計がSec_Ene_Ele（約369 EJ/年）と一致することをチェックする。詳細272地域とR31集計を同時に加えない。
- `fuel_transport_by_mode_2050.png`：水素・NH3・合成燃料の3パネル、方式ごとの積上げ棒。primaryの道路/鉄道集合で分類し、同地点の接続・転換を除外した地域間リンクの正逆流量を合計する。色はキャリアの色系統を維持して方式別に濃淡を分ける。地図の表示閾値や微小値除外を使わない。
- `biomass_transport_2050.png`：中間報告fig3_bioと同じR31間CRN/CRNTRD（固体）とBMS/BMSTRD（液体）の船舶リンク。正逆合計を積み上げる。外生輸入設備・転換量は除く。
- `transmission_capacity_and_flow_2050.png`：AC端点の物理的な地域間送電リンクについて、設備容量（TW）と正逆合計の送電量（EJ/年）を2パネルで1枚にする。R31内・R31間を積み分け、元のfig2-bのR31間容量も読めるようにした。容量は同じ物理ペアを一度だけ数え、方向別に複数容量レコードがあれば値の一致を検証してから最大値を使う。

輸送量と送電量は区間ごとの延べ量であり、同じエネルギーが複数区間を通れば繰り返し計上される。mainの年集計値には時間重みを再適用しない。primaryの流量は従来と同じ30.417×3.6e-6でEJ/年へ換算し、vfs_lの送電容量はGWからTWへ1/1000する。

### 円グラフの定義と限界

中間報告fig3-bの式を流用し、詳細地点のSec_Ene_HydからH2→NH3/SYF/SYMの投入量を引き、その正の残差をDirect H2とする。内訳はすべてH2量基準（EJ/年）で、NH3・合成燃料の製品エネルギー量ではない。共通関数fig3_pie_layerの6.2mm最大半径を使い、面積尺度は4ケース・4種類の地図で共通。閾値は0.1 EJ/年。個別輸送網図でも同じ水素内訳の円を表示し、輸送リンクのキャリアだけを切り替える。

現在の合成燃料はR31集約のため、R31の転換投入を勝手にR272へ配分せず、既存R31代表港座標で表示する。詳細ノードは従来の地域中心、海運端点は引き続き31代表港そのものの位置。これにより旧fig3-bの詳細地点だけへの絞込みで落ちるR31合成燃料投入を残す。

Sec_Ene_Hydは生産側の集計で、地点別の転換投入には移入H2も含み得る。元図の正残差処理を踏襲するため、円のDirect H2は直接最終需要を厳密に示さず、円の合計も世界の生産量/需要量に一致する指標ではない。今回も負の残差があり、R31合成燃料約2.01 EJ/年を含めて、残差を0へ切り上げた分は約2.48 EJ/年/ケース。監査値をコンソールへ出す。円は参照図の定義による地理的な内訳表示として使用し、需給収支の検証へ転用しない。

### 出力と実行

比較図は各テーマ1枚。地図16枚は各scen_idの下。新フォルダのみへ保存し、旧Baseline図を削除しない。地図300×135mm、発電300×175mm、燃料320×180mm、バイオマス300×160mm、送電320×170mm、全て300dpiのPNG。CSV/JSON/集計データ等は保存しない。

通常はworkspaceから上記実行コードをRscriptで実行する。最新出力を上書きするときだけ`--replace-generated`を指定する。送電図の凡例抽出はggplot2 4.0の複数guide-boxを考慮し、bottomを選んでcowplot::get_legendへ渡す。古い20図の手順・係数・来歴は以下に履歴として残す。

最新確認：変更R2本のstatic parse成功、4ケース2050年のIAMC存在・Optimal・発電内訳と総発電量の一致、未対応端点なし・物理ペアの重複容量整合、20 PNGのデコード/サイズ/配置とBaselineなしを確認。比較図4枚と代表の統合・合成燃料地図を視覚確認した。新しい4ケースの共有陸上経路は45ペア。初回の送電凡例抽出の警告は修正し、比較4図を再生成、終了コード0・描画警告なし。R起動時の既存locale警告のみ。確認者Codex、2050年の集計・作図と記載した参照円の定義の範囲でverified。Direct H2を最終需要と解釈した収支検証は対象外。

- main SHA256：`11e9eae4998dd9f7cabfc8867bc37f7a3f5f3dfe99308b1cfdd739fcae66984d`（14,121,971 bytes、最終更新2026-10-29 17:06:39 JST）。
- `plot_transport_network.R` SHA256：`d35ec25600e588ac7309f6cbeb9bae6afbfb425c315f113817fb86ec0064cac0`。
- `transport_summary.R` SHA256：`c195e07ef44df74b78b92efe5b1b6dbaf10772400bf91b068935be99b0c145a5`。
- 生成環境はR4.4.3/ggplot2 4.0.0、下記の既存依存関係にcowplot/gtable/tidyrを使用。参照plot commitと地理入力SHA256は下記から変更なし。作図コードはトピック側の未コミット変更で、他projectのdirty stateを保持する。

## 初期版と港座標修正の記録

## 入力と来歴

- PSM producer token：`global_H2_Transport / Global / 2610281619`。
- run：`//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output/global_H2_Transport/Global/2610281619`。
- 入力は`gams_output/gdx_primary/<scen_id>/2050.gdx`の5ファイルのみ。ほかの年、AIM-Technology、secondary/IAMCは今回読まない。
- producer：`NH3_transportation / d73f5795dc70b8c5fad8c83ecc27c3d7823f8fd7`。runの`modelstat/systemlog/systemlog.txt`と照合し、`modelstat/summary.csv`で全5ケース2050年Optimalを確認。run当時の未コミット差分の有無はsystemlogだけでは確定できない。
- PSM・Global・12DAY・2050年。run設定でNH3 sectoralと輸送スイッチを確認した。
- 参照plot repo：`8842341883a2066aa8323a551dbfb08ac8cf55ed`。着手前からの他projectのdirty stateは保持。今回のコードと図はtopic側の未コミット成果で、plot repoへの新規実装はない。
- 生成環境：R 4.4.3、gdxrrw 1.0.6（GDX API `C:/GAMS/win64/26.1`）、ggplot2 4.0.0、dplyr 1.1.4、jsonlite 1.9.1、ragg 1.3.3。NASを読める実行環境の値。sandbox内のggplot2は3.5.1。パッケージ導入なし。
- 元GDXは読取り専用。GDXコピー・集計表・CSV・JSON・PDF等の追加出力なし。

## 対象ケースと出力

1. `500C-H2_Pipeline-NH3_Rail_Ship`
2. `500C-H2_Pipeline_Truck-NH3_Rail_Truck_Ship`
3. `500C-H2_Pipeline_Truck-NH3_Rail_Ship`
4. `500C-H2_Pipeline-NH3_Rail_Truck_Ship`
5. `Baseline`

各scen_idフォルダに`network_all_2050.png`・`network_hydrogen_2050.png`・`network_ammonia_2050.png`・`network_synfuel_2050.png`。中間報告fig3-bと同じ300×135 mm、300 dpi、3543×1594 px。

## 数値・地理の定義

- `vff_l(L,T)`・`vfb_l(L,T)`の元単位はGW。producerの`wt(T)=ceil(365/12*1000)/1000=30.417`時間を`T=y00`にも適用し、各方向を`sum_T[value]*30.417*3.6e-6`でEJ/年へ換算。中間報告の`primary_time_weight_from_timeresol()`を使用。
- `L_H2ROAD`・`L_ROAD`・`L_RAIL`は各primaryの保存集合を読む。H2管路・NH3海運・SYF海運はproducerの命名と技術対応による。同一点の接続・キャリア転換・外生輸入機器は除外する。
- 陸上の線幅は同じ地点ペア・キャリア・方式の正逆合計gross量。船舶は方向別に描き、矢印の先が到着地点。方向別合計とgross合計の一致を確認する。ネットワーク合計は通過区間を重複計上し、世界生産量・需要量とは異なる。
- 数値微小値の除外は正逆合計1e-8 EJ/年以下。表示閾値は0.001 EJ/年で、陸上はgross、船舶は方向別量に適用。
- 地理入力は`AIMPSY_plot/IntermReport/data/nodes_centroid.geojson`（R272）と`searoute_r31_representative.geojson`（31代表港）。陸上リンクは従来のR272中心座標、NH3・SYF船舶の表示端点は代表港そのもののLongitude/Latitudeを使う。NH3の集計上のノードIDはproducer commitの`data/set/M_NH3_PORT.gms`対応を保持し、海運の描画座標だけ港の位置へ置き換える。GeoJSONのR31・N対応がproducerの31港すべてと一致することを確認する。SYFは従来から同じ代表港座標を使用している。
- 合成燃料の検索はSYF/SYMだが、producerの海運技術は`ship_liq_synliq`で実際の描画はSYF海運。独立したSYMやLOHCの輸送を追加した図ではない。
- 背景は同じR272ポリゴンの外周。輸送線は接続の模式線で、実在の道路・線路・航路の形状ではない。

## 中間報告の体裁

色は`IntermReport/code/plot_common.R`と`fig3/fig3_common.R`の既存定義を直接参照する。

| 対象 | 色 | 線 |
| --- | --- | --- |
| H2管路 | thistle3 | 実線 |
| H2トラック | thistle3 | 点線 |
| NH3鉄道 | #8E5EA2 | 実線 |
| NH3トラック | #8E5EA2 | 点線 |
| NH3船舶 | #8E5EA2 | 方向付き曲線、curvature=0.12 |
| 合成燃料船舶 | mediumorchid1 | 方向付き曲線、curvature=-0.12 |

背景grey97・境界grey65、線幅平方根変換の範囲0.15～3、船舶の閉じた矢印1.8 mm、地図内凡例を元図に揃えた。全20枚で共通の線幅尺度、最大約2.0997 EJ/年。陸上の複数モードは5ケースの和集合から固定レーンを決め、端点でずれをゼロへ戻す。表示対象の共有陸上経路は47ペア。日付変更線では短い接続を選び、陸上は分割、船舶はwrap部分の複製とクリッピングで描く。

円グラフは今回は省略。元図が使う地点別`Sec_Ene_Hyd` IAMC集計値はprimaryに直接保存されておらず、当該runの`main/merged_output.gdx`もまだない。転換量の元変数はprimaryにあるが、別の代用指標は描かない。前の試作の輸送取扱量を示す円も削除。小さい白丸は元図と同じR31代表地点で、量を表さない。mergedが利用可能になれば、元図の用途内訳と円面積を追加する。

## 実行と確認（2026-10-29）

workspaceからの実行例：

```powershell
& 'C:/Program Files/R/R-4.4.3/bin/Rscript.exe' 'topics/NH3transport/visualization/transport_visualization/code/plot_transport_network.R' --replace-generated
```

NAS読取り権限が必要。既存PNGの更新時のみ`--replace-generated`を使う。今回更新するのは、このprojectで作成した20枚のみ。

R static parse、全5ケースのOptimal、保存集合253/212/355、31港座標、未対応端点・非有限値なし、時間ラベルと換算、海運方向別合計とgrossの一致を確認。20 PNGすべて3543×1594 px、5ケース各4枚、project内は20 PNG＋R＋READMEの22ファイルのみ。統合・H2・NH3・合成燃料の代表4図について、配色・凡例・方向・重なり・日付変更線を視覚確認した。確認者はCodex、状態はこのrunの2050年輸送集計・作図の範囲でverified。モデル自体の再計算やrun全体の再現性検証は行っていない。

港座標への修正前の生成は終了コード0、描画時警告なし。R起動時の既存locale警告4件は残るが、数値・英語ラベルの描画には影響しなかった。Skillのquick_validateは実行を試みたが、既存PythonにPyYAMLがないため起動不可。frontmatterのname/descriptionと追加した配置規則を手動確認し、追加インストールは行わなかった。

H2トラックは許可した両ケースとも全経路が表示閾値未満。NH3トラックは許可した各ケース3経路（GMB-XX–SEN-XX、JPN-CB–JPN-KS、JPN-CG–JPN-KS）が閾値以上。表示されない微小輸送をゼロと断定しない。

### トラック輸送量の追加確認（2026-10-29）

同じ5ケースの2050年primaryについて、保存集合L_H2ROAD/L_ROADで抽出したvff_l/vfb_lを正逆合計し、表示閾値0.001 EJ/年と微小値除外1e-8 EJ/年の両方を適用せず再集計した。元GDX・図・作図コードは変更せず、一時Rコードからコンソール集計のみ実行。以下は地域間リンク輸送量の合計であり、最終需要や重複のない貨物量ではない。

| 500Cのトラック許可 | H2道路合計（TJ/年） | NH3道路合計（PJ/年） |
| --- | ---: | ---: |
| H2・NH3ともに許可 | 0.80956134 | 86.40014696 |
| H2のみ許可 | 0.71970710 | 0（候補なし） |
| NH3のみ許可 | 0（候補なし） | 86.39920132 |
| 両方なし | 0（候補なし） | 0（候補なし） |

Baselineも両トラック集合が空。許可したH2道路は355候補すべて正の値だが、最大リンクでも0.01548 TJ/年、1 TJ/年以上の経路はゼロ。H2管路＋道路のリンク量に占める道路割合は約0.00000491～0.00000553％。数値上はゼロではないが、ほぼ使われない。微小値が解法の数値的残差か、厳密な最適解でも必要な輸送かは、この集計だけでは断定しない。

許可したNH3道路は253候補中145が正値、1 TJ/年以上は4経路、地図の表示閾値1 PJ/年以上は3経路。NH3鉄道＋道路のリンク量に占める道路割合は約0.891％。両トラック許可ケースの内訳はJPN-CB–JPN-KS=59.67337、JPN-CG–JPN-KS=25.07527、GMB-XX–SEN-XX=1.20267、ALB-XX–MKD-XX=0.44783 PJ/年。第4経路は表示閾値未満。日本の2経路が道路合計の約98.1％を占める。NH3のみ許可ケースもほぼ同じ配分。確認者Codex、今回の2050年raw集計の範囲でverified。

### 海運端点の表示修正（2026-10-29）

ユーザー指示に従い、NH3海運の描画端点を代表港を含むR272地域の中心から代表港そのものの座標へ変更した。モデル内の接続ID・輸送量・正逆方向・表示閾値・線幅尺度・陸上レーンは保持する。既存GeoJSONの31地点について、Longitude/LatitudeとPoint geometryが一致し、R31/N対応もproducerのM_NH3_PORT.gmsとすべて一致した。合成燃料は従来から同じ港座標を使用しており、追加の座標変更はない。

同じ5ケース2050年から20 PNGを更新。初回保存で両トラック許可ケースの統合図・NH3図の2枚に書込み警告が出たため、その2枚だけ同じ全5ケースの尺度・レーンで再生成し、PNGへの正常保存と置換を確認した。一時PNGと一時Rコードは除去済み。最終20枚すべてをデコードし、3543×1594 px・追加ファイルなしを確認。H2単独5枚のSHA256は修正前とすべて一致。統合図・NH3図で港への接続と凡例を視覚確認し、海運端点表示の修正範囲でverified（確認者Codex）。

コードSHA256：`ccf52926a80a01467aad422a48ab732c9b4555da0586a25480ecfc3db9e5897e`。

地理入力SHA256：nodes=`d873708310df22d222d617d4faccc9ca989505e516fe0315ff1ea15eec5c3fa0`、sea representative=`44f70c13c66f41fc7bc21e9df39cc6eb927bdbb38dba472e1efb6ae3a39c07ef`。
