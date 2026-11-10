# 水素貯蔵の地域別導入傾向：報告用図

2026-11-10の文字拡大指定：北米の図12・図06のみ再出力。図12は軸目盛9/10→14 pt、y軸名11→16 pt、ファセット12→17 pt、凡例10→15 pt、棒の数値3→4.5 mm、主題17→22 ptへ拡大。`MyTheme` の `strip.text.x` 固有指定も上書きし、実際のファセット見出しへ反映した。脚注は4行に分けて10.5 ptへ拡大し、凡例/脚注の領域配分と数値ラベルの余白を調整。`transport_by_mode.R --region=NorthAmerica` で当該1 PNGのみ更新できる。

図06は共通 `power_response_text_theme()` で軸目盛12 pt、軸名14 pt、見出し15 pt、凡例12 pt、上部のrun説明12 ptへ拡大。発電凡例は3列、図の高さは6.5→7.5 inchにして文字の領域を確保した。指定済みの色・積み上げ順を保持。`update_power_response.R` で当該1 PNGを更新し、main/system_evolutionの再生成も同じ文字設定に接続した。検証：4caseの正常年・時間重みと既存総量の再現、変更R5本の構文、完成2 PNGの凡例・ファセット・軸・数値の表示と重なりを確認。描画以外の仕様変更なし、他地域・他図は今回更新していない。実行警告は既存locale4件のみ。確認者Codex。

2026-11-10の発電図パレット指定：北米の [図06](output/NorthAmerica_2610291832/06_power_system_response.png) を、ユーザー提示の `secele` 14区分へ更新した。色と下→上の積み上げ順をOil w/o CCS、Oil w/ CCS、Coal w/o CCS、Coal w/ CCS、Gas w/o CCS、Gas w/ CCS、Nuclear、Hydro、Biomass w/o CCS、Biomass w/ CCS、Geothermal、Solar、Wind、Hydrogenで固定。凡例も同じ順で左から右へ並べる。`prog/generation_style.R` が提示された色・ラベル・順序の正本で、`main.R` と `system_evolution.R` も参照する。CCS有無はnative generator技術名のccs要素で分け、発電量は従来どおりAC bus・generator familyベースの6時間積分。混焼の燃料別配賦へは変更しない。Oil w/ CCSのようなデータにない区分も指定色を凡例に表示する。

限定再描画の `prog/update_power_response.R` はNorthAmerica/2610291832・2045年・500Cの4貯蔵caseを直接読み、図06の1 PNGだけを更新する。各LSTのOptimal/Normalと1460電力時刻・重み6を確認し、未分類の有意な発電なし、分類後の発電総量と元dispatch積分が1e-5 TWh以内で一致。総発電はOFF 16641.835432、両設備ON 16731.749789、タンクのみ16736.044026、空洞のみ16651.249904 TWh/年。右側の設備差分は既存と同じ一次容量定義・色・順序で再計算した。変更R4本の構文、出力画像の色・凡例・積み上げ順を確認。北米以外の図06や他番号の図は今回再描画していない。R起動の既存locale4警告のみ。モデル・共有入力出力の変更、追加solveなし。研究全体のpartially_verifiedを継承。確認者Codex、2026-11-10。以下の旧コードSHAは各更新前の履歴である。

2026-11-06の依頼。以前の `analysis/storage_effect` を参考に、新しい圧縮機分離版をアジア・北米・欧州で比較する。Baselineは作図しない。ユーザー指定によりコードは `prog/`、図は `output/` に置く。

2026-11-10追加：北米2045年・両貯蔵ONについて、日別図04を「空洞が下・タンクが上＋黒い合計線」へ更新。USA/CAN/MEXの個別図15と、両設備容量を円グラフで重ねた地図16も追加した。現在は計53 PNG。詳細は下記「北米の貯蔵詳細図」を参照。

同日、[地域別の生産・輸送地図13/14](#2026-11-10追加地域別の生産輸送ネットワーク)を追加。アジア・北米それぞれに4caseの個別地図と2×2比較図を保存し、この時点で計49 PNG。図07と同じ地理範囲・投影を使う。

同日、ユーザー指定の輸送比較図に合わせ、アジア・北米の2045年について水素系キャリア輸送量の方式別比較図12を追加した。水素・NH3・合成燃料を横3panel、各panel内で貯蔵4caseを横並びにする。この時点の図は計39 PNG。

| 2045年の輸送比較 | 図 |
| --- | --- |
| アジア | [水素系キャリア輸送量](output/Asia_2610301310/12_fuel_transport_by_mode_2045.png) |
| 北米 | [水素系キャリア輸送量](output/NorthAmerica_2610291832/12_fuel_transport_by_mode_2045.png) |

同日追加依頼で、06の発電積み上げを上下逆順に更新し、太陽光・風力・電解槽・蓄電池inverterの4ファセット経年図10と、太陽光の容量・発電・出力抑制・実利用率の確認図11を各地域へ追加した。現在は各地域12 PNG＋地域比較1 PNG＝計37 PNG。追加コードは `prog/system_evolution.R`。全地域500Cの4caseのみで、Baselineは含めない。

| 追加図 | アジア2045まで | 北米2045まで | 欧州2030まで |
| --- | --- | --- | --- |
| 4設備の容量推移 | [図10](output/Asia_2610301310/10_capacity_evolution_facets.png) | [図10](output/NorthAmerica_2610291832/10_capacity_evolution_facets.png) | [図10](output/Europe_2610281735/10_capacity_evolution_facets.png) |
| 太陽光の利用率 | [図11](output/Asia_2610301310/11_solar_capacity_and_utilization.png) | [図11](output/NorthAmerica_2610291832/11_solar_capacity_and_utilization.png) | [図11](output/Europe_2610281735/11_solar_capacity_and_utilization.png) |

## 2026-11-10追加：地域別の生産・輸送ネットワーク

コードは `prog/production_transport_maps.R`。対象は上記Asia/2610301310とNorthAmerica/2610291832、2045年・電力6HOUR/水素日別・500Cの4貯蔵case。Baselineと不成立の2050年を除く。入力GDX・primary・LSTは下記の共通producer階層を直接参照し、統合IAMCは使わない。

| 地域 | 4シナリオ比較 | タンク＋洞窟の個別図 |
| --- | --- | --- |
| アジア | [図14](output/Asia_2610301310/14_production_transport_scenarios_2045.png) | [図13](output/Asia_2610301310/13_production_transport_2045_500C-H2storage.png) |
| 北米 | [図14](output/NorthAmerica_2610291832/14_production_transport_scenarios_2045.png) | [図13](output/NorthAmerica_2610291832/13_production_transport_2045_500C-H2storage.png) |

各地域に `13_production_transport_2045_<scenario>.png` が4枚、`14_production_transport_scenarios_2045.png` が1枚、計10 PNGを追加した。再実行はこの10枚だけを更新する。

```powershell
& 'C:/Program Files/R/R-4.4.3/bin/Rscript.exe' --vanilla 'topics/H2storage/visualization/storage_adoption_20261106/prog/production_transport_maps.R'
```

**円の定義**：参照されたNH3transport図のH2 allocationとは異なり、依頼どおりキャリアの製造出力を表示する。Hydrogenは電解＋石炭/SMR（CCSあり・なし）の一次水素、AmmoniaはHaber–BoschのNH3出力、Synfuelは水素由来のFT液体燃料＋メタネーション出力。輸入・貯蔵払出・NH3 crackingによる再転換は生産に加えない。H2原料とその下流のNH3/Synfuel製造出力は別工程なので、円全体の合計は工程別gross生産量であり、最終エネルギーや用途配分ではない。円の面積がこの合計に比例し、扇形比率は各キャリアの出力比率となる。

**生産の集計・位置**：prepared input `M_LL2(L,L2)` を実在集合 `L` で限定し、electrolysis/haber_bosch/fischer_tropsch/methanationを選ぶ。`vff_l(L,T)` ×入力 `wt(T)` ×対応する `feta(L,I0,I1)` ×3.6e-6でEJ/年へ換算。効率は実inputから読み、全caseで0.7/約0.865/0.73/0.77。変換の逆流合計は1e-5 EJ/年未満を確認。化石水素は `vg_l(N,H2,R,T)` のh2_coal/h2_coal_ccs/h2_smr/h2_smr_ccsを日別GWhとして重み1で積分する。

この版ではNH3/SYF変換linkが「詳細ノードH2→R31キャリアpool」につながる。製造出力の円は**変換linkの水素供給側の詳細ノード**へ置く。R31 poolに全国の工場が実在するとみなした配置や、別の詳細ノードへの再配分はしない。座標自体はモデルノードの代表点なので、工場の実地点を示すものではない。

**線・集約点の定義**：図12と同じphysical link抽出・時間積分を使用。H2パイプラインは往復gross合計、NH3/SYF船は方向別で矢印を付ける。R31の中空点とラベルは集約需給poolの図示用代表点で、地理資料 `searoute_r31_representative.geojson` のLongitude/Latitudeを参照する。新しいNH3transport版のgateway mappingは旧producerへ適用しない。航路は模式線であり、実航路や港湾配置を主張しない。詳細ノード→R31 poolの受渡しlinkは輸送実績として描かない。輸入に由来するキャリアも物理輸送には含まれるので、地域内生産量と輸送量が一致する必要はない。

地図は図07と同じ `coord_sf`、Asia:経度60–150/緯度−12–57、NorthAmerica:経度−179–−50/緯度5–84。円の最大径12 mmと線幅の尺度は**地域内の4ケースで共通**。地域間は独立尺度、TWhの貯蔵容量とEJ/年の生産量の円尺度は別物である。線の表示下限は方向別（pipelineは往復合算後）0.001 EJ/年。除外輸送総量はAsia各case約0.00480–0.00795 EJ/年、北米約0.00157–0.00292 EJ/年。表示外生産はAsia 0、北米約0.000376–0.000380 EJ/年（Guamを含む）で、集計総量からは引かない。case別の正確な除外値は実行時コンソールと個別図の脚注に表示する。

| 地域 | 貯蔵case | H2製造 EJ/年 | NH3製造 EJ/年 | H2由来Synfuel製造 EJ/年 |
| --- | --- | ---: | ---: | ---: |
| アジア | OFF | 20.1750896 | 6.35629519 | 0.0000159424 |
| アジア | タンク＋洞窟 | 20.1963951 | 6.33968535 | 0.0000153258 |
| アジア | タンクのみ | 20.1964218 | 6.33971355 | 0.0000176558 |
| アジア | 洞窟のみ | 20.1780916 | 6.35618058 | 0.0000164619 |
| 北米 | OFF | 6.74053516 | 4.67468212 | 0.0001703789 |
| 北米 | タンク＋洞窟 | 6.88973599 | 4.67439649 | 0.0002371220 |
| 北米 | タンクのみ | 6.89324809 | 4.67422826 | 0.0002665317 |
| 北米 | 洞窟のみ | 6.76972594 | 4.67370276 | 0.0001829188 |

Synfuelの地域内製造が極小のため、今回の共通円尺度では扇形をほぼ識別できない。生産値を輸送値やH2投入量に置き換えて見かけ上拡大しない。

**検証（Codex、2026-11-10）**：全8caseでproducer commit、2045年Model1 Optimal/Solver1 Normal、h2_time_version4、電力/日別/年量の重み、変換効率と逆流許容範囲、詳細生産ノード・輸送端点と座標欠落なし、物理輸送linkの一意性を確認。表示下限を適用する前の輸送総量は既存図12の全24地域/case/carrier値と表示桁で一致した。R構文検査、10 PNGの読取、両地域の比較図と個別図の凡例・円面積/線幅尺度・地図範囲を確認。実行の警告は既存localeの4件のみ。対象集計と表示は確認済み、producerのdirty/data Gitlinkのgap等を含む研究全体のpartially_verified状態は継承する。モデル・共有データ・既存図は変更せず、追加solveなし。script SHA256: `491BAAF6F8D484BC97EC3400962856C8D380B1612C594489DCBD1D48372ADEEF`。topicsはGit初期化前でcommitなし、既存のAIMPSY_AI/AIMPSY_plotの変更を保持した。

## 北米の貯蔵詳細図（2026-11-10追加）

**最新の表示指定**：日別在庫は1枚1地域とする。USA/CAN/MEXの図15を各230×120 mm、300 dpiの横長図として再出力した。サンプル同様、地域名と2045年のタイトル、日・日末在庫TWhの軸、下部にタンク/空洞/Totalの凡例を置く。空洞下段・タンク上段・黒い合計線は継続。3地域まとめ図04は旧版として保持するが、北米の日別図として案内・再生成しない。以下に記載する初回の図04更新は過去時点の記録である。両entry pointを1地域1枚へ変更し、詳細entry pointの `--daily-only` 指定で3枚のみ更新できる。今回この指定で実行し、365日の収支残差最大0 GWhとR構文を再確認、米国の個別図で配置を視覚確認した。地図は再生成していない。最新SHA256: `northamerica_storage_detail.R` = `221FEEA78907176C03D7FDF7F0F136FBDA6E1F65F1B9FC10CF45861F07E4D885`、`main.R` = `F35C78FC4D60BBC2B3F6EE6A4F8C7E214494D1B8658BDDFDAC9CC4145DDC0531`。

対象はNorthAmerica/2610291832、500C-H2storage、2045年の1case。producer・入力来歴は下記と同じ。`prog/northamerica_storage_detail.R` で対象primaryだけを直接読み、日別図04を更新し、図15を3枚、図16を1枚新規作成する。他地域・他シナリオ・従来のタイプ別地図07は更新しない。`prog/main.R` の北米図04の描画部分も同じ色・積み上げ順・合計線・縦配置へ変更した。main全体の再実行はしていない。

| 図 | 出力 |
| --- | --- |
| 旧版：R31別の日別在庫まとめ（採用対象外） | [04](output/NorthAmerica_2610291832/04_daily_inventory_by_country.png) |
| 米国 | [15 USA](output/NorthAmerica_2610291832/15_daily_inventory_USA_2045.png) |
| カナダ | [15 CAN](output/NorthAmerica_2610291832/15_daily_inventory_CAN_2045.png) |
| メキシコ | [15 MEX](output/NorthAmerica_2610291832/15_daily_inventory_MEX_2045.png) |
| 設置容量の円グラフ地図 | [16](output/NorthAmerica_2610291832/16_storage_capacity_pie_map_2045.png) |

日別量は `ve_l(N,I,S,T)` の日末在庫GWhを `M_R272.csv` によってR31へ合算し、1000で割ってTWhへ換算する。時間重みは掛けない。365日の積み上げ面は水色 `#A6CEE3` のタンクを上、オレンジ `#FDBF6F` の空洞を下とし、両者の同日合計を黒線で重ねた。国別に縦軸を変える。

地図は `ves_l(N,I,S)` の設置容量GWh/1000を使用。日末在庫や年間入出庫ではない。各詳細ノードの円面積がタンク＋空洞の合計に比例し、扇形が両設備の容量比率を示す。地図色は参考画像および図07を踏襲してタンク青 `#0072B2`、空洞オレンジ `#E69F00`。地図範囲・座標は図07と同じ。合計はタンク7.484299 TWh＋空洞0.758022 TWh＝8.242321 TWhで、参考画像中の7.42 TWhは転記しない。表示範囲外容量0.000779985 TWhをタイトル合計に含む。

```powershell
& 'C:/Program Files/R/R-4.4.3/bin/Rscript.exe' --vanilla 'topics/H2storage/visualization/storage_adoption_20261106/prog/northamerica_storage_detail.R'
```

**検証（Codex、2026-11-10）**：2045年LSTのModel1/Solver1、version4、時間重み、集合・座標・重複なしを確認。全ノードの365日在庫は非負かつ設置容量以内で、`ve(day) - eeta^24*ve(previous day) + vh(day)` の年末循環を含む残差最大0 GWh。R31別合計線とタイプ別在庫の和、地図の円合計と設置容量総和の一致を確認した。変更R2本の構文検査、5 PNGの読取、3か国まとめ図・メキシコ個別図・円地図の表示を確認。実行は既存locale警告4件のみ。確認範囲は当該集計・表示で、研究全体のpartially_verified状態は継承する。モデル・NAS上の出力は変更せず、追加solve・依存導入・Git書込みなし。

code SHA256: `prog/northamerica_storage_detail.R` = `39A97E9DADB303F867490F330E96C7A349C654499153D98B696A82915648B5EB`、更新後 `prog/main.R` = `D6D6E9D23BD1A0A46B92DD94EF6B28A303F356E9A1B5EC838ADBFF40457F5055`。以前のmain SHAは過去時点の来歴として保持する。

## 対象と実行

PSM producer root: `//SY_3600_01/atfs02/Individual/hsakata/AIMPSY_2nd/output/H2_storage_compressor`。

| 地域 | 完全token | 主図 | 推移 | 対象R31 |
| --- | --- | --- | --- | --- |
| Asia | 2610301310 | 2045 | 2020–2045、5年刻み | CHN/IDN/IND/JPN/KOR/MYS/THA/VNM/XEA/XSA/XSE |
| NorthAmerica | 2610291832 | 2045 | 2020–2045、5年刻み | USA/CAN/MEX |
| Europe | 2610281735 | 2030 | 2020–2030、5年刻み | XE15/XE10/XE3/XEWI |

| 地域 | 報告用まとめ | 圧縮機の容量推移 | 空洞利用率とタンク導入 |
| --- | --- | --- | --- |
| アジア2045 | [主図](output/Asia_2610301310/00_report_overview.png) | [圧縮機](output/Asia_2610301310/08_compressor_capacity_timeline.png) | [空洞・タンク](output/Asia_2610301310/09_cavern_potential_and_tank_deployment.png) |
| 北米2045 | [主図](output/NorthAmerica_2610291832/00_report_overview.png) | [圧縮機](output/NorthAmerica_2610291832/08_compressor_capacity_timeline.png) | [空洞・タンク](output/NorthAmerica_2610291832/09_cavern_potential_and_tank_deployment.png) |
| 欧州2030 | [主図](output/Europe_2610281735/00_report_overview.png) | [圧縮機](output/Europe_2610281735/08_compressor_capacity_timeline.png) | [空洞・タンク](output/Europe_2610281735/09_cavern_potential_and_tank_deployment.png) |

欧州は2035年に4ケースすべてInfeasibleなので、2035年の解を使用せず2030年を主図にした。北米・アジアの2050年も除外。最終summaryではなく各年LSTのModel1/Solver1を確認する。各地域500Cの貯蔵OFF・タンク＋洞窟・タンクのみ・洞窟のみの4ケース。日別水素収支ON・電力6HOURは共通。

モデルは各run systemlogの `H2_storage/8002fe88e99fce839140c97bead693b3ac0295ce`、GAMS39.3/Linux。実行時source dirty state/data Gitlinkは未取得。モデル・データ来歴の残るgapは [run診断](../../analysis/run_diagnostics/README.md) を参照。作図コードは本project、R4.4.3・既存gdxrrw/GAMS26.1のreaderを使用。共有出力への書込み、GDX本体の複製、モデル実行、依存導入はしない。背景形状は既存 `AIMPSY_plot/IntermReport/data/nodes_centroid.geojson`、地域対応は同 `M_R272.csv` を参照する。

実行はworkspaceから：

```powershell
& 'C:/Program Files/R/R-4.4.3/bin/Rscript.exe' --vanilla 'topics/H2storage/visualization/storage_adoption_20261106/prog/main.R'
```

出力は各 `output/<Region>_<token>/` の10 PNGと、`output/regional_comparison_capacity.png` の計31 PNG。実行はこのprojectの同名PNGを更新する。以前の図や共有runは変更しない。PNG以外のデータ・集計表・GDXコピーは書き出さず、集計と照合値はコンソールに出す。所要時間は共有ストレージの応答に依存し、数分程度。大きな全リンクflowは展開しない。

上記は初回31図のentry point。追加図10/11と更新した06だけを作る場合は次を実行する（3図×3地域、うち06は更新、10/11は追加）。入力gmx_tは大きいsymbolなので、太陽光・AC・主年・1460時刻へ絞ってRに返すが、GDX内部の走査に時間がかかり、共有ストレージでは数分以上かかる。初回31図を再描画する必要はない。

```powershell
& 'C:/Program Files/R/R-4.4.3/bin/Rscript.exe' --vanilla 'topics/H2storage/visualization/storage_adoption_20261106/prog/system_evolution.R'
```

`--region=Europe` などでその地域だけ更新できる。地域比較図はこの場合も全3地域の主年容量を一次結果から読み、年別statusを再確認して更新する。

## 統合結果のシナリオ不一致を回避

作図前照合で、アジアの `main/merged_output.gdx:iamc_gdx(Sc,Sr,Sv,Y5)` に、貯蔵OFFやBaselineへONと同じ容量が記録される不一致を確認した。例：CHN/2045のタンク容量5913.597 GWhがON/OFF/Baselineにあり、一次結果OFFの `ves_l` は0。CHNの電解容量にもON/OFF/Baselineで同じ値がある。単独タンクcaseの一次容量が正なのに該当統合値がない例もある。**今回の比較図は統合IAMCを使わず、全地域を年別一次結果から再集計する。** 原因がmergeだけか、前段の後処理も含むかは未確定で、モデル修正はこの作図の範囲に含めない。過去の別tokenの統合結果まで誤りとする証拠ではない。

## 入力symbolと単位

年別入力は各producerの `gams_output/gdx_primary/<case>/<year>.gdx`。時間ラベル確認のみ `data/input/<case>.gdx:wt(T)` も参照する。

| Symbol | 寸法・意味 | 表示・扱い |
| --- | --- | --- |
| ves_l(N,I,S) | 設置済み貯蔵エネルギー容量 GWh | タンクH2S/h2_tank、洞窟H2C/h2_cavern。/1000でTWh |
| vfs_l(L) | 設置済みlink容量 | H2→H2S/H2Cを圧縮機GW_H2として集計。電力消費GWと区別 |
| ve_l(N,I,S,T) | 日末在庫 GWh | H2貯蔵だけをUEL指定で読取。時間重みを掛けず/1000でTWh |
| vh_l(N,I,S,T) | 貯蔵の純日出庫 GWh/day | 在庫更新・年末循環の照合に使う |
| h2_stg_charge_l(N,I,S,MT) | 注入前の総入庫 GWh/day | 365日合計。/1000でTWh/年。損失後の実貯蔵量とは異なる |
| h2_stg_discharge_l(N,I,S,MT) | 在庫側の総出庫 GWh/day | 出口の損失を受ける前。入庫と同様、6倍しない |
| h2_stg_elec_l(N,T) | 圧縮の運転時刻電力 GW | 6時間重みで積分し/1000でTWh/年 |
| eeta(N,I,S) | 1時間の在庫保持率 | 前日在庫×eeta^24−vh＝当日在庫で確認 |
| h2_time_version | convention scalar | version4を確認 |
| vgs_l(N,I,R) | 発電設備容量 GW | ACのSolar/Wind/biomass_ccsを集計 |
| vg_l(N,I,R,T) | 発電運転量 GW | AC・発電技術・1460時刻だけを限定読取し、6倍/1000でTWh/年 |
| h2_cavern_potential(N_ALL) | 洞窟容量上限 GWh | 詳細ノードだけを集計し、設置量/ポテンシャルを計算 |

Battery powerはAC→DCのinverter容量、electrolyzerはAC→H2の入力電力容量。GW_H2の圧縮機容量と混同しない。発電分類はnative AC設備名からSolar/Wind/Hydro/Nuclear/Biomass/Coal/Gas/Oil/Geothermal/Hydrogenへまとめる。バイオマス混焼はその発電機のCoal分類、ガス水素混焼はGas分類。燃料投入量を発電量へ読み替えない。

詳細ノードN_INDだけを地域に合計し、R31集計と二重に足さない。GDXの疎なゼロは対象集合を確定した後に補う。stock・flowの許容差以内の小さな負値は収支チェック後に表示用ゼロへ丸める。

## 図の読み方

| PNG | 内容 |
| --- | --- |
| 00_report_overview | 主年の容量・年入庫・国別立地・365日在庫の報告用まとめ |
| 01_storage_cases | OFF/両設備/タンクのみ/洞窟のみの容量と年間総入庫 |
| 02_adoption_timeline | タンク・洞窟の導入量と年間総入庫の推移 |
| 03_storage_and_compressors_by_country | 両設備ONでのR31別エネルギー容量と圧縮機GW_H2 |
| 04_daily_inventory_by_country | 両設備ONのR31別365日在庫。パネル別縦軸 |
| 05_inventory_and_flows_cases | 3貯蔵caseの日別在庫・総入庫/出庫。パネル別縦軸 |
| 06_power_system_response | 主年の発電構成、OFFに対する設備容量差 |
| 07_storage_capacity_map | タンク・洞窟の立地。円面積が容量、両技術で同一scale |
| 08_compressor_capacity_timeline | 独立した圧縮機容量の年次推移、3caseで同一縦軸 |
| 09_cavern_potential_and_tank_deployment | R31別の洞窟導入/ポテンシャル、タンク・洞窟設置容量、洞窟最大在庫/設置容量 |
| 10_capacity_evolution_facets | 太陽光・風力・電解槽・蓄電池inverterのGW容量推移、4caseの色・線種比較 |
| 11_solar_capacity_and_utilization | 主年の太陽光容量・実発電量・出力抑制・実利用率を別単位で比較 |
| 12_fuel_transport_by_mode_2045 | アジア・北米のみ。方式別総輸送量EJ/年、3キャリア×4貯蔵case |

09の灰色bar全体が利用可能ポテンシャル100%、橙が導入割合。導入率が低くても、別立地での需要・輸送・圧縮費用等によりタンクが導入され得る。「地域全体で空洞ポテンシャルが余る＝タンクが不要」とは結論しない。最大充填率は各日の地域合計在庫の最大値/地域設置容量で、ノードごとの最大値を足さない。

ポテンシャル0の地域は導入率の分母がないため左panelから除き、タンク容量を示す中央panelには含める。設置洞窟0の地域は右panelの充填率から除く。洞窟がある各ノードの設置容量≦ポテンシャルも照合した。

ポテンシャルは採用済みモデル入力の暫定Onshore上限であり、科学的校正済みの実資源量ではない。地図の表示範囲外の容量も全体集計に含め、図注に量を記す。欧州R31はEU加盟国の厳密な集合ではなく指定欧羅巴4区分。地域比較図は欧州のみ2030年なので同一年の地域順位として解釈しない。

## 検証・分析の現在地

### 2026-11-10追加：方式別水素系キャリア輸送量

実行コードは [transport_by_mode.R](prog/transport_by_mode.R)。参照図は `topics/NH3transport/visualization/transport_visualization/output/global_H2_Transport_Global_2611061840_2020_2050_vs_2610281619/fuel_transport_by_mode_2050_12DAY_vs_6HOUR.png`。その配色・3キャリア横並び・方式別積み上げ・EJ/年の体裁を使用し、x軸を今回の4貯蔵シナリオに置換した。Baselineは含めない。小さい合成燃料輸送も読めるよう総量を3桁の数値ラベルで付記。縦軸は各地域の3キャリア間で共通、地域間では独立なのでアジアと北米の棒の高さを直接比較しない。

```powershell
& 'C:/Program Files/R/R-4.4.3/bin/Rscript.exe' --vanilla 'topics/H2storage/visualization/storage_adoption_20261106/prog/transport_by_mode.R'
```

この実行は上記2 PNGだけを作成・更新する。各340×190 mm、300 dpi。モデル実行、GDXのcopy/変換/編集、既存の他図の再描画、CSV等の追加出力はしない。

入力は本README記載のアジア `2610301310`・北米 `2610291832` の4case、2045年primary `vff_l(L,T)` と `vfb_l(L,T)`。対応する入力GDXの `M_LL2(L,L2)`、地域に有効な `L`、`wt(T)` を使う。producer commitは8caseの属する各run systemlogで8002fe8を確認し、各2045年LSTでModel1/Solver1を確認した。シナリオの不一致があったmain統合結果は使わない。

候補は準備済みM_LL2で `pipeline_h2/truck_h2/rail_nh3/truck_nh3/ship_nh3/ship_liq_synliq` を特定し、実Lに絞る。両端のbus名を除いて同じ地点になる接続を除外する。地域の詳細ノード・R31集計ノードへの対応、同一方式の物理ペアに重複したモデルlinkがないことを確認する。ここでいう地域間は詳細ノード間を含み、R31境界を越えるものだけに限定しない。貯蔵入出庫・変換link・同一地点pool接続・域外輸入generatorは対象外。

**日別H2を6倍しない**：`h2_time_version=4` を確認。H2パイプラインはd001–d365のGWh/dayを365日合計し、wt=1。NH3・合成燃料はy00の旧来のscaled GW activityで、wt(y00)=6時間を掛ける。`sum((vff+vfb)*wt)*3.6e-6` でEJ/年へ換算する。正逆の両方向を加えるgross輸送量で、経路の各区間を数えるため最終需要と等しい指標ではない。距離は掛けず、EJ-kmでもない。数値のmap cutoffは適用しない。

今回のM_LL2に道路・鉄道輸送方式はなく、正の輸送はH2パイプライン・NH3船舶・合成液体燃料船舶（SYF）。参照図の6色凡例は維持し、未導入方式を図注に記した。方式の不存在を、候補はあるが最適化で利用されなかったという意味にはしない。

| 地域 | case | H2 pipeline EJ/年 | NH3 ship EJ/年 | Synfuel ship EJ/年 |
| --- | --- | ---: | ---: | ---: |
| アジア | OFF | 10.61090 | 2.583648 | 0.00231254 |
| アジア | タンク＋洞窟 | 8.721109 | 2.482751 | 0.00222498 |
| アジア | タンクのみ | 8.712246 | 2.507607 | 0.00256373 |
| アジア | 洞窟のみ | 10.62282 | 2.562080 | 0.00243228 |
| 北米 | OFF | 0.5037824 | 0.2477651 | 0.02270761 |
| 北米 | タンク＋洞窟 | 0.1836747 | 0.2666875 | 0.03160072 |
| 北米 | タンクのみ | 0.1463965 | 0.2657320 | 0.03552007 |
| 北米 | 洞窟のみ | 0.5238073 | 0.2024060 | 0.02437777 |

両設備ONではH2輸送がOFF比でアジア約17.8%、北米約63.5%減る。一方、北米のNH3・合成燃料輸送は増えており、全キャリアの輸送が一律に減る結果ではない。輸送容量・輸送費の変化や、輸送量変化の因果経路は本図だけでは評価しない。

**検証（Codex、2026-11-10）**：8caseの主年状態・version・重み、物理端点と一意性を確認。アジアは各case 229物理link（H2 119、NH3 55、SYF 55）、読取87090方向/時刻record。北米は62物理link（H2 56、NH3 3、SYF 3）、40892 record。H2は全て日別365時刻、NH3/SYFは全てy00のみ。選択linkの直接積分と方式別総和は1e-9 EJ以内で一致。R構文、2 PNGのデコード・数値ラベル・凡例・地域別縦軸を視覚確認した。今回の限定集計・表示の範囲は検証済み。共有source dirty/data Gitlink等の未取得事項による研究全体のpartially_verified状態は継承する。最終実行の警告は既存R起動localeの4件のみ。source code SHA256: `F6A315A2E245FAECB505CE2BDB9747F5A67B7CBEB35FA3FDDC50C1DBCF161AAC`。

### 追加分析：太陽光容量が減る理由と発電量の見え方

**2026-11-10追記：北米のタンクと日内/日間の役割を区別する。** 実producer `8002fe88e99fce839140c97bead693b3ac0295ce` の `core/prog/calc.gms` のEQ_H2DAY/EQ_H2STGと、`inc_prog/import_parameter.gms` のh2_day_wtを再確認した。電力は6時間刻み、水素は各日の積分収支であり、年量の直接需要は365日へ均等配分、年量NH3等の変換活動もH2側で日割りになる。このため、貯蔵OFFでも同じ日の昼に電解してその日の水素需要を満たせる。6時間ごとのH2在庫・供給連続性を表す仕組みではないので、「タンクを追加して初めて昼の太陽光を夜のH2需要へ回せる」と説明しない。

タンク追加の主な自由度は**水素製造日と使用日を切り離すこと**にある。日別収支だけなら、その日の水素所要量を当日の製造・輸送などで賄う必要があり、毎日ある程度再現する昼間の日射は短い電解運転で日量を確保する電源として価値を持ちうる。風力の不規則な強弱が日をまたぐ場合、強風日に多く作っても貯蔵なしでは別の日の不足へ使えない。タンクがあれば日間の不足を在庫で補えるため、PVの「毎日の水素供給を下支えする価値」が下がり、風力の強風日の発電を使う構成へ移れる。これはモデルの構造と整合する有力な解釈であり、太陽光が毎日安定・風力が常に日間変動優勢という一般則ではない。両者の実際の入力資源系列、電力需要、電解負荷、限界価格を同時に照合して原因を確定したわけではない。

北米2045年の両設備ONではOFF比PV容量−230.700 GW/発電−250.535 TWh、風力容量約＋64.8 GW/発電＋440.323 TWh、電解槽＋26.8 GW、電池inverter＋5.91 GW。したがって「日中PV→夜間供給のための電池をタンクが置換した」「電解設備が必ず減る」とは説明できない。強風日の製造を増やす電解能力を確保するという解釈とは整合するが、利用率上昇を確認したわけではない。PV発電の減少率−5.634%は容量の−5.903%に近く、容量係数改善だけの見かけではない。風力増とPV減は1対1ではなく、バイオマス等の電源構成・圧縮電力・水素損失等も含む。

同caseのタンク7.484 TWhに対して空洞0.758 TWhと導入量に約10倍の差があり、タンクのみcaseでも大きな設備差、空洞のみでは小さな設備差という図06は、利用可能な貯蔵量の差とも整合する。「タンクという方式が普遍的にPVを減らす」とせず、この入力ではタンクが大きな日間融通を担うと読む。年総入庫/容量のタンク約30.2倍、および日別在庫の頻繁な増減から、今回の説明を主に季節間貯蔵へ置く必要はない。ただしサイクル長分解は未実施。

報告用の表現案は「水素貯蔵により日ごとの製造義務が緩み、日射の毎日の繰り返しに依存した供給構成から、風況のよい日に製造して日をまたいで利用する構成へ移った可能性がある。これに伴いPVが減り風力が増えた」。今後の直接確認は、タンク立地別の入力PV/風力発電可能量、電解入力、注入/払出を同じ365日で照合し、低風況日を前後に在庫で越えているかを見る。時刻別電力価格や電解の昼夜配分は補助的に確認する。この追記ではproducer sourceの読取と既検証集計の考察のみ、モデル変更・追加solve・追加図なし。確認者Codex、2026-11-10。機構の因果確定は未検証、研究全体はpartially_verified。

対象は従来06と同じ**AC busの系統側PV**。屋根置きAC2は含めない。特にアジア2045年には屋根置き容量が約3527 GW（OFF）あるので、地域の全PV容量と読み替えない。10の電解槽は入力電力GW、蓄電池はgrid inverterの電力GWで、蓄電池のエネルギー容量GWhではない。

**容量と発電量は、ともに減っている。** 下表は両設備ONとOFFの一次結果。発電量は時間別vg_l×6時間と、独立な年量vp_l(AC,P_SOL)で一致を確認した。

| 地域・主年 | PV容量 OFF→ON GW | 容量変化 | PV発電 OFF→ON TWh/年 | 発電変化 | 実容量係数 OFF→ON |
| --- | ---: | ---: | ---: | ---: | ---: |
| アジア2045 | 13311.382→12942.401 | −2.772% | 15476.843→15111.596 | −2.360% | 13.2726→13.3288% |
| 北米2045 | 3908.308→3677.608 | −5.903% | 4446.675→4196.140 | −5.634% | 12.9880→13.0251% |
| 欧州2030 | 530.868→526.965 | −0.735% | 539.306→537.959 | −0.250% | 11.5970→11.6537% |

06の右panelは設備容量の**差分**、左panelは全電源の**総発電量の積み上げ**なので、同じ相対変化でも目立ち方が違う。アジアの−369 GWは絶対量として大きいが、元の系統側PV約13311 GWに対して約2.8%。実発電も365.247 TWh減っており、「容量が大きく減っても発電が変わらない」という前提は当てはまらない。アジアでは風力が9640.094→10005.680 TWh、＋365.587 TWhで、PVの減少とほぼ対応する。北米の風力は＋440.323 TWh、欧州は＋47.785 TWhだが、PV減少量と一対一ではない。

**容量削減の仕組みとして整合的な解釈**：日別H2収支に対して、貯蔵があると製造日と使用日を在庫でつなげる。風況のよい日に製造したH2を別の日に供給する選択肢が増え、毎日のH2製造を支えるPV・電力貯蔵等の設備構成を組み替えられる。今回の両設備ONでは、3地域とも風力容量が増え、PV容量が減っている。アジアでは電池inverter・電解容量も減る一方、北米では電解容量・主年の電池容量が増えるため、単純な一対一の設備代替ではない。これは数式構造と解の変化に基づく考察で、特定の失風日・時間帯の限界価格や、制約を一つずつ固定した対照計算による因果寄与までは特定していない。2020年からの再帰的設備選択の差も含む。

**実利用率の改善は小さく、出力抑制減少が容量減を一部相殺する。** 同じrunの入力 `gmx_t(N,I,R,T,Y5)` と主年vgs_lで発電可能量を計算し、実vg_lとの差を出力抑制とした。これは本体reporting.gms:440のCURTAILMENTと同じ定義。モデル入力の容量係数上限なので、設備の追加故障等を別途推定した量ではない。

| 地域 | 発電可能量 OFF→ON TWh/年 | 出力抑制 OFF→ON TWh/年 | 抑制率 OFF→ON |
| --- | ---: | ---: | ---: |
| アジア | 約17302→16801 | 約1825→1690 | 約10.5→10.1% |
| 北米 | 約4880→4557 | 約434→361 | 約8.89→7.91% |
| 欧州 | 約558→556 | 約18.5→17.7 | 約3.31→3.18% |

アジアの年発電変化は、OFFの設備加重resource CFを基準に、①容量差−479.585 TWh、②設備立地構成によるresource CF差−20.566 TWh、③出力抑制量の減少＋134.904 TWh、合計−365.247 TWhと算術分解できる。②は日射条件のよい立地への大幅な移転を示しておらず、設備加重の発電可能CFはむしろ微減。実CFの改善は＋0.0562 percentage pointに留まる。北米は①−288.079、②−35.566、③＋73.109、合計−250.535 TWh。欧州は①−4.101、②＋1.964、③＋0.790、合計−1.347 TWh。

この分解は会計的な恒等式で、独立した因果効果ではない。抑制量減少には容量を減らして発電可能量自体が減った影響も含むため、アジアの134.904 TWhを「水素貯蔵が全量吸収した余剰電力」と解釈しない。報告では、**「水素貯蔵ありでPVから風力へ構成が移り、PV発電も容量に近い割合で減少した。PVの実利用率はわずかに改善した」**という表現が妥当。

10/11を含む追加図の作成では全60 scenario-yearのLSTを確認し、12主年caseで入力gmx_tからの発電可能量≧実発電を確認。年量P_SOLと時間積分の差は全caseで0.01 TWh未満。算術分解は1e-6 TWh以内で閉じる。追加図を視覚確認し、元の発電順を反転した06も確認した。コンソール表示にtibbleのdigits引数警告が出たが、作図・計算は完了し、引数を通常data.frame表示へ修正して構文検査した。以後の図数は37。モデル実行はしていない。

**アジア2045年の読み取り**：両設備ONはタンク6.422 TWh・洞窟0.198 TWh、独立圧縮機はそれぞれ約146・5.56 GW_H2。中国のタンクは5.914 TWhで地域タンクの約92.1%。洞窟ポテンシャルのある7区分はすべてほぼ100%導入し、各区分の洞窟最大在庫も設置容量へ達する。洞窟のないIDN/JPN/KOR/MYSにもタンクがある。モデルの洞窟容量上限を利用しつつ、タンクで補う形が明瞭である。

年間総入庫は両設備ONでタンク約227・洞窟約7.90 TWh。容量に対する年総入庫は約35.4・40.0倍で、年末在庫や一度だけの充填量ではない。圧縮電力は約27.6 TWh/年。タンクのみでは容量約6.52 TWh・圧縮機148 GW_H2、洞窟のみでは約0.198 TWh・5.94 GW_H2。洞窟のみcaseでは大きなタンクを代替できず、系統全体の設備差も小さい。

両設備ONのOFF比ではPV約−369 GW、風力約＋86.4 GW、電池inverter約−59.2 GW、電解入力容量約−35.5 GW。BECCS容量はほぼ同じ。この差は2020年からの再帰runの結果で、2045年だけ貯蔵を追加した瞬間的効果ではない。費用の優位性・因果の経路や、年内変動の周波数分解までは未評価。

**北米2045年の読み取り**：両設備ONはタンク7.484 TWh・洞窟0.758 TWh、圧縮機は約150・24.1 GW_H2。タンクは米国約6.20 TWh、カナダ約1.27 TWhに集中し、メキシコは約0.012 TWh。3区分とも洞窟はポテンシャルをほぼ100%導入し、最大在庫も設置容量へ達する。年間総入庫はタンク約226・洞窟約37.1 TWhで、容量比約30.2・49.0倍。圧縮電力は28.7 TWh/年。タンクのみcaseは容量8.28 TWh・圧縮機171 GW_H2となり、洞窟も使うcaseではタンクの容量と圧縮能力を削減する組合せが見られる。

北米の両設備ONのOFF比はPV約−231 GW、風力＋64.8 GW、電池inverter＋5.91 GW、電解入力容量＋26.8 GW。アジアと異なり、電解容量・電池容量が減る方向とは限らない。「水素貯蔵が電池・電解設備を必ず代替する」という一般化はしない。

**欧州2030年の読み取り**：両設備ONはタンク約1.32 TWh・洞窟0.124 TWh、圧縮機約24.6・3.42 GW_H2。XE15のタンク約1.19 TWhが中心。洞窟導入率はXE15が87.5%、XE10/XE3がほぼ100%。XEWIには今回入力上の洞窟ポテンシャルがなく、タンク約0.0932 TWhが入る。洞窟最大在庫は設置容量へ達する。年間総入庫はタンク38.5・洞窟5.26 TWh、圧縮電力4.85 TWh/年。OFF比でPV−3.90 GW、風力＋2.47 GW、電池inverter−1.71 GW、電解入力容量＋13.7 GW。2030年の導入開始時点の結果であり、北米・アジア2045年と導入規模を直接順位付けしない。

**確認結果（Codex、2026-11-06）**：アジア24、北米24、欧州12の計60 scenario-yearでLSTのOptimal/Normalを確認。主年の貯蔵3case×3地域についてversion4、365日・年末循環、在庫非負/容量以内、在庫更新式を照合し、最大在庫式残差はすべて0 GWh。日別flowの年合計と、独立に読んだ同symbolの年集計は一致。洞窟設置量は各ノードのポテンシャル上限内、OFF貯蔵容量・圧縮機容量は0。電力1460時刻は6時間、日別量は重み1として処理した。R構文検査、31 PNGの存在・読取、および図の視覚確認を実施。最終作図実行の警告はR起動時の既存C.UTF-8 locale警告4件のみ。

図の対象値・単位・年別状態の確認済み範囲と、モデル全体の科学的検証を区別する。入力ポテンシャルの校正、共有実行時のdirty state/data Gitlink、後処理のシナリオ不一致の根本原因は未確認なので、研究結果全体の状態はpartially_verified。モデル変更・solveは実施していない。図を報告に使う際は暫定入力と有効年の範囲を併記する。

最新code SHA256: `prog/main.R` = `F432E6808EE7E48ACA95DF342875714488BB87B0F5D478B6EF514A8BF5F35BBE`、`prog/system_evolution.R` = `A267A0C02BE06CDD7535E35ADB2AF45C71FB7E64741499FD1863239BE4031F23`。topicsはGit初期化前で、本projectのcommitはない。AIMPSY_AI/AIMPSY_plotの既存変更は保持し、両repoへの変更は加えていない。初回31図の視覚確認用PNGを `work/qa_contact.png` にローカル保管する（06更新前で、追加図10/11は含まない）。
