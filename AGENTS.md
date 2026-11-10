# AIMPSYプロット

共通描画・報告や論文の図表を所有する。トピック専用分析との責任境界は親READMEを参照する。

- 対象projectはトピックの指定を優先し、未指定の既存汎用作図は `master/` を使う。
- 作図時は [.agents/skills/aimpsy-png-output/SKILL.md](.agents/skills/aimpsy-png-output/SKILL.md) を適用する。出力はPNGを既定とし、追加形式・集計表は依頼範囲に合わせる。
- 入力・集計・単位・対象runを変更するとき、または作図を実行するときは、入力pathと完全なproducer token、PSM/AIM-Technologyの区別、symbol/dimension、scenario・年/timeslice・地域、元単位・表示単位・換算を既存の分析記録で確認する。表示だけの修正は変更範囲の確認に絞る。
- 実行前にscript・対象図表・想定出力数・新規出力先を定める。入力・symbol・単位・出力先の不備は書込み前に解消する。Rの `source()` は実行として扱う。
- 変更したRファイルはstatic parseし、実行検証は承認範囲に限定する。対象範囲・数値の照合と代表図の視覚確認を記録する。
- 大規模GDX処理・bulk plot・長時間計算・規模不明の処理、既存data/outputの上書き・削除・移動・改名、モデル出力のcopy/link/convert/publishは、command・input/output・時間/規模・上書き有無を示して明示承認を得る。
- 結果はplot commit/dirty state、入力run、script/設定、生成pathと検証状態を対応付ける。tokenはproducerから完全な値を取得し、複数モデルの入力は別々に記録する。
- 完了時に `git status --short` と全差分を確認し、変更範囲・検証・残る問題を報告する。
