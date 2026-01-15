# CLAUDE.ja.md

このファイルは、このリポジトリでコードを扱う際のClaude Code (claude.ai/code)へのガイダンスを提供します。

## プロジェクト概要

これは、tmuxで動作するAIコーディングエージェントとEmacsを橋渡しするEmacs拡張です。tmuxセッションの自動監視によりClaude Codeが入力プロンプト待ちの状態を検出し、EmacsからAIエージェントへのテキスト送信を支援します。

## 開発セットアップ

これはEmacs拡張プロジェクトのため、開発には以下が含まれます：
- Emacs Lispコード（`.el`ファイル）の作成
- Emacs内でのテスト
- tmux統合のためのシェルスクリプトやPythonの実装（可能性あり）

## アーキテクチャ考慮事項

このブリッジを実装する際は、以下を考慮してください：
1. **Emacs側の実装**: Emacsユーザー向けのコマンド、モード、UI要素
2. **tmux通信**: tmuxセッションへのコマンド送信と出力受信の方法
3. **AIエージェントプロトコル**: EmacsとAIエージェント間のデータ交換フォーマット
4. **非同期処理**: AIエージェントとの通信中もEmacs操作を応答性を保つ

## 現在の状況

- リポジトリは`prototyping-1`ブランチにあります
- メインの実装は`emacs-ai-agent-bridge.el`にあります
- 実装済みの核心機能：
  - tmuxセッションの自動監視（5秒間隔）
  - 変更されないコンテンツに基づくプロンプト検出（パターンマッチングなし）
  - プロンプト検出時の自動バッファ表示（フォーカスは現在のバッファに維持）
  - プロンプト検出ごとに1回のみバッファを表示
  - 自動C-m（Enterキー）付きの範囲送信機能

## 実装された機能

### 1. tmux監視
- **関数**: `emacs-ai-agent-bridge-start-monitoring` - 5秒ごとのtmuxセッション監視を開始
- **関数**: `emacs-ai-agent-bridge-stop-monitoring` - 監視タイマーを停止
- **関数**: `emacs-ai-agent-bridge-monitor-status` - 現在の監視状態を表示

### 2. プロンプト検出
システムは、tmuxコンソールの内容がチェック間で変更されていないかを監視することで、AIエージェントが入力待ちの状態を検出します。5秒間内容が変化しない場合、エージェントがプロンプト待ちの状態と判定します。
- 検出は、パターンマッチングではなく、コンテンツの変化のみに基づく
- *ai*バッファは、プロンプトが最初に検出されたときのみ表示
- バッファが既に表示されている場合は再利用され、コンテンツのみが更新される
- フォーカスは現在の作業バッファに維持される

### 3. テキスト送信
- **関数**: `send-to-ai` - 選択範囲を自動C-m（Enterキー）付きでtmuxに送信
- **関数**: `emacs-ai-agent-bridge-send-region-to-tmux` - 核心実装
- テキストはtmuxに送信され、その後C-mでコマンドを実行

### 4. 設定変数
- `emacs-ai-agent-bridge-tmux-session` - 監視するtmuxセッション（nilで自動検出）
- `emacs-ai-agent-bridge-tmux-pane` - ペインID（デフォルト: "0"）
- `emacs-ai-agent-bridge-monitor-interval` - チェック間隔（秒）（デフォルト: 2）
- `emacs-ai-agent-bridge-scrollback-lines` - tmux履歴からキャプチャするスクロールバック行数（デフォルト: 3000）

## ファイル構造

- `emacs-ai-agent-bridge.el` - 全ての核心機能を含むメインパッケージファイル
- `CLAUDE.md` - このドキュメントファイル
- `README.md` - ユーザー向けドキュメント
- `LICENSE` - GPL v3ライセンス

## Issue #8の修正内容

### 問題
Claude Code 2.0.31へのアップデート後、UIの変更により以下の問題が発生：
- 数字キー（1、2、3など）による選択肢の選択ができなくなった
- 上下矢印キーの動作が反転（上キーで最上位から最下位へジャンプし、3→2→1と上方向に循環）

### 修正内容
`emacs-ai-agent-bridge-select-option`関数（emacs-ai-agent-bridge.el:176-186）を修正：
- Claude Code 2.0.31以降では、数字キーを直接tmuxに送信するように変更
- 以前の実装では矢印キーとEnterキーを使った複雑な選択プロセスだったが、新バージョンでは数字キーの直接送信でシンプルに実装

**関連関数**:
- `emacs-ai-select-option-1` から `emacs-ai-select-option-5` - それぞれ対応する選択肢を直接選択
- *ai*バッファで1-5キーを押すことで、Claude Codeの選択肢を直接選択可能

## Issue #11の実装内容

### 機能リクエスト
tmuxで画面外にスクロールしてしまった過去のメッセージを*ai*バッファに表示できるようにする。

### 問題
以前は、*ai*バッファはtmuxウィンドウに表示されている内容のみをキャプチャしていたため、画面外にスクロールアウトしたメッセージにアクセスできませんでした。ユーザーは過去の会話を包括的にレビューしたいと要望していました。

### 実装内容
`emacs-ai-agent-bridge-capture-tmux-pane`関数（emacs-ai-agent-bridge.el:269-283）を修正：
- 新しい設定変数 `emacs-ai-agent-bridge-scrollback-lines` を追加（デフォルト: 3000）
- `tmux capture-pane` コマンドにスクロールバック履歴用の `-S` オプションを追加
- `emacs-ai-agent-bridge-scrollback-lines` が 0 より大きい場合、その行数分のスクロールバック履歴をキャプチャ
- 0 に設定した場合、表示されているコンテンツのみをキャプチャ（以前の動作）

**設定方法**:
- `emacs-ai-agent-bridge-scrollback-lines` を設定することでスクロールバック行数をカスタマイズ可能
- 例: `(setq emacs-ai-agent-bridge-scrollback-lines 5000)` で5000行の履歴をキャプチャ

## Issue #12の修正内容

### 問題
`emacs-ai-agent-bridge-input-mode`を有効にした後、RETキーのバインディングが`emacs-ai-agent-bridge-smart-input-return`で上書きされ、@ai行でない場合は単に`newline`を呼び出すだけになっていました。これにより、モード固有のRETキーの動作が正しく機能しなくなりました。例えば：
- markdown-modeのテーブル自動整列機能
- org-modeのリスト続行やその他のorg-return動作
- その他のメジャーモードのカスタムRETキーハンドラ

### 修正内容
`emacs-ai-agent-bridge-smart-input-return`関数をモード非依存にするよう修正（emacs-ai-agent-bridge.el:612-638）：
- `emacs-ai-agent-bridge-get-original-return-command`関数を追加：マイナーモードを一時的に無効化して元のRETキーバインディングを取得
- `emacs-ai-agent-bridge-call-original-return`関数を追加：`call-interactively`を使って元のモードのRETハンドラを呼び出す
- `emacs-ai-agent-bridge-smart-input-return`を修正：@ai行でない場合、単に`newline`ではなく元のRETハンドラを呼び出す

**技術的な詳細**:
- `let`バインディングを使って`emacs-ai-agent-bridge-input-mode`を一時的にnilに設定
- `(key-binding (kbd "RET"))`で元のキーバインディングを取得
- `call-interactively`で元のコマンドを呼び出し、モード固有の動作をすべて保持
- 元のコマンドが見つからない場合は`newline`にフォールバック

**利点**:
- どのメジャーモードでも変更なしで動作
- モード固有のRETキー動作（テーブル整列、リスト続行など）をすべて保持
- markdown-modeやorg-modeなどの特定モードに依存しない

### 実装した変更

1. **新しいヘルパー関数** (emacs-ai-agent-bridge.el:612-625):
   - `emacs-ai-agent-bridge-get-original-return-command`: マイナーモードを一時的に無効化して元のRETキーバインディングを取得
   - `emacs-ai-agent-bridge-call-original-return`: 元のモードのRETハンドラを`call-interactively`で呼び出す

2. **修正された関数** (emacs-ai-agent-bridge.el:627-638):
   - `emacs-ai-agent-bridge-smart-input-return`: 単に`newline`を呼ぶ代わりに、`emacs-ai-agent-bridge-call-original-return`を呼び出す

3. **ドキュメント更新**:
   - CLAUDE.mdにIssue #12の修正内容を追加
   - バージョンを0.2.0から0.3.0にバンプ

### 動作確認

- ✓ text-mode、org-mode、その他のモードで元のRETハンドラが正しく呼ばれる
- ✓ org-modeでは`org-return`が保持される（リスト続行などの機能）
- ✓ markdown-modeのテーブル自動整列機能も動作する（モード非依存のため）
- ✓ @ai行の処理は引き続き正常に動作

### 技術的な特長

- **完全にモード非依存**: markdown-mode、org-mode、その他どのモードでも動作
- **モード固有機能を保持**: 各モードのRETキーの特殊機能がそのまま使える
- **保守性が高い**: 新しいモードが追加されても変更不要

