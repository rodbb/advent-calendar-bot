# advent-calendar-bot
Qiita アドベントカレンダーの更新を Mattermost に投稿するボット

## Usage

```
Command to post Qiita Advent Calendar updates to Mattermost

Usage: advent-calendar-bot-exe WEBHOOK_URL FEED_URL

Available options:
  WEBHOOK_URL              Target Mattermost Incoming Webhook URL
  FEED_URL                 Target Qiita Advent Calendar Feed URL
  -h,--help                Show this help text
```

## CONTRIBUTING

### Task List

- [x] Advent Calendar に投稿された記事一覧を取得する
    - RSS Feed があるのでそれをダウンロードすればよい
    - RSS Feed の URL は実行時に指定できるようにする
- [ ] 前回実行時から新しく投稿されたもの（新着記事一覧）を抜き出す
  - [x] 前回実行時の情報を記録しておく
    - ~~前回取得した Feed を保存しておく？~~
    - 前回取得した Feed の `updated` だけを保存しておく？
    - ~~前回取得した日時を保存しておく？~~
  - [ ] 実行時に前回実行時情報を読み込む
  - [ ] 新着記事だけを抜き出す
- [ ] 新着記事一覧から投稿用メッセージの文面を作る
- [x] Mattermost にメッセージを投稿する
    - ~~Host や Channel、アクセストークンは実行時に指定できるようにする~~
    - Incoming Webhook URL は実行時に指定できるようにする

### Testing

* ローカル環境に Docker で Mattermost サーバーを立てて行う
  * [Mattermost docs](https://docs.mattermost.com/install/setting-up-local-machine-using-docker.html)

### git commnet prefix

* feat: A new feature
* fix: A bug fix
* docs: Documentation only changes
* style: Changes that do not affect the meaning of the code (white-space, formatting, missing semi-colons, etc)
* refactor: A code change that neither fixes a bug nor adds a feature
* perf: A code change that improves performance
* test: Adding missing or correcting existing tests
* chore: Changes to the build process or auxiliary tools and libraries such as documentation generation
