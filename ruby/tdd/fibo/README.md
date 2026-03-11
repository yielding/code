# 현재 사용법

1. `bundler 2.5.16`을 맞춘다.
2. `bundle install`
3. 현재 macOS에서는 `./watch.sh`를 우선 권장한다.
4. 필요하면 `bundle exec guard`도 시도할 수 있다.

- `./watch.sh`
- `bundle exec guard`

# 메모

- 이 폴더의 현재 설정은 Growl이 아니라 macOS 알림센터를 사용한다.
- `.ruby-version`은 `4.0.1`로 맞췄다.
- Hammerspoon이 설치돼 있으면 `test.sh`는 이를 우선 사용해 상단 오버레이를 띄운다.
- `hammerspoon_tdd.lua`는 우측 상단 카드 스타일 오버레이를 그린다.
- 카드에는 앱 이름, 제목, 요약 본문이 보인다.
- 카드는 중립색이고, 성공/실패는 좌측 체크/X 상태 아이콘으로만 구분한다.
- 실패 시에는 첫 번째 RSpec 실패를 짧게 요약해서 본문에 넣는다.
- 최근 알림은 최대 3개까지 우측 상단에 스택된다.
- `TDD_APP_NAME`, `TDD_APP_BUNDLE_ID` 환경변수로 카드의 앱 이름과 아이콘을 바꿀 수 있다.
- `Guardfile`은 `terminal-notifier`를 쓰도록 되어 있지만, 이 머신에서는 `terminal-notifier`가 실행 중 비정상 종료됐다.
- `test.sh`는 Hammerspoon을 먼저 시도하고, 없으면 macOS 기본 `osascript` 알림으로 폴백한다.
- `USE_TERMINAL_NOTIFIER=1 ./test.sh`처럼 실행하면 `terminal-notifier`도 선택적으로 쓸 수 있다.
- `test.sh`는 PATH에 있는 시스템 Ruby 대신, `bundler 2.5.16`을 제공하는 `bundle`을 자동으로 찾는다.
- Ruby 4.0에서 `guard`를 돌리려면 빠진 기본 라이브러리 gem이 있어 `logger`, `ostruct`를 `Gemfile`에 추가했다.
- 따라서 "저장할 때 테스트 자동 실행 + 결과 알림"은 현재 macOS에서도 가능하다.
- 다만 Growl의 옛날 빨강/파랑 막대 같은 커스텀 시각 스타일은 macOS 기본 알림으로는 동일하게 재현되지 않는다.
