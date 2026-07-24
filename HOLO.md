# HOLO.md — Голограмма Shaoline

## Stage
**RealityCheck** — Проект стабилен, используется в production (Emacs mode-line/echo-area).

---

## Purpose

Shaoline — функциональный минималистичный mode-line/echo-area для Emacs.
Следует принципам Dao (道): composition without side effects, adaptation without force.
Обеспечивает отображение имени проекта, AI модели, и системной информации.

---

## Инварианты (базовые 7 + уточнения)

- **INV-Core-IO-Boundary (must)**: Core не зависит от IO; эффекты изолированы в shaoline-effects.el
- **INV-Determinism (must)**: Одинаковые входы/директория → одинаковые выходы (имя проекта)
- **INV-Canonical-Roundtrip (must)**: encode∘decode = id (где применимо, напр. cached value)
- **INV-Compat-Policy (must)**: Frozen эволюционирует аддитивно или через v2
- **INV-Traceability (must)**: Change Gate для каждого изменения
- **INV-Surface-First (must)**: Публичный смысл сначала фиксируется в SURFACE.md
- **INV-Single-Intent (must)**: Один PR — одна доминирующая цель

### Уточнения Shaoline
- **INV-Pure-Segments**: Каждый segment регистрируется через `shaoline-define-segment`
- **INV-TTL-Cache**: Тяжёлые сегменты кешируются с TTL (shaoline-cache-ttl)
- **INV-Stable-Cache**: Кеш сбрасывается при смене директории
- **INV-Composition-Purity**: `shaoline-compose` — чистая функция (без side effects)
- **INV-Stratety-Adaptive**: Стратегия влияет на поведение, но не на core логику

---

## Decisions

### [Draft] Стратегия адаптации
- **Choice**: yin (пассивный) / yang (активный) / adaptive
- **Status**: Draft
- **Exit**: Проверить в реальных сценариях использования
- **Proof**: Тесты strategy показывают корректное поведение

### [Frozen] Отображение project name
- **Choice**: Использовать project.el, fallback на projectile, затем default-directory
- **Status**: Frozen
- **Exit**: N/A (стабильный контракт)
- **Proof**: `make test-core` — тесты segment-project-name

### [Frozen] Отображение AI модели
- **Choice**: gptel-model если доступен; agent-shell model если в его буфере
- **Status**: Frozen
- **Exit**: N/A (стабильный контракт)
- **Proof**: `make test-core` — тесты segment-gptel-model

### [Frozen] Pure composition
- **Choice**: shaoline-compose возвращает строку без side effects
- **Status**: Frozen
- **Exit**: N/A (стабильный контракт)
- **Proof**: `make test-core` — тесты shaoline-compose-*

### [Draft] EXWM tray support
- **Choice**: Автоопределение ширины tray через exwm-systemtray--*
- **Status**: Draft
- **Exit**: Работает в EXWM без ручной настройки
- **Proof**: Визуальная проверка в EXWM

### [Frozen] Защита echo-area от внешних `(message nil)`
- **Choice**: В Emacs 30+ ставим `clear-message-function` в `shaoline--clear-message-guard`,
  который возвращает `dont-clear-message`, если в echo лежит наша строка
  (свойство `shaoline-origin`). На более старых Emacs работает advice-обёртка
  `shaoline--advice-preserve-empty-message` (depth 100, блокирует оригинальный
  `message` для пустого/nil формата).
- **Yielding rule (minibuffer)**: в ОБОИХ механизмах — guard и advice — мы
  пропускаем очистку, когда **активен минибуфер** — проверка
  `(active-minibuffer-window)`, а не `(minibufferp)`. В minibuffer echo-областью
  владеет вертикальный completion UI (Vertico/Consult/Corfu), и блокировка
  `(message nil)` приводит к тому, что список кандидатов рендерится пустым
  до первого ввода символа. В обычных буферах guard по-прежнему защищает
  нашу модель-лайн от внешних атак.
- **Почему `active-minibuffer-window`, а не `minibufferp`**: `(message ...)`,
  вызванный из хука в исходном буфере (vertico/consult
  `minibuffer-setup-hook`, shaoline-таймер, любой сторонний код), выполняется
  в контексте *исходного* буфера — `minibufferp` = nil, даже когда активен
  минибуфер. Это и был баг, ломавший `M-x` в yang/yin-стратегиях: advice
  считал echo свободной и подавлял clear, старая shaoline-строка оставалась
  в echo-области, и приглашение `M-x` становилось невидимым до первого
  символа (тогда происходит minibuffer redraw и сбрасывает echo через
  другой путь). То же касается `shaoline--echo-area-busy-p` — таймер
  shaoline, срабатывающий в исходном буфере при активном минибуфере,
  должен видеть echo как занятую, иначе он перерисует shaoline-строку
  поверх `M-x ` (это «подмигивания» в adaptive-режиме).
- **Status**: Frozen
- **Exit**: N/A (стабильный контракт: наш контент в echo не стирается чужим кодом,
  но в minibuffer UI completion имеет приоритет)
- **Proof**: `make test-effects`:
  - `shaoline-clear-message-guard-*` (5 тестов, поведение guard-а)
  - `shaoline-preserve-empty-message-allows-in-minibuffer` (advice пропускает в minibuffer)
  - `shaoline-preserve-empty-message-allows-when-minibuffer-active-from-regular-buffer`
    (advice пропускает `(message nil)` из исходного буфера при активном
    минибуфере — регрессионный тест pro-nix)
  - `shaoline-echo-area-busy-p-active-minibuffer-from-regular-buffer`
    (busy-p = t при активном минибуфере независимо от current-buffer —
    регрессионный тест pro-nix)
  - `shaoline-preserve-empty-message-blocks-in-regular-buffer` (advice блокирует в обычном буфере)
  - `shaoline-preserve-empty-message-allows-empty-in-minibuffer` (пустые строки в minibuffer)

### [Frozen] Compose-cache key включает динамику
- **Choice**: `shaoline--compose-cache-key` теперь включает `line-number-at-pos`,
  `current-column` и `shaoline--current-keys` помимо `buffer/width/right-margin`.
  Без этого кеш возвращал устаревший composed-стринг, и модель-лайн отставал
  от курсора на TTL окно (0.5–2 с).
- **Status**: Frozen
- **Proof**: `make test-core` — `shaoline-compose-cache-key-includes-line-and-keys`

### [Frozen] Echo-area width and visibility snapshot
- **Choice**: Implicit composition width follows `window-body-width` of the echo-area window, reserves the final column for single-row output, and deduplicates only while the tagged Shaoline content remains visible in `current-message`.
- **Status**: Frozen
- **Exit**: N/A
- **Proof**: `make test-core` — `shaoline-compose-uses-echo-area-width`, `shaoline-layout-reserves-final-column`; `make test-effects` — `shaoline-display-cached-redraws-after-echo-was-cleared`

- **Choice**: `shaoline--reassert-yang-visibility` больше не дросселирует по
  `since-last > min-int` от now; вместо этого при реальном re-assert обновляет
  `shaoline--last-display-time` (= текущее время). Это устраняет баг рассинхронизации,
  при котором заблокированный `preserve-empty-message` сбрасывал throttle-anchor,
  и модель-лайн «залипал» на 1–2 с после внешнего `(message nil)`.
- **Status**: Frozen
- **Proof**: `make test-effects` — `shaoline-reassert-updates-last-display-time` и
  `shaoline-reassert-skipped-when-our-line-already-in-echo`

---

## Критерии голографичности

- [x] HOLO.md существует (Stage, Purpose, Invariants, Decisions)
- [x] SURFACE.md существует (FROZEN items + Proof)
- [x] ≥1 FROZEN item с Proof (7 items)
- [x] E2E тест (vertical scenario в SURFACE.md)
- [x] Инварианты покрыты E2E тестами
- [ ] `make test` проходят (nix сломан, использовать pro)

---

Last Updated: 2026-06-10