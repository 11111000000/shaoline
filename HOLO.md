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

### [Draft] EXWM tray support
- **Choice**: Автоопределение ширины tray через exwm-systemtray--*
- **Status**: Draft
- **Exit**: Работает в EXWM без ручной настройки
- **Proof**: Визуальная проверка в EXWM

---

## Критерии голографичности

- [x] HOLO.md существует (Stage, Purpose, Invariants, Decisions)
- [x] SURFACE.md существует (FROZEN items + Proof)
- [x] ≥1 FROZEN item с Proof
- [x] E2E тест (vertical scenario в SURFACE.md)
- [ ] `make test` проходят

---

Last Updated: 2026-04-12