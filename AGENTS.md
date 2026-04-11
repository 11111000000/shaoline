# AGENTS.md — Shaoline Development (HDS Edition)

**Проект**: Shaoline (Emacs mode-line/echo-area)
**Стадия**: RealityCheck (HDS)
**Обновлено**: 2026-04-12

---

## 1. HDS-ритуал (обязательно)

Для каждого изменения:

```
1. Surface → Проверить/создать SURFACE.md
2. Proof → Добавить тесты/проверки
3. Code → Минимальная реализация под Proof
4. Verify → make test && make byte-compile
5. Update HOLO → Обновить HOLO.md если изменился смысл
```

---

## 2. Change Gate (обязательно)

Каждое изменение должно содержать:

- **Intent**: <одна фраза>
- **Pressure**: Bug | Feature | Debt | Ops
- **Surface impact**: touches: <item> [FROZEN/FLUID]
- **Proof**: tests: <путь/команда>

---

## 3. Инварианты (must)

- INV-Core-IO-Boundary: Core не зависит от IO
- INV-Determinism: одинаковые входы → одинаковые выходы
- INV-Canonical-Roundtrip: encode∘decode = id (где применимо)
- INV-Compat-Policy: Frozen → аддитивно или v2
- INV-Traceability: Change Gate для каждого изменения
- INV-Surface-First: SURFACE.md → код
- INV-Single-Intent: один PR — одна цель

---

## 4. Структура проекта

```
shaoline/
├── lisp/              # Основной код (shaoline.el, shaoline-*.el)
├── test/              # ERT тесты
├── scripts/           # Инструменты lint/format
├── docs/              # Документация
├── recipes/           # Конфигурации для пакетных менеджеров
├── AGENTS.md          # Этот файл
├── hds-llm-seed-ru.md # HDS спецификация
├── Makefile           # Команды сборки
└── README.org         # Основная документация
```

---

## 5. Команды проверки

```bash
# Все тесты
make test

# Отдельные тесты
make test-core
make test-cache
make test-effects
make test-strategy

# Компиляция
make byte-compile

# QA (clean + build + test + lint)
make qa

# Форматирование
make autofix

# Только lint
make lint
```

---

## 6. HDS Bootstrap (если нужно создать Surface/HOLO)

Из hds-llm-seed-ru.md:

1. **Discovery**: домен (1 фраза), главный пользователь путь, внешние формы
2. **Surface**: создать SURFACE.md с ≥1 [FROZEN] + Proof
3. **Proof**: добавить тесты для [FROZEN] + 1 вертикальный сценарий
4. **HOLO**: Stage=RealityCheck, Purpose, инварианты, решения
5. **Verify**: прогнать `make test`
6. **Code**: минимальная реализация под Proof

---

## 7. E2E тестирование и сбор логов с pro

### Запуск E2E тестов

```bash
# Из директории pro
cd ~/pro

# Запуск тестов (по аналогии с UHA-6.0)
./gradlew test

# Или через Emacs
emacs --batch -Q -L . -l test/run.el -f ert-run-tests-batch-and-exit
```

### Сбор логов с pro

```bash
# Основной лог Emacs
tail -f ~/pro/emacs.log

# Лог из agent-shell
# В буфере *agent-shell* нажми C-c C-l для просмотра логов

# Отладочный вывод в pro
# Открий буфер *Messages* в Emacs (или в agent-shell)
```

### Debug скрипт pro

```bash
# Используй встроенный скрипт отладки
~/pro/bin/opencode-debug.sh

# Это покажет путь к последнему логу headless сессии
```

### Для e2e тестирования изменений в shaoline из pro

1. Измени код в `~/Code/shaoline/lisp/`
2. Перекомпилируй: `cd ~/Code/shaoline && make byte-compile`
3. Перезагрузи pro (или перезапусти Emacs)
4. Проверь визуально в mode-line

---

## 8. Зависимости

- Emacs 29+
- ERT (встроен)
- project.el или projectile (опционально)
- gptel (опционально)
- agent-shell (опционально)

---

## 9. frozen items (текущие)

- shaoline-project-face: [FROZEN] — визуальный стиль project name
- shaoline-gptel-face: [FROZEN] — визуальный стиль модели AI
- shaoline-segment-project-name: [FROZEN] — отображение имени проекта
- shaoline-segment-gptel-model: [FROZEN] — отображение модели AI/agent-shell