# SURFACE.md — Публичные контракты Shaoline

## FROZEN Items (стабильные контракты)

### shaoline-segment-project-name
- **Stability**: [FROZEN]
- **Spec**: Отображает текущий проект (project.el / projectile)
- **Proof**: `make test-core` — тест segment-project-name возвращает имя проекта
- **Invariant**: INV-Determinism

### shaoline-segment-gptel-model
- **Stability**: [FROZEN]
- **Spec**: Отображает текущую AI модель (gptel или agent-shell)
- **Proof**: `make test-core` — тест segment-gptel-model возвращает строку модели
- **Invariant**: INV-Determinism

### shaoline-project-face
- **Stability**: [FROZEN]
- **Spec**: Визуальный стиль для имени проекта (голубой #5e9fff, жирный)
- **Proof**: Тест проверяет color и weight face
- **Invariant**: INV-Compat-Policy

### shaoline-gptel-face
- **Stability**: [FROZEN]
- **Spec**: Визуальный стиль для модели AI (фиолетовый #b259b6)
- **Proof**: Тест проверяет color face
- **Invariant**: INV-Compat-Policy

### shaoline--stable-cached-call
- **Stability**: [FROZEN]
- **Spec**: Кеширование сегментов с TTL до смены директории
- **Proof**: `make test-cache` — тест кеша
- **Invariant**: INV-Determinism, INV-Core-IO-Boundary

### shaoline-compose
- **Stability**: [FROZEN]
- **Spec**: Композиция сегментов в строку mode-line (pure function); без явного WIDTH использует ширину echo-area и оставляет один столбец справа для однострочного вывода
- **Proof**: `make test-core` — тесты `shaoline-compose-output-is-string`, `shaoline-compose-uses-echo-area-width`, `shaoline-layout-reserves-final-column`
- **Invariant**: INV-Pure-Segments, INV-Determinism

### shaoline-define-segment
- **Stability**: [FROZEN]
- **Spec**: Макрос регистрации сегмента в реестре
- **Proof**: `make test-core` — сегменты доступны через shaoline--segment-registry
- **Invariant**: INV-Pure-Segments

---

## FLUID Items (изменяемые контракты)

### shaoline-segment-echo-message
- **Stability**: [FLUID]
- **Spec**: Сообщение из echo-area
- **Proof**: -

### shaoline-segment-input-method
- **Stability**: [FLUID]
- **Spec**: Индикатор метода ввода
- **Proof**: -

### shaoline-segment-time
- **Stability**: [FLUID]
- **Spec**: Текущее время
- **Proof**: -

### shaoline-segment-battery
- **Stability**: [FLUID]
- **Spec**: Уровень заряда батареи
- **Proof**: -

---

## Operations (операции)

###Активация mode
- **Spec**: `(shaoline-mode 1)` активирует mode-line
- **Success**: mode-line отображает сегменты
- **Error**: Ошибка если Emacs < 27.1

###Конфигурация
- **Spec**: Настройка `shaoline-segments` через customize
- **Success**: Сегменты перестраиваются

###Обновление
- **Spec**: `shaoline-update` принудительное обновление
- **Success**: Все сегменты пересчитываются

###Стратегия yin/yang
- **Spec**: `(shaoline-switch-to-yin)`, `(shaoline-switch-to-yang)`, `(shaoline-switch-to-adaptive)`
- **Success**: Поведение mode-line меняется
- **Invariant**: INV-Determinism

###Visibility control
- **Spec**: `shaoline--should-display-p` проверяет условия отображения
- **Success**: mode-line виден/скрыт в зависимости от состояния
- **Invariant**: INV-Determinism

---

## Proofs (тесты)

```bash
# Все тесты
make test

# Тест core (включает segment tests)
make test-core

# Тест кеширования
make test-cache
```

### Vertical E2E Scenario (HDS)

```
Scenario: Активация и отображение mode-line с проектом
1. Запустить Emacs с shaoline
2. Открыть буфер в проекте (~/Code/shaoline)
3. Включить (shaoline-mode 1)
4. Проверить: mode-line содержит имя проекта
5. Проверить: shaoline-project-face применён (color #5e9fff, weight bold)
6. Сменить директорию на ~/Code/pro
7. Проверить: кеш сброшен, отображается новый проект
```

---

Last Updated: 2026-04-12