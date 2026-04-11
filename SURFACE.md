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