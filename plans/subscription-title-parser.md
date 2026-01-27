# 实现计划：引入 SubscriptionTitle 标题解析器

## 需求重述

将备份项目 `moe-bangumi-backup` 中的 `SubscriptionTitle.hs` 模块引入当前项目，用于解析 RSS 订阅标题，提取集数、字幕组和字幕语言信息。

## 现有分析

### 备份项目结构
- `src/Moe/Parsing/SubscriptionTitle.hs` - 主解析逻辑
- `src/Moe/Group.hs` - 字幕组规范化
- `src/Moe/Parsing/Types.hs` - ParseResult 类型
- `src/Moe/Subtitle.hs` - Subtitle 枚举
- `test/Moe/Parsing/SubscriptionTitleSpec.hs` - 测试文件
- `test/Moe/Parsing/test_cases.json` - 测试数据

### 当前项目
- 已有 `SubtitleLang` 类型（使用 `JPN`，备份使用 `JAP`）
- 使用 Relude 作为 Prelude
- 无注释代码风格

### 关键差异
| 项目 | 当前项目 | 备份项目 |
|------|---------|---------|
| 日语字幕 | `JPN` | `JAP` |
| 模块路径 | `Bangumi.Internal.*` | `Moe.*` |
| 外部依赖 | 无 regex | 需要 `regex-tdfa` |

## 实现阶段

### Phase 1: 添加依赖
- 在 `moe-bangumi.cabal` 中添加 `regex-tdfa` 和 `aeson` 依赖

### Phase 2: 创建核心模块
1. `src/Bangumi/Internal/Group.hs` - 字幕组类型和规范化
2. `src/Bangumi/Internal/ParseResult.hs` - 解析结果类型
3. `src/Bangumi/Internal/TitleParser.hs` - 标题解析逻辑（适配 JPN）

### Phase 3: 导出公共 API
- 更新 `src/Bangumi/File.hs` 或创建新的 `src/Bangumi/Parser.hs` 导出解析功能

### Phase 4: 测试
1. 复制并适配测试文件
2. 更新测试用例 JSON（JAP -> JPN）
3. 集成到 test suite

## 文件变更清单

### 新建文件
- `src/Bangumi/Internal/Group.hs`
- `src/Bangumi/Internal/ParseResult.hs`
- `src/Bangumi/Internal/TitleParser.hs`
- `src/Bangumi/Parser.hs`
- `test/Bangumi/TitleParserTest.hs`
- `test/Bangumi/test_cases.json`

### 修改文件
- `moe-bangumi.cabal` - 添加依赖和模块
- `test/Main.hs` - 集成新测试

## 依赖关系
```
TitleParser.hs
├── Group.hs
├── ParseResult.hs
└── Subtitle.hs (已存在)
```

## 风险评估

| 风险 | 级别 | 缓解措施 |
|------|------|---------|
| regex-tdfa 兼容性 | 低 | 标准库，已在备份项目验证 |
| JAP/JPN 命名不一致 | 低 | 统一使用当前项目的 JPN |
| 测试数据格式 | 低 | 适配 JSON 字段名 |

## 复杂度评估

**中等** - 主要是代码迁移和适配工作，逻辑已经过验证。

## 确认问题

1. 是否将解析模块导出为独立的 `Bangumi.Parser` 模块，还是合并到现有的 `Bangumi.File`？
2. 字幕组规范化列表是否需要调整？
