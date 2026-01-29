# Bangumi-Data 按需获取优化

## 目标

将 bangumi-data 数据获取从全量下载改为按年/月按需获取，减少带宽消耗和响应时间。

## 当前问题

- `Client.fetchAll` 下载完整 `dist/data.json` (~10MB+，包含 1943 年至今所有数据)
- 每次查询特定季度都要下载全量数据后在内存中过滤

## 优化方案

利用 bangumi-data 仓库的 `data/items/{year}/{month}.json` 结构，只获取对应季度的 3 个月数据。

URL 格式: `https://raw.githubusercontent.com/bangumi-data/bangumi-data/master/data/items/{year}/{month:02d}.json`

## 需要修改的文件

### 1. `moe-bangumi.cabal`
添加依赖:
- `async` - 并发请求
- `http-types` - HTTP 状态码判断

### 2. `src/core/Moe/Model/Bangumi/Types.hs`
添加 `seasonToMonths` 函数:
```haskell
seasonToMonths :: Season -> [Int]
seasonToMonths Winter = [1, 2, 3]
seasonToMonths Spring = [4, 5, 6]
seasonToMonths Summer = [7, 8, 9]
seasonToMonths Fall   = [10, 11, 12]
```

### 3. `src/libs/bangumi-data/src/BangumiData/Client.hs`
添加按月获取函数:
- `fetchByMonth :: Word16 -> Int -> IO (Either String [BangumiDataItem])` - 获取单月数据
- `fetchByMonths :: Word16 -> [Int] -> IO (Either String [BangumiDataItem])` - 并发获取多月数据

关键实现:
- 月份 JSON 是数组格式，直接解析为 `[BangumiDataItem]`
- 404 返回空列表 `Right []`
- 使用共享 Manager 并发请求

### 4. `src/core/Moe/Model/BangumiData/Interpreter.hs`
修改 `runBangumiDataHttp`:
- 从 `AnimeSeason` 提取年份和季度
- 调用 `seasonToMonths` 获取对应月份
- 使用 `Client.fetchByMonths` 获取数据
- 保留 `matchesAnimeSeason` 过滤（处理跨季度番剧）

## 实现步骤

1. 修改 `moe-bangumi.cabal` 添加 `async` 和 `http-types` 依赖
2. 在 `Moe.Model.Bangumi.Types` 添加 `seasonToMonths` 并导出
3. 在 `BangumiData.Client` 添加 `fetchByMonth` 和 `fetchByMonths`
4. 修改 `Interpreter` 使用新的获取方式
5. 编译验证

## 验证方式

```bash
cabal build
cabal run moe-bangumi -- sync --season "2024 Winter"
```

预期: 只下载 2024 年 1、2、3 月的数据，而不是全量数据。
