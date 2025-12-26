import * as React from "react";
import { cn } from "@/lib/utils";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { Label } from "@/components/ui/label";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import {
  IconDownload,
  IconFilter,
  IconPlugConnected,
  IconEye,
  IconEyeOff,
  IconLoader2,
  IconCheck,
  IconX,
  IconDeviceFloppy,
  IconPlus,
  IconTrash,
  IconRegex,
} from "@tabler/icons-react";
import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import {
  getSettingsOptions,
  testDownloaderConnection,
  updateSettingsMutation,
  getSettingsQueryKey,
} from "@/lib/api";

// ============================================================================
// Shared Components
// ============================================================================

function SectionHeader({
  icon,
  title,
  description,
  accent = "chart-1",
}: {
  icon: React.ReactNode;
  title: string;
  description?: string;
  accent?: "chart-1" | "chart-3";
}) {
  return (
    <div className="flex items-center gap-3 pb-2">
      <div
        className={cn(
          "flex size-8 items-center justify-center rounded-lg",
          accent === "chart-1" && "bg-chart-1/10 text-chart-1",
          accent === "chart-3" && "bg-chart-3/10 text-chart-3"
        )}
      >
        {icon}
      </div>
      <div>
        <h2 className="text-sm font-semibold text-foreground">{title}</h2>
        {description && (
          <p className="text-xs text-muted-foreground">{description}</p>
        )}
      </div>
    </div>
  );
}

function FormField({
  label,
  children,
}: {
  label: string;
  children: React.ReactNode;
}) {
  return (
    <div className="space-y-1.5">
      <Label className="text-sm text-muted-foreground">{label}</Label>
      {children}
    </div>
  );
}

// ============================================================================
// Downloader Section
// ============================================================================

type ConnectionStatus = "idle" | "loading" | "success" | "error";

interface DownloaderSectionProps {
  settings?: {
    type?: "qbittorrent";
    url?: string | null;
    username?: string | null;
    password?: string | null;
  };
  onChange: (values: {
    type: "qbittorrent";
    url: string;
    username: string;
    password: string;
  }) => void;
}

function DownloaderSection({ settings, onChange }: DownloaderSectionProps) {
  const [downloaderType, setDownloaderType] = React.useState<"qbittorrent">("qbittorrent");
  const [showPassword, setShowPassword] = React.useState(false);
  const [connectionStatus, setConnectionStatus] = React.useState<ConnectionStatus>("idle");
  const [errorMessage, setErrorMessage] = React.useState<string>("");
  const [url, setUrl] = React.useState("");
  const [username, setUsername] = React.useState("");
  const [password, setPassword] = React.useState("");

  React.useEffect(() => {
    if (settings) {
      if (settings.type) setDownloaderType(settings.type);
      if (settings.url) setUrl(settings.url);
      if (settings.username) setUsername(settings.username);
      if (settings.password) setPassword(settings.password);
    }
  }, [settings]);

  React.useEffect(() => {
    onChange({ type: downloaderType, url, username, password });
  }, [downloaderType, url, username, password, onChange]);

  const handleCheckConnection = async () => {
    setErrorMessage("");

    if (!url || !username || !password) {
      setConnectionStatus("error");
      setErrorMessage("请填写所有必要字段");
      return;
    }

    setConnectionStatus("loading");

    try {
      const { response } = await testDownloaderConnection({
        body: { type: downloaderType, url, username, password },
      });

      if (response.ok) {
        setConnectionStatus("success");
        setErrorMessage("");
      } else {
        const text = await response.text();
        setConnectionStatus("error");
        setErrorMessage(text || "连接失败");
      }
    } catch {
      setConnectionStatus("error");
      setErrorMessage("网络错误，请检查服务器是否运行");
    }
  };

  return (
    <section className="space-y-4">
      <SectionHeader
        icon={<IconDownload className="size-4" />}
        title="下载器配置"
        description="设置 qBittorrent 连接信息"
      />

      <div className="space-y-4 pl-11">
        <FormField label="下载器类型">
          <Select
            value={downloaderType}
            onValueChange={(v) => setDownloaderType(v as "qbittorrent")}
          >
            <SelectTrigger className="w-full">
              <SelectValue />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="qbittorrent">qBittorrent</SelectItem>
            </SelectContent>
          </Select>
        </FormField>

        <FormField label="服务器地址">
          <Input
            type="url"
            placeholder="http://localhost:8080"
            value={url}
            onChange={(e) => setUrl(e.target.value)}
          />
        </FormField>

        <div className="grid gap-4 sm:grid-cols-2">
          <FormField label="用户名">
            <Input
              type="text"
              placeholder="admin"
              value={username}
              onChange={(e) => setUsername(e.target.value)}
            />
          </FormField>

          <FormField label="密码">
            <div className="relative">
              <Input
                type={showPassword ? "text" : "password"}
                placeholder="••••••••"
                value={password}
                onChange={(e) => setPassword(e.target.value)}
                className="pr-10"
              />
              <button
                type="button"
                onClick={() => setShowPassword(!showPassword)}
                className="absolute right-2 top-1/2 -translate-y-1/2 p-1 text-muted-foreground transition-colors hover:text-foreground"
              >
                {showPassword ? <IconEyeOff className="size-4" /> : <IconEye className="size-4" />}
              </button>
            </div>
          </FormField>
        </div>

        <div className="flex items-center gap-3">
          <Button
            type="button"
            variant="outline"
            size="sm"
            onClick={handleCheckConnection}
            disabled={connectionStatus === "loading"}
            className="gap-1.5"
          >
            {connectionStatus === "loading" ? (
              <IconLoader2 className="size-3.5 animate-spin" />
            ) : (
              <IconPlugConnected className="size-3.5" />
            )}
            {connectionStatus === "loading" ? "连接中..." : "测试连接"}
          </Button>

          {connectionStatus === "success" && (
            <span className="flex items-center gap-1.5 text-xs text-chart-5">
              <IconCheck className="size-3.5" />
              连接成功
            </span>
          )}
          {connectionStatus === "error" && (
            <span className="flex items-center gap-1.5 text-xs text-destructive">
              <IconX className="size-3.5" />
              {errorMessage || "连接失败"}
            </span>
          )}
        </div>
      </div>
    </section>
  );
}

// ============================================================================
// Filter Section
// ============================================================================

interface RegexPattern {
  id: string;
  pattern: string;
}

interface FilterSectionProps {
  settings?: {
    global_rss_filters?: string[];
  };
  onChange: (patterns: string[]) => void;
}

function FilterSection({ settings, onChange }: FilterSectionProps) {
  const [patterns, setPatterns] = React.useState<RegexPattern[]>([]);
  const [newPattern, setNewPattern] = React.useState("");
  const [error, setError] = React.useState<string | null>(null);

  React.useEffect(() => {
    if (settings?.global_rss_filters) {
      setPatterns(
        settings.global_rss_filters.map((pattern, index) => ({
          id: index.toString(),
          pattern,
        }))
      );
    }
  }, [settings]);

  React.useEffect(() => {
    onChange(patterns.map((p) => p.pattern));
  }, [patterns, onChange]);

  const handleAddPattern = () => {
    if (!newPattern.trim()) return;

    try {
      new RegExp(newPattern);
    } catch {
      setError("无效的正则表达式");
      return;
    }

    setPatterns([...patterns, { id: Date.now().toString(), pattern: newPattern }]);
    setNewPattern("");
    setError(null);
  };

  const handleRemovePattern = (id: string) => {
    setPatterns(patterns.filter((p) => p.id !== id));
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Enter") {
      e.preventDefault();
      handleAddPattern();
    }
  };

  return (
    <section className="space-y-4">
      <SectionHeader
        icon={<IconFilter className="size-4" />}
        title="全局过滤规则"
        description="使用正则表达式过滤 RSS 订阅内容"
        accent="chart-3"
      />

      <div className="space-y-4 pl-11">
        <div className="space-y-1.5">
          <Label className="text-sm text-muted-foreground">添加规则</Label>
          <div className="flex gap-2">
            <div className="relative flex-1">
              <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                <IconRegex className="size-4" />
              </div>
              <Input
                type="text"
                placeholder="输入正则表达式..."
                value={newPattern}
                onChange={(e) => {
                  setNewPattern(e.target.value);
                  setError(null);
                }}
                onKeyDown={handleKeyDown}
                className={cn(
                  "pl-9 font-mono text-sm",
                  error && "border-destructive"
                )}
              />
            </div>
            <Button
              type="button"
              size="sm"
              onClick={handleAddPattern}
              disabled={!newPattern.trim()}
              className="gap-1"
            >
              <IconPlus className="size-3.5" />
              添加
            </Button>
          </div>
          {error && <p className="text-xs text-destructive">{error}</p>}
        </div>

        {patterns.length > 0 && (
          <div className="space-y-1.5">
            <Label className="text-sm text-muted-foreground">
              已添加 ({patterns.length})
            </Label>
            <div className="space-y-1">
              {patterns.map((pattern, index) => (
                <div
                  key={pattern.id}
                  className="group flex items-center gap-2 rounded-md border border-border/50 bg-muted/30 px-2.5 py-1.5"
                >
                  <span className="flex size-5 shrink-0 items-center justify-center rounded bg-chart-3/10 text-xs text-chart-3">
                    {index + 1}
                  </span>
                  <code className="flex-1 truncate font-mono text-sm text-foreground/80">
                    {pattern.pattern}
                  </code>
                  <Button
                    type="button"
                    variant="ghost"
                    size="icon-sm"
                    onClick={() => handleRemovePattern(pattern.id)}
                    className="size-6 shrink-0 text-muted-foreground hover:bg-destructive/10 hover:text-destructive"
                  >
                    <IconTrash className="size-3.5" />
                  </Button>
                </div>
              ))}
            </div>
          </div>
        )}
      </div>
    </section>
  );
}

// ============================================================================
// Main Settings Page
// ============================================================================

export function SettingsPage() {
  const queryClient = useQueryClient();
  const { data: settings } = useQuery(getSettingsOptions());

  const downloaderRef = React.useRef({
    type: "qbittorrent" as const,
    url: "",
    username: "",
    password: "",
  });
  const filterPatternsRef = React.useRef<string[]>([]);

  const handleDownloaderChange = React.useCallback(
    (values: { type: "qbittorrent"; url: string; username: string; password: string }) => {
      downloaderRef.current = values;
    },
    []
  );

  const handleFilterChange = React.useCallback((patterns: string[]) => {
    filterPatternsRef.current = patterns;
  }, []);

  const { mutate: saveSettings, isPending: isSaving } = useMutation({
    ...updateSettingsMutation(),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: getSettingsQueryKey() });
    },
  });

  const handleSaveAll = () => {
    saveSettings({
      body: {
        downloader: {
          type: downloaderRef.current.type,
          url: downloaderRef.current.url || null,
          username: downloaderRef.current.username || null,
          password: downloaderRef.current.password || null,
        },
        filter: {
          global_rss_filters: filterPatternsRef.current,
        },
      },
    });
  };

  return (
    <div className="min-h-full">
      <div className="px-6 py-8 md:px-8">
        <div className="mx-auto max-w-xl">
          <form
            className="space-y-8"
            onSubmit={(e) => {
              e.preventDefault();
              handleSaveAll();
            }}
          >
            <DownloaderSection settings={settings?.downloader} onChange={handleDownloaderChange} />

            <div className="h-px bg-border" />

            <FilterSection settings={settings?.filter} onChange={handleFilterChange} />

            {/* Save Button */}
            <div className="flex justify-end pt-4">
              <Button type="submit" disabled={isSaving} className="gap-2">
                {isSaving ? (
                  <IconLoader2 className="size-4 animate-spin" />
                ) : (
                  <IconDeviceFloppy className="size-4" />
                )}
                保存
              </Button>
            </div>
          </form>
        </div>
      </div>
    </div>
  );
}
