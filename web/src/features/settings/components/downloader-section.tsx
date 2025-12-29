import * as React from "react";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import {
  IconPlugConnected,
  IconEye,
  IconEyeOff,
  IconLoader2,
  IconCheck,
  IconX,
  IconFolder,
} from "@tabler/icons-react";
import { testDownloaderConnection } from "@/lib/api";
import { FormField } from "./shared";
import { getErrorMessage, type SettingsFormInstance } from "../hooks";
import { downloaderTypes, type DownloaderTypeValue } from "../schema";

type ConnectionStatus = "idle" | "loading" | "success" | "error";

export interface DownloaderSectionProps {
  form: SettingsFormInstance;
}

export function DownloaderSection({ form }: DownloaderSectionProps) {
  // UI-only state (not part of form data)
  const [showPassword, setShowPassword] = React.useState(false);
  const [connectionStatus, setConnectionStatus] = React.useState<ConnectionStatus>("idle");
  const [errorMessage, setErrorMessage] = React.useState<string>("");

  // Test connection using current form values
  const handleCheckConnection = async () => {
    setErrorMessage("");

    const values = form.state.values.downloader;
    const isQB = values.type === "qBittorrent";
    const currentConfig = isQB ? values.configs.qbittorrent : values.configs.transmission;

    // URL is always required
    if (!currentConfig.url) {
      setConnectionStatus("error");
      setErrorMessage("请填写服务器地址");
      return;
    }

    // qBittorrent requires username and password
    if (isQB && (!currentConfig.username || !currentConfig.password)) {
      setConnectionStatus("error");
      setErrorMessage("请填写用户名和密码");
      return;
    }

    setConnectionStatus("loading");

    try {
      const { response } = await testDownloaderConnection({
        body: {
          type: values.type,
          url: currentConfig.url,
          username: currentConfig.username,
          password: currentConfig.password,
        },
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
    <section className="space-y-5">
      <div className="space-y-4">
        {/* Downloader Type */}
        <form.Field name="downloader.type">
          {(field) => (
            <FormField label="下载器类型">
              <Select
                value={field.state.value}
                onValueChange={(v) => {
                  field.handleChange(v as DownloaderTypeValue);
                  // Reset connection status when switching
                  setConnectionStatus("idle");
                  setErrorMessage("");
                }}
              >
                <SelectTrigger className="w-full">
                  <SelectValue />
                </SelectTrigger>
                <SelectContent>
                  {downloaderTypes.map((type) => (
                    <SelectItem key={type} value={type}>
                      {type}
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
            </FormField>
          )}
        </form.Field>

        {/* Subscribe to downloader type to conditionally render configs */}
        <form.Subscribe selector={(state) => state.values.downloader.type}>
          {(downloaderType) => {
            const isQBittorrent = downloaderType === "qBittorrent";
            const isTransmission = downloaderType === "Transmission";

            return (
              <>
                {/* qBittorrent Config */}
                {isQBittorrent && (
                  <>
                    {/* URL */}
                    <form.Field name="downloader.configs.qbittorrent.url">
                      {(field) => {
                        const error = getErrorMessage(field.state.meta.errors[0]);
                        return (
                          <FormField label="服务器地址">
                            <Input
                              type="url"
                              placeholder="http://localhost:8080"
                              value={field.state.value}
                              onChange={(e) => field.handleChange(e.target.value)}
                              onBlur={field.handleBlur}
                            />
                            {error && (
                              <p className="text-xs text-destructive mt-1">{error}</p>
                            )}
                          </FormField>
                        );
                      }}
                    </form.Field>

                    {/* Username & Password */}
                    <div className="grid gap-4 sm:grid-cols-2">
                      <form.Field name="downloader.configs.qbittorrent.username">
                        {(field) => {
                          const error = getErrorMessage(field.state.meta.errors[0]);
                          return (
                            <FormField label="用户名">
                              <Input
                                type="text"
                                placeholder="admin"
                                value={field.state.value}
                                onChange={(e) => field.handleChange(e.target.value)}
                                onBlur={field.handleBlur}
                              />
                              {error && (
                                <p className="text-xs text-destructive mt-1">{error}</p>
                              )}
                            </FormField>
                          );
                        }}
                      </form.Field>

                      <form.Field name="downloader.configs.qbittorrent.password">
                        {(field) => {
                          const error = getErrorMessage(field.state.meta.errors[0]);
                          return (
                            <FormField label="密码">
                              <div className="relative">
                                <Input
                                  type={showPassword ? "text" : "password"}
                                  placeholder="••••••••"
                                  value={field.state.value}
                                  onChange={(e) => field.handleChange(e.target.value)}
                                  onBlur={field.handleBlur}
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
                              {error && (
                                <p className="text-xs text-destructive mt-1">{error}</p>
                              )}
                            </FormField>
                          );
                        }}
                      </form.Field>
                    </div>
                  </>
                )}

                {/* Transmission Config */}
                {isTransmission && (
                  <>
                    {/* URL */}
                    <form.Field name="downloader.configs.transmission.url">
                      {(field) => {
                        const error = getErrorMessage(field.state.meta.errors[0]);
                        return (
                          <FormField label="服务器地址">
                            <Input
                              type="url"
                              placeholder="http://localhost:9091/transmission/rpc"
                              value={field.state.value}
                              onChange={(e) => field.handleChange(e.target.value)}
                              onBlur={field.handleBlur}
                            />
                            {error && (
                              <p className="text-xs text-destructive mt-1">{error}</p>
                            )}
                          </FormField>
                        );
                      }}
                    </form.Field>

                    {/* Username & Password (Optional) */}
                    <div className="grid gap-4 sm:grid-cols-2">
                      <form.Field name="downloader.configs.transmission.username">
                        {(field) => {
                          const error = getErrorMessage(field.state.meta.errors[0]);
                          return (
                            <FormField label="用户名 (可选)">
                              <Input
                                type="text"
                                placeholder="admin"
                                value={field.state.value}
                                onChange={(e) => field.handleChange(e.target.value)}
                                onBlur={field.handleBlur}
                              />
                              {error && (
                                <p className="text-xs text-destructive mt-1">{error}</p>
                              )}
                            </FormField>
                          );
                        }}
                      </form.Field>

                      <form.Field name="downloader.configs.transmission.password">
                        {(field) => {
                          const error = getErrorMessage(field.state.meta.errors[0]);
                          return (
                            <FormField label="密码 (可选)">
                              <div className="relative">
                                <Input
                                  type={showPassword ? "text" : "password"}
                                  placeholder="••••••••"
                                  value={field.state.value}
                                  onChange={(e) => field.handleChange(e.target.value)}
                                  onBlur={field.handleBlur}
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
                              {error && (
                                <p className="text-xs text-destructive mt-1">{error}</p>
                              )}
                            </FormField>
                          );
                        }}
                      </form.Field>
                    </div>
                  </>
                )}
              </>
            );
          }}
        </form.Subscribe>

        {/* Test Connection */}
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

        {/* Save Path (shared across all downloaders) */}
        <form.Field name="downloader.save_path">
          {(field) => {
            const error = getErrorMessage(field.state.meta.errors[0]);
            return (
              <FormField label="默认保存路径">
                <div className="relative">
                  <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                    <IconFolder className="size-4" />
                  </div>
                  <Input
                    type="text"
                    placeholder="/Media/Bangumi"
                    value={field.state.value}
                    onChange={(e) => field.handleChange(e.target.value)}
                    onBlur={field.handleBlur}
                    className="pl-9"
                  />
                </div>
                {error && (
                  <p className="text-xs text-destructive mt-1">{error}</p>
                )}
              </FormField>
            );
          }}
        </form.Field>
      </div>
    </section>
  );
}
