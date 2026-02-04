import * as React from "react";
import { Input } from "@/components/ui/input";
import {
  IconEye,
  IconEyeOff,
  IconKey,
  IconServer,
  IconLink,
} from "@tabler/icons-react";
import { FormField } from "./shared";
import { type SettingsFormInstance } from "../hooks";

export interface MediaLibrarySectionProps {
  form: SettingsFormInstance;
}

export function MediaLibrarySection({ form }: MediaLibrarySectionProps) {
  const [showEmbyApiKey, setShowEmbyApiKey] = React.useState(false);

  return (
    <section className="space-y-5">
      <div className="space-y-4">
        <div className="rounded-lg border border-border/50 bg-muted/20 p-4">
          <div className="flex items-center gap-2 pb-3">
            <IconServer className="size-4 text-emerald-500" />
            <span className="text-sm font-medium">Emby</span>
          </div>
          <p className="pb-3 text-xs text-muted-foreground">
            连接 Emby 媒体服务器，可从媒体库导入番剧信息。填写服务器地址和 API Key 后即可使用。
          </p>

          <div className="space-y-4">
            <form.Field name="emby.url">
              {(field) => (
                <FormField label="服务器地址">
                  <div className="relative">
                    <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                      <IconLink className="size-4" />
                    </div>
                    <Input
                      type="url"
                      placeholder="http://localhost:8096"
                      value={field.state.value ?? ""}
                      onChange={(e) => field.handleChange(e.target.value)}
                      onBlur={field.handleBlur}
                      className="pl-9"
                    />
                  </div>
                </FormField>
              )}
            </form.Field>

            <form.Field name="emby.apiKey">
              {(field) => (
                <FormField label="API Key">
                  <div className="relative">
                    <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                      <IconKey className="size-4" />
                    </div>
                    <Input
                      type={showEmbyApiKey ? "text" : "password"}
                      placeholder="输入你的 Emby API Key"
                      value={field.state.value ?? ""}
                      onChange={(e) => field.handleChange(e.target.value)}
                      onBlur={field.handleBlur}
                      className="pl-9 pr-10 font-mono text-sm"
                    />
                    <button
                      type="button"
                      onClick={() => setShowEmbyApiKey(!showEmbyApiKey)}
                      className="absolute right-2 top-1/2 -translate-y-1/2 p-1 text-muted-foreground transition-colors hover:text-foreground cursor-pointer"
                    >
                      {showEmbyApiKey ? (
                        <IconEyeOff className="size-4" />
                      ) : (
                        <IconEye className="size-4" />
                      )}
                    </button>
                  </div>
                </FormField>
              )}
            </form.Field>
          </div>

          <div className="mt-4 pt-4 border-t border-border/30">
            <p className="text-xs text-muted-foreground">
              如何获取 API Key？
            </p>
            <ul className="mt-1 text-xs text-muted-foreground list-disc list-inside space-y-0.5">
              <li>在 Emby 服务器中进入「设置」→「API 密钥」</li>
              <li>点击「新建 API 密钥」并输入应用名称</li>
            </ul>
          </div>
        </div>
      </div>
    </section>
  );
}
