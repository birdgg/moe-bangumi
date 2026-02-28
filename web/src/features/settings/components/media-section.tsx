import * as React from "react";
import { Input } from "@/components/ui/input";
import {
  IconEye,
  IconEyeOff,
  IconKey,
  IconLink,
  IconServer,
} from "@tabler/icons-react";
import { FormField } from "./shared";
import { type SettingsFormInstance } from "../hooks";

export interface MediaSectionProps {
  form: SettingsFormInstance;
}

export function MediaSection({ form }: MediaSectionProps) {
  const [showToken, setShowToken] = React.useState(false);

  return (
    <section className="space-y-5">
      <div className="space-y-4">
        <div className="rounded-xl border border-border/50 bg-muted/20 p-4">
          <div className="flex items-center gap-2 pb-4">
            <IconServer className="size-5 text-amber-500" />
            <span className="text-sm font-medium">Plex</span>
          </div>

          <div className="space-y-4">
            <form.Field name="media.plexUrl">
              {(field) => (
                <FormField label="Plex URL">
                  <div className="relative">
                    <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                      <IconLink className="size-4" />
                    </div>
                    <Input
                      type="text"
                      placeholder="http://localhost:32400"
                      value={field.state.value ?? ""}
                      onChange={(e) => field.handleChange(e.target.value)}
                      onBlur={field.handleBlur}
                      className="pl-9"
                    />
                  </div>
                </FormField>
              )}
            </form.Field>

            <form.Field name="media.plexToken">
              {(field) => (
                <FormField label="Plex Token">
                  <div className="relative">
                    <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                      <IconKey className="size-4" />
                    </div>
                    <Input
                      type={showToken ? "text" : "password"}
                      placeholder="xxxxxxxxxxxxxxxxxxxxxxxxx"
                      value={field.state.value ?? ""}
                      onChange={(e) => field.handleChange(e.target.value)}
                      onBlur={field.handleBlur}
                      className="pl-9 pr-10"
                    />
                    <button
                      type="button"
                      onClick={() => setShowToken(!showToken)}
                      className="absolute right-2 top-1/2 -translate-y-1/2 p-1 text-muted-foreground transition-colors hover:text-foreground cursor-pointer"
                    >
                      {showToken ? (
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
              如何获取 Plex Token？
            </p>
            <ul className="mt-1 text-xs text-muted-foreground list-disc list-inside space-y-0.5">
              <li>
                登录 Plex Web App，打开任意媒体，在 URL 中查看{" "}
                <code className="text-chart-1">X-Plex-Token</code> 参数
              </li>
              <li>
                或在{" "}
                <a
                  href="https://support.plex.tv/articles/204059436-finding-an-authentication-token-x-plex-token/"
                  target="_blank"
                  rel="noopener noreferrer"
                  className="text-chart-1 hover:underline"
                >
                  Plex 官方文档
                </a>{" "}
                查看详细说明
              </li>
            </ul>
            <p className="mt-2 text-xs text-muted-foreground">
              配置后，番剧下载完成时将自动触发 Plex 媒体库刷新。
            </p>
          </div>
        </div>
      </div>
    </section>
  );
}
