import * as React from "react";
import { Input } from "@/components/ui/input";
import {
  IconEye,
  IconEyeOff,
  IconKey,
  IconBrandOpenSource,
} from "@tabler/icons-react";
import { FormField } from "./shared";
import { type SettingsFormInstance } from "../hooks";

export interface MetadataSectionProps {
  form: SettingsFormInstance;
}

export function MetadataSection({ form }: MetadataSectionProps) {
  const [showTmdbApiKey, setShowTmdbApiKey] = React.useState(false);

  return (
    <section className="space-y-5">
      <div className="space-y-4">
        <div className="rounded-lg border border-border/50 bg-muted/20 p-4">
          <div className="flex items-center gap-2 pb-3">
            <IconBrandOpenSource className="size-4 text-chart-1" />
            <span className="text-sm font-medium">TMDB API</span>
          </div>
          <p className="pb-3 text-xs text-muted-foreground">
            用于获取动画海报和元数据信息。可在{" "}
            <a
              href="https://www.themoviedb.org/settings/api"
              target="_blank"
              rel="noopener noreferrer"
              className="text-chart-1 hover:underline"
            >
              TMDB 官网
            </a>{" "}
            免费申请 API Key。
          </p>

          <form.Field name="tmdb.apiKey">
            {(field) => (
              <FormField label="API Key">
                <div className="relative">
                  <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                    <IconKey className="size-4" />
                  </div>
                  <Input
                    type={showTmdbApiKey ? "text" : "password"}
                    placeholder="输入你的 TMDB API Key"
                    value={field.state.value ?? ""}
                    onChange={(e) => field.handleChange(e.target.value)}
                    onBlur={field.handleBlur}
                    className="pl-9 pr-10 font-mono text-sm"
                  />
                  <button
                    type="button"
                    onClick={() => setShowTmdbApiKey(!showTmdbApiKey)}
                    className="absolute right-2 top-1/2 -translate-y-1/2 p-1 text-muted-foreground transition-colors hover:text-foreground cursor-pointer"
                  >
                    {showTmdbApiKey ? (
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
      </div>
    </section>
  );
}
