import * as React from "react";
import { useMutation } from "@tanstack/react-query";
import { postApiNotificationTest } from "@/client/sdk.gen";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Spinner } from "@/components/ui/spinner";
import {
  IconEye,
  IconEyeOff,
  IconBrandTelegram,
  IconKey,
  IconHash,
  IconPlugConnected,
} from "@tabler/icons-react";
import { FormField } from "./shared";
import { type SettingsFormInstance } from "../hooks";

export interface NotificationSectionProps {
  form: SettingsFormInstance;
}

export function NotificationSection({ form }: NotificationSectionProps) {
  const [showToken, setShowToken] = React.useState(false);

  const testMutation = useMutation({
    mutationFn: async () => {
      const values = form.state.values.notification.telegram;
      const { data } = await postApiNotificationTest({
        body: {
          botToken: values.botToken ?? "",
          chatId: values.chatId ?? "",
        },
        throwOnError: true,
      });
      return data;
    },
  });

  const testResult = testMutation.data;
  const testError = testMutation.isError;

  return (
    <section className="space-y-5">
      <div className="space-y-4">
        <div className="rounded-xl border border-border/50 bg-muted/20 p-4">
          <div className="flex items-center gap-2 pb-4">
            <IconBrandTelegram className="size-5 text-[#2AABEE]" />
            <span className="text-sm font-medium">Telegram</span>
          </div>

          <div className="space-y-4">
            <form.Field name="notification.telegram.botToken">
              {(field) => (
                <FormField label="Bot Token">
                  <div className="relative">
                    <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                      <IconKey className="size-4" />
                    </div>
                    <Input
                      type={showToken ? "text" : "password"}
                      placeholder="123456789:ABCdefGHIjklMNOpqrSTUvwxYZ"
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

            <form.Field name="notification.telegram.chatId">
              {(field) => (
                <FormField label="Chat ID">
                  <div className="relative">
                    <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                      <IconHash className="size-4" />
                    </div>
                    <Input
                      type="text"
                      placeholder="-1001234567890"
                      value={field.state.value ?? ""}
                      onChange={(e) => field.handleChange(e.target.value)}
                      onBlur={field.handleBlur}
                      className="pl-9"
                    />
                  </div>
                </FormField>
              )}
            </form.Field>
          </div>

          <form.Subscribe
            selector={(state) => state.values.notification.telegram}
          >
            {(telegram) => (
              <div className="flex items-center gap-2 mt-4">
                <Button
                  type="button"
                  variant="outline"
                  size="sm"
                  disabled={
                    testMutation.isPending ||
                    !telegram.botToken ||
                    !telegram.chatId
                  }
                  onClick={() => testMutation.mutate()}
                >
                  {testMutation.isPending ? (
                    <>
                      <Spinner className="size-4" />
                      <span>测试中...</span>
                    </>
                  ) : (
                    <>
                      <IconPlugConnected className="size-4" />
                      <span>发送测试消息</span>
                    </>
                  )}
                </Button>
                {testResult?.success && (
                  <span className="text-xs text-emerald-600">
                    发送成功
                  </span>
                )}
                {testResult && !testResult.success && (
                  <span className="text-xs text-destructive">
                    {testResult.message}
                  </span>
                )}
                {testError && (
                  <span className="text-xs text-destructive">发送失败</span>
                )}
              </div>
            )}
          </form.Subscribe>

          <div className="mt-4 pt-4 border-t border-border/30">
            <p className="text-xs text-muted-foreground">
              如何获取 Bot Token 和 Chat ID？
            </p>
            <ul className="mt-1 text-xs text-muted-foreground list-disc list-inside space-y-0.5">
              <li>
                使用{" "}
                <a
                  href="https://t.me/BotFather"
                  target="_blank"
                  rel="noopener noreferrer"
                  className="text-chart-1 hover:underline"
                >
                  @BotFather
                </a>{" "}
                创建 Bot 获取 Token
              </li>
              <li>
                使用{" "}
                <a
                  href="https://t.me/userinfobot"
                  target="_blank"
                  rel="noopener noreferrer"
                  className="text-chart-1 hover:underline"
                >
                  @userinfobot
                </a>{" "}
                获取你的 Chat ID
              </li>
            </ul>
          </div>
        </div>
      </div>
    </section>
  );
}
