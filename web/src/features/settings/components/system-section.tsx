import * as React from "react";
import { useQuery, useMutation } from "@tanstack/react-query";
import { getApiAbout, postApiUpdate } from "@/client/sdk.gen";
import { toast } from "sonner";
import { Button } from "@/components/ui/button";
import { Spinner } from "@/components/ui/spinner";
import {
  IconDownload,
  IconCheck,
  IconAlertCircle,
  IconChevronDown,
  IconChevronUp,
} from "@tabler/icons-react";

export function SystemSection() {
  const [expandedChangelog, setExpandedChangelog] = React.useState(false);

  // 查询版本信息
  const aboutQuery = useQuery({
    queryKey: ["about"],
    queryFn: async () => {
      const { data } = await getApiAbout({
        throwOnError: true,
      });
      return data;
    },
    staleTime: 5 * 60 * 1000, // 5分钟
  });

  // 执行更新
  const updateMutation = useMutation({
    mutationFn: async () => {
      const { data } = await postApiUpdate({
        throwOnError: true,
      });
      return data;
    },
    onSuccess: (data) => {
      if (data.success) {
        toast.success("更新开始，程序即将重启");
        // 等待后端重启后刷新页面以显示新版本
        setTimeout(() => {
          window.location.reload();
        }, 3000);
      } else {
        toast.error(data.message || "更新失败");
      }
    },
    onError: () => {
      toast.error("更新请求失败");
    },
  });

  const isLoading = aboutQuery.isLoading;
  const isError = aboutQuery.isError;
  const about = aboutQuery.data;

  // 渲染加载状态
  if (isLoading) {
    return (
      <section className="space-y-5">
        <div className="rounded-lg border border-border/50 bg-muted/20 p-4 flex items-center gap-3">
          <Spinner className="size-4" />
          <span className="text-sm text-muted-foreground">检查版本信息中...</span>
        </div>
      </section>
    );
  }

  // 渲染错误状态
  if (isError || !about) {
    return (
      <section className="space-y-5">
        <div className="rounded-lg border border-destructive/30 bg-destructive/5 p-4 flex items-start gap-3">
          <IconAlertCircle className="size-4 text-destructive mt-0.5 shrink-0" />
          <span className="text-sm text-destructive">
            无法获取版本信息，请检查网络连接
          </span>
        </div>
      </section>
    );
  }

  const hasUpdate = about.needUpdate;
  const publishedDate = about.publishedAt
    ? new Date(about.publishedAt).toLocaleDateString("zh-CN")
    : null;

  return (
    <section className="space-y-5">
      {/* 版本信息卡片 */}
      <div className="rounded-lg border border-border/50 bg-muted/20 p-4 space-y-3">
        <div className="flex items-center justify-between">
          <div>
            <p className="text-xs text-muted-foreground mb-1">当前版本</p>
            <p className="text-sm font-mono text-foreground">{about.version}</p>
          </div>
          <div className="text-right">
            <p className="text-xs text-muted-foreground mb-1">最新版本</p>
            <p className="text-sm font-mono text-foreground">{about.latest}</p>
          </div>
        </div>

        {publishedDate && (
          <div className="flex items-center justify-between text-xs">
            <span className="text-muted-foreground">发布日期</span>
            <span className="text-foreground">{publishedDate}</span>
          </div>
        )}
      </div>

      {/* 状态指示 + 更新按钮 */}
      <div className="flex items-center gap-3">
        {hasUpdate ? (
          <>
            <Button
              type="button"
              onClick={() => updateMutation.mutate()}
              disabled={updateMutation.isPending}
              size="sm"
            >
              {updateMutation.isPending ? (
                <>
                  <Spinner className="size-4" />
                  <span>更新中...</span>
                </>
              ) : (
                <>
                  <IconDownload className="size-4" />
                  <span>立即更新</span>
                </>
              )}
            </Button>
            <span className="text-xs text-emerald-600">有新版本可用</span>
          </>
        ) : (
          <div className="flex items-center gap-2">
            <IconCheck className="size-4 text-emerald-600" />
            <span className="text-xs text-emerald-600">已是最新版本</span>
          </div>
        )}
      </div>

      {/* 更新日志 */}
      {about.changelog && (
        <div className="rounded-lg border border-border/50 bg-muted/20 p-4">
          <button
            type="button"
            onClick={() => setExpandedChangelog(!expandedChangelog)}
            className="flex items-center gap-2 text-sm text-muted-foreground hover:text-foreground transition-colors w-full"
          >
            {expandedChangelog ? (
              <IconChevronUp className="size-4" />
            ) : (
              <IconChevronDown className="size-4" />
            )}
            <span>更新日志</span>
          </button>

          {expandedChangelog && (
            <div className="mt-3 p-3 bg-muted/30 rounded-md border border-border/20">
              <pre className="text-xs text-muted-foreground whitespace-pre-wrap wrap-break-word font-mono overflow-hidden">
                {about.changelog}
              </pre>
            </div>
          )}
        </div>
      )}
    </section>
  );
}
