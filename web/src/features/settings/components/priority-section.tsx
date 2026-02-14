import * as React from "react";
import { cn } from "@/lib/utils";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import {
  IconPlus,
  IconTrash,
  IconGripVertical,
  IconX,
} from "@tabler/icons-react";
import { type SettingsFormInstance } from "../hooks";
import { Reorder, useDragControls } from "framer-motion";
import {
  type SubtitlePattern,
  type GroupFormData,
  subtitlePatternLabel,
  patternKey,
} from "../schema";

export interface PrioritySectionProps {
  form: SettingsFormInstance;
}

interface GroupReorderItemProps {
  group: GroupFormData;
  index: number;
  onRemove: () => void;
  onAddAlias: (alias: string) => void;
  onRemoveAlias: (aliasIndex: number) => void;
}

function GroupReorderItem({ group, index, onRemove, onAddAlias, onRemoveAlias }: GroupReorderItemProps) {
  const controls = useDragControls();
  const [newAlias, setNewAlias] = React.useState("");

  const handleAddAlias = () => {
    const trimmed = newAlias.trim();
    if (!trimmed || group.aliases.includes(trimmed)) return;
    onAddAlias(trimmed);
    setNewAlias("");
  };

  return (
    <Reorder.Item
      value={group.name}
      dragListener={false}
      dragControls={controls}
      className="group/item rounded-lg border border-border/50 bg-muted/30 data-dragging:z-50 data-dragging:border-chart-1/50 data-dragging:bg-muted/80 data-dragging:shadow-lg"
    >
      <div className="flex items-center gap-2 px-2.5 py-1.5">
        <span
          className={cn(
            "flex size-5 shrink-0 items-center justify-center rounded text-xs font-medium",
            index === 0
              ? "bg-chart-1/20 text-chart-1"
              : index === 1
                ? "bg-chart-3/20 text-chart-3"
                : index === 2
                  ? "bg-chart-5/20 text-chart-5"
                  : "bg-muted text-muted-foreground"
          )}
        >
          {index + 1}
        </span>

        <button
          type="button"
          className="cursor-grab touch-none text-muted-foreground/50 hover:text-muted-foreground active:cursor-grabbing"
          onPointerDown={(e) => controls.start(e)}
        >
          <IconGripVertical className="size-4 shrink-0" />
        </button>

        <span className="flex-1 text-sm font-medium">{group.name}</span>

        <Button
          type="button"
          variant="ghost"
          size="icon-sm"
          onClick={onRemove}
          className="size-6 shrink-0 cursor-pointer text-muted-foreground opacity-0 transition-opacity hover:bg-destructive/10 hover:text-destructive group-hover/item:opacity-100"
        >
          <IconTrash className="size-3.5" />
        </Button>
      </div>

      <div className="flex flex-wrap items-center gap-1.5 border-t border-border/30 px-2.5 pb-2 pt-1.5">
          {group.aliases.map((alias, aliasIdx) => (
            <span
              key={alias}
              className="inline-flex items-center gap-1 rounded-md border border-border/40 bg-muted px-2 py-0.5 text-xs text-muted-foreground"
            >
              {alias}
              <button
                type="button"
                onClick={() => onRemoveAlias(aliasIdx)}
                className="cursor-pointer text-muted-foreground/40 transition-colors hover:text-destructive"
              >
                <IconX className="size-3" />
              </button>
            </span>
          ))}
        <input
          type="text"
          placeholder="+ 别名"
          value={newAlias}
          onChange={(e) => setNewAlias(e.target.value)}
          onKeyDown={(e) => {
            if (e.key === "Enter") {
              e.preventDefault();
              handleAddAlias();
            }
            if (e.key === "Backspace" && !newAlias && group.aliases.length > 0) {
              onRemoveAlias(group.aliases.length - 1);
            }
          }}
          className="h-5 min-w-20 flex-1 bg-transparent text-xs text-muted-foreground outline-hidden placeholder:text-muted-foreground/40"
        />
      </div>
    </Reorder.Item>
  );
}

interface LanguagePatternItemProps {
  pattern: SubtitlePattern;
  index: number;
}

function LanguagePatternItem({ pattern, index }: LanguagePatternItemProps) {
  const controls = useDragControls();

  return (
    <Reorder.Item
      value={patternKey(pattern)}
      dragListener={false}
      dragControls={controls}
      className="flex items-center gap-2 rounded-lg border border-border/50 bg-muted/30 px-2 py-1.5 data-dragging:z-50 data-dragging:border-chart-1/50 data-dragging:bg-muted/80 data-dragging:shadow-lg"
    >
      <span
        className={cn(
          "flex size-5 shrink-0 items-center justify-center rounded text-xs font-medium",
          index === 0
            ? "bg-chart-1/20 text-chart-1"
            : index === 1
              ? "bg-chart-3/20 text-chart-3"
              : index === 2
                ? "bg-chart-5/20 text-chart-5"
                : "bg-muted text-muted-foreground"
        )}
      >
        {index + 1}
      </span>

      <button
        type="button"
        className="cursor-grab touch-none text-muted-foreground/50 hover:text-muted-foreground active:cursor-grabbing"
        onPointerDown={(e) => controls.start(e)}
      >
        <IconGripVertical className="size-4 shrink-0" />
      </button>

      <span className="flex-1 text-sm">{subtitlePatternLabel(pattern)}</span>
    </Reorder.Item>
  );
}

export function PrioritySection({ form }: PrioritySectionProps) {
  const [newGroup, setNewGroup] = React.useState("");

  const handleAddGroup = () => {
    const trimmed = newGroup.trim();
    if (!trimmed) return;
    const currentGroups = form.getFieldValue("priority.groups") as GroupFormData[];
    if (currentGroups.some((g) => g.name === trimmed)) return;
    form.setFieldValue("priority.groups", [...currentGroups, { name: trimmed, aliases: [] }]);
    setNewGroup("");
  };

  const handleRemoveGroup = (name: string) => {
    const currentGroups = form.getFieldValue("priority.groups") as GroupFormData[];
    form.setFieldValue(
      "priority.groups",
      currentGroups.filter((g) => g.name !== name)
    );
  };

  const handleReorderGroups = (newNames: string[]) => {
    const currentGroups = form.getFieldValue("priority.groups") as GroupFormData[];
    const nameToGroup = new Map(currentGroups.map((g) => [g.name, g]));
    const reordered = newNames.map((name) => nameToGroup.get(name)!).filter(Boolean);
    form.setFieldValue("priority.groups", reordered);
  };

  const handleAddAlias = (groupIndex: number, alias: string) => {
    const currentGroups = form.getFieldValue("priority.groups") as GroupFormData[];
    const updated = currentGroups.map((g, i) =>
      i === groupIndex ? { ...g, aliases: [...g.aliases, alias] } : g
    );
    form.setFieldValue("priority.groups", updated);
  };

  const handleRemoveAlias = (groupIndex: number, aliasIndex: number) => {
    const currentGroups = form.getFieldValue("priority.groups") as GroupFormData[];
    const updated = currentGroups.map((g, i) =>
      i === groupIndex ? { ...g, aliases: g.aliases.filter((_, ai) => ai !== aliasIndex) } : g
    );
    form.setFieldValue("priority.groups", updated);
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Enter") {
      e.preventDefault();
      handleAddGroup();
    }
  };

  const handleReorderLanguages = (newKeys: string[], currentPatterns: SubtitlePattern[]) => {
    const keyToPattern = new Map(currentPatterns.map(p => [patternKey(p), p]));
    const newPatterns = newKeys.map(key => keyToPattern.get(key)!).filter(Boolean);
    form.setFieldValue("priority.languages", newPatterns);
  };

  return (
    <section className="space-y-6">
      <div className="rounded-xl bg-linear-to-br from-chart-1/5 to-chart-3/5 p-4 border border-chart-1/20">
        <p className="text-sm text-muted-foreground">
          配置资源选择的优先级顺序。当多个资源可用时，系统会自动选择优先级最高的资源下载。
        </p>
      </div>

      <form.Subscribe
        selector={(state) => ({
          groups: state.values.priority.groups,
          languages: state.values.priority.languages,
        })}
      >
        {({ groups, languages }) => (
          <>
            <div className="space-y-3">
              <div className="flex items-start gap-2">
                <span className="text-base">♡</span>
                <div>
                  <h4 className="text-sm font-medium text-foreground">字幕组优先级</h4>
                  <p className="text-xs text-muted-foreground">越靠前优先级越高</p>
                </div>
              </div>

              <div className="flex gap-2">
                <Input
                  type="text"
                  placeholder="输入字幕组名称..."
                  value={newGroup}
                  onChange={(e) => setNewGroup(e.target.value)}
                  onKeyDown={handleKeyDown}
                  className="flex-1 text-sm"
                />
                <Button
                  type="button"
                  size="sm"
                  onClick={handleAddGroup}
                  disabled={!newGroup.trim() || groups.some((g) => g.name === newGroup.trim())}
                  className="gap-1 cursor-pointer"
                >
                  <IconPlus className="size-3.5" />
                  添加
                </Button>
              </div>

              {groups.length > 0 && (
                <Reorder.Group
                  axis="y"
                  values={groups.map((g) => g.name)}
                  onReorder={handleReorderGroups}
                  className="space-y-1.5"
                >
                  {groups.map((group, index) => (
                    <GroupReorderItem
                      key={group.name}
                      group={group}
                      index={index}
                      onRemove={() => handleRemoveGroup(group.name)}
                      onAddAlias={(alias) => handleAddAlias(index, alias)}
                      onRemoveAlias={(aliasIdx) => handleRemoveAlias(index, aliasIdx)}
                    />
                  ))}
                </Reorder.Group>
              )}
            </div>

            <div className="space-y-3">
              <div className="flex items-start gap-2">
                <span className="text-base">*</span>
                <div>
                  <h4 className="text-sm font-medium text-foreground">字幕语言优先级</h4>
                  <p className="text-xs text-muted-foreground">越靠前优先级越高，拖拽调整顺序</p>
                </div>
              </div>

              {languages.length > 0 && (
                <Reorder.Group
                  axis="y"
                  values={languages.map(patternKey)}
                  onReorder={(newKeys) => handleReorderLanguages(newKeys, languages)}
                  className="space-y-1"
                >
                  {languages.map((pattern, index) => (
                    <LanguagePatternItem
                      key={patternKey(pattern)}
                      pattern={pattern}
                      index={index}
                    />
                  ))}
                </Reorder.Group>
              )}
            </div>
          </>
        )}
      </form.Subscribe>
    </section>
  );
}
