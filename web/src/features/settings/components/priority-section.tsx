import * as React from "react";
import { cn } from "@/lib/utils";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import {
  IconPlus,
  IconTrash,
  IconGripVertical,
  IconCheck,
} from "@tabler/icons-react";
import { type SettingsFormInstance } from "../hooks";
import { Reorder, useDragControls } from "framer-motion";
import {
  subtitleLanguages,
  subtitleLanguageLabels,
  type SubtitleLanguage,
  type SubtitleLanguageSet,
} from "../schema";

export interface PrioritySectionProps {
  form: SettingsFormInstance;
}

interface PriorityListProps {
  title: string;
  description: string;
  icon: string;
  items: string[];
  placeholder: string;
  onAdd: (item: string) => void;
  onRemove: (item: string) => void;
  onReorder: (items: string[]) => void;
}

interface ReorderItemProps<T extends string> {
  item: T;
  index: number;
  displayText?: string;
  onRemove: () => void;
}

function ReorderItem<T extends string>({
  item,
  index,
  displayText,
  onRemove,
}: ReorderItemProps<T>) {
  const controls = useDragControls();

  return (
    <Reorder.Item
      value={item}
      dragListener={false}
      dragControls={controls}
      className="group flex items-center gap-2 rounded-lg border border-border/50 bg-muted/30 px-2 py-1.5 data-dragging:z-50 data-dragging:border-chart-1/50 data-dragging:bg-muted/80 data-dragging:shadow-lg"
    >
      {/* Rank badge */}
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

      {/* Drag handle */}
      <button
        type="button"
        className="cursor-grab touch-none text-muted-foreground/50 hover:text-muted-foreground active:cursor-grabbing"
        onPointerDown={(e) => controls.start(e)}
      >
        <IconGripVertical className="size-4 shrink-0" />
      </button>

      {/* Item text */}
      <span className="flex-1 truncate text-sm">{displayText ?? item}</span>

      {/* Delete button */}
      <Button
        type="button"
        variant="ghost"
        size="icon-sm"
        onClick={onRemove}
        className="size-6 shrink-0 text-muted-foreground opacity-0 transition-opacity hover:bg-destructive/10 hover:text-destructive group-hover:opacity-100"
      >
        <IconTrash className="size-3.5" />
      </Button>
    </Reorder.Item>
  );
}

function PriorityList({
  title,
  description,
  icon,
  items,
  placeholder,
  onAdd,
  onRemove,
  onReorder,
}: PriorityListProps) {
  const [newItem, setNewItem] = React.useState("");

  const handleAdd = () => {
    const trimmed = newItem.trim();
    if (!trimmed || items.includes(trimmed)) return;
    onAdd(trimmed);
    setNewItem("");
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Enter") {
      e.preventDefault();
      handleAdd();
    }
  };

  return (
    <div className="space-y-3">
      {/* Header */}
      <div className="flex items-start gap-2">
        <span className="text-base">{icon}</span>
        <div>
          <h4 className="text-sm font-medium text-foreground">{title}</h4>
          <p className="text-xs text-muted-foreground">{description}</p>
        </div>
      </div>

      {/* Add Input */}
      <div className="flex gap-2">
        <Input
          type="text"
          placeholder={placeholder}
          value={newItem}
          onChange={(e) => setNewItem(e.target.value)}
          onKeyDown={handleKeyDown}
          className="flex-1 text-sm"
        />
        <Button
          type="button"
          size="sm"
          onClick={handleAdd}
          disabled={!newItem.trim() || items.includes(newItem.trim())}
          className="gap-1"
        >
          <IconPlus className="size-3.5" />
          添加
        </Button>
      </div>

      {/* Item List */}
      {items.length > 0 && (
        <Reorder.Group
          axis="y"
          values={items}
          onReorder={onReorder}
          className="space-y-1"
        >
          {items.map((item, index) => (
            <ReorderItem
              key={item}
              item={item}
              index={index}
              onRemove={() => onRemove(item)}
            />
          ))}
        </Reorder.Group>
      )}
    </div>
  );
}

// Language combination item for reordering
interface CombinationItemProps {
  combo: SubtitleLanguageSet;
  index: number;
  onRemove: () => void;
}

function CombinationItem({ combo, index, onRemove }: CombinationItemProps) {
  const controls = useDragControls();
  // Create a stable key from the sorted combo
  const comboKey = [...combo].sort().join(",");

  return (
    <Reorder.Item
      value={comboKey}
      dragListener={false}
      dragControls={controls}
      className="group flex items-center gap-2 rounded-lg border border-border/50 bg-muted/30 px-2 py-1.5 data-dragging:z-50 data-dragging:border-chart-1/50 data-dragging:bg-muted/80 data-dragging:shadow-lg"
    >
      {/* Rank badge */}
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

      {/* Drag handle */}
      <button
        type="button"
        className="cursor-grab touch-none text-muted-foreground/50 hover:text-muted-foreground active:cursor-grabbing"
        onPointerDown={(e) => controls.start(e)}
      >
        <IconGripVertical className="size-4 shrink-0" />
      </button>

      {/* Language badges */}
      <div className="flex flex-1 flex-wrap gap-1">
        {combo.map((lang) => (
          <span
            key={lang}
            className="inline-flex items-center rounded-md bg-chart-1/10 px-2 py-0.5 text-xs font-medium text-chart-1 border border-chart-1/20"
          >
            {subtitleLanguageLabels[lang]}
          </span>
        ))}
      </div>

      {/* Delete button */}
      <Button
        type="button"
        variant="ghost"
        size="icon-sm"
        onClick={onRemove}
        className="size-6 shrink-0 text-muted-foreground opacity-0 transition-opacity hover:bg-destructive/10 hover:text-destructive group-hover:opacity-100"
      >
        <IconTrash className="size-3.5" />
      </Button>
    </Reorder.Item>
  );
}

interface LanguageCombinationListProps {
  title: string;
  description: string;
  icon: string;
  items: SubtitleLanguageSet[];
  onAdd: (combo: SubtitleLanguageSet) => void;
  onRemove: (index: number) => void;
  onReorder: (items: SubtitleLanguageSet[]) => void;
}

function LanguageCombinationList({
  title,
  description,
  icon,
  items,
  onAdd,
  onRemove,
  onReorder,
}: LanguageCombinationListProps) {
  const [isAdding, setIsAdding] = React.useState(false);
  const [selectedLangs, setSelectedLangs] = React.useState<
    Set<SubtitleLanguage>
  >(new Set());

  const toggleLanguage = (lang: SubtitleLanguage) => {
    setSelectedLangs((prev) => {
      const next = new Set(prev);
      if (next.has(lang)) {
        next.delete(lang);
      } else {
        next.add(lang);
      }
      return next;
    });
  };

  const handleAdd = () => {
    if (selectedLangs.size === 0) return;
    const combo = Array.from(selectedLangs).sort();
    // Check if this combination already exists
    const exists = items.some(
      (existing) =>
        existing.length === combo.length &&
        [...existing].sort().join(",") === combo.join(",")
    );
    if (exists) return;

    onAdd(combo);
    setSelectedLangs(new Set());
    setIsAdding(false);
  };

  const handleCancel = () => {
    setSelectedLangs(new Set());
    setIsAdding(false);
  };

  // Create stable keys for reordering
  const comboKeys = items.map((combo) => [...combo].sort().join(","));

  const handleReorder = (newKeys: string[]) => {
    // Map keys back to original combos
    const keyToCombo = new Map<string, SubtitleLanguageSet>();
    items.forEach((combo) => {
      keyToCombo.set([...combo].sort().join(","), combo);
    });
    const newItems = newKeys
      .map((key) => keyToCombo.get(key))
      .filter((c): c is SubtitleLanguageSet => c !== undefined);
    onReorder(newItems);
  };

  return (
    <div className="space-y-3">
      {/* Header */}
      <div className="flex items-start gap-2">
        <span className="text-base">{icon}</span>
        <div>
          <h4 className="text-sm font-medium text-foreground">{title}</h4>
          <p className="text-xs text-muted-foreground">{description}</p>
        </div>
      </div>

      {/* Explanation */}
      <div className="rounded-lg bg-muted/30 px-3 py-2 text-xs text-muted-foreground space-y-1">
        <div className="flex items-center gap-2">
          <span className="text-chart-1">●</span>
          <span>精确匹配：[简中 + 日语] 只会匹配同时包含这两种语言的资源</span>
        </div>
        <div className="flex items-center gap-2">
          <span className="text-chart-3">●</span>
          <span>未匹配组合：视为最低优先级</span>
        </div>
      </div>

      {/* Item List */}
      {items.length > 0 && (
        <Reorder.Group
          axis="y"
          values={comboKeys}
          onReorder={handleReorder}
          className="space-y-1"
        >
          {items.map((combo, index) => (
            <CombinationItem
              key={[...combo].sort().join(",")}
              combo={combo}
              index={index}
              onRemove={() => onRemove(index)}
            />
          ))}
        </Reorder.Group>
      )}

      {/* Add new combination */}
      {!isAdding ? (
        <Button
          type="button"
          variant="outline"
          size="sm"
          onClick={() => setIsAdding(true)}
          className="w-full gap-2 border-dashed"
        >
          <IconPlus className="size-4" />
          添加语言组合
        </Button>
      ) : (
        <div className="rounded-lg border border-border bg-muted/30 p-4 space-y-3">
          <h5 className="text-sm font-medium">选择语言组合</h5>

          {/* Language toggle buttons */}
          <div className="grid grid-cols-2 gap-2">
            {subtitleLanguages
              .filter((lang) => lang !== "UNKNOWN")
              .map((lang) => {
                const isSelected = selectedLangs.has(lang);
                return (
                  <button
                    key={lang}
                    type="button"
                    onClick={() => toggleLanguage(lang)}
                    className={cn(
                      "flex items-center gap-2 rounded-md border px-3 py-2 text-left transition-colors",
                      isSelected
                        ? "border-chart-1/50 bg-chart-1/10 text-chart-1"
                        : "border-border/50 bg-background hover:bg-muted/50"
                    )}
                  >
                    <span
                      className={cn(
                        "flex size-4 shrink-0 items-center justify-center rounded border",
                        isSelected
                          ? "border-chart-1 bg-chart-1 text-white"
                          : "border-muted-foreground/30"
                      )}
                    >
                      {isSelected && <IconCheck className="size-3" />}
                    </span>
                    <span className="text-sm flex-1">
                      {subtitleLanguageLabels[lang]} ({lang})
                    </span>
                  </button>
                );
              })}
          </div>

          {/* Preview */}
          {selectedLangs.size > 0 && (
            <div className="flex items-center gap-2 flex-wrap">
              <span className="text-xs text-muted-foreground">预览：</span>
              {Array.from(selectedLangs)
                .sort()
                .map((lang) => (
                  <span
                    key={lang}
                    className="inline-flex items-center rounded-md bg-chart-1/10 px-2 py-0.5 text-xs font-medium text-chart-1 border border-chart-1/20"
                  >
                    {subtitleLanguageLabels[lang]}
                  </span>
                ))}
            </div>
          )}

          {/* Actions */}
          <div className="flex gap-2 justify-end">
            <Button type="button" variant="ghost" size="sm" onClick={handleCancel}>
              取消
            </Button>
            <Button
              type="button"
              size="sm"
              onClick={handleAdd}
              disabled={selectedLangs.size === 0}
              className="gap-1"
            >
              <IconPlus className="size-3.5" />
              添加组合
            </Button>
          </div>
        </div>
      )}

      {/* Empty state hint */}
      {items.length === 0 && !isAdding && (
        <p className="text-xs text-muted-foreground/60 italic">
          未配置任何语言组合优先级
        </p>
      )}
    </div>
  );
}

export function PrioritySection({ form }: PrioritySectionProps) {
  // Helper to create handlers for subtitle groups
  const createGroupHandlers = () => {
    return {
      onAdd: (item: string) => {
        const currentItems = form.getFieldValue(
          "priority.subtitle_groups"
        ) as string[];
        form.setFieldValue("priority.subtitle_groups", [...currentItems, item]);
      },
      onRemove: (item: string) => {
        const currentItems = form.getFieldValue(
          "priority.subtitle_groups"
        ) as string[];
        form.setFieldValue(
          "priority.subtitle_groups",
          currentItems.filter((i) => i !== item)
        );
      },
      onReorder: (newItems: string[]) => {
        form.setFieldValue("priority.subtitle_groups", newItems);
      },
    };
  };

  // Helper to create handlers for language combinations
  const createCombinationHandlers = () => {
    return {
      onAdd: (combo: SubtitleLanguageSet) => {
        const currentItems = form.getFieldValue(
          "priority.subtitle_language_sets"
        ) as SubtitleLanguageSet[];
        form.setFieldValue("priority.subtitle_language_sets", [
          ...currentItems,
          combo,
        ]);
      },
      onRemove: (index: number) => {
        const currentItems = form.getFieldValue(
          "priority.subtitle_language_sets"
        ) as SubtitleLanguageSet[];
        form.setFieldValue(
          "priority.subtitle_language_sets",
          currentItems.filter((_, i) => i !== index)
        );
      },
      onReorder: (newItems: SubtitleLanguageSet[]) => {
        form.setFieldValue("priority.subtitle_language_sets", newItems);
      },
    };
  };

  return (
    <section className="space-y-6">
      {/* Header */}
      <div className="rounded-xl bg-linear-to-br from-chart-1/5 to-chart-3/5 p-4 border border-chart-1/20">
        <p className="text-sm text-muted-foreground">
          配置资源选择的优先级顺序。当多个资源可用时，系统会自动选择优先级最高的资源下载。
          如果发现更高优先级的资源，会自动替换（洗版）已下载的低优先级资源。
        </p>
        <p className="mt-2 text-xs text-muted-foreground/80">
          优先级固定顺序：字幕组 &gt; 字幕语言组合。未配置的属性视为最低优先级。
        </p>
      </div>

      {/* Subscribe to priority values for reactivity */}
      <form.Subscribe
        selector={(state) => ({
          subtitleGroups: state.values.priority.subtitle_groups,
          languageSets: state.values.priority.subtitle_language_sets,
        })}
      >
        {({ subtitleGroups, languageSets }) => (
          <>
            {/* Subtitle Groups */}
            <PriorityList
              title="字幕组优先级"
              description="越靠前优先级越高"
              icon="♡"
              items={subtitleGroups}
              placeholder="输入字幕组名称..."
              {...createGroupHandlers()}
            />

            {/* Subtitle Language Combinations */}
            <LanguageCombinationList
              title="字幕语言组合优先级"
              description="精确匹配语言组合，越靠前优先级越高"
              icon="✨"
              items={languageSets}
              {...createCombinationHandlers()}
            />
          </>
        )}
      </form.Subscribe>
    </section>
  );
}
