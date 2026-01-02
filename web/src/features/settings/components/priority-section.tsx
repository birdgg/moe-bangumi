import * as React from "react";
import { cn } from "@/lib/utils";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { IconPlus, IconTrash, IconGripVertical } from "@tabler/icons-react";
import { type SettingsFormInstance } from "../hooks";
import { Reorder, useDragControls } from "framer-motion";
import {
  subtitleLanguages,
  subtitleLanguageLabels,
  type SubtitleLanguage,
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

interface LanguagePriorityListProps {
  title: string;
  description: string;
  icon: string;
  items: SubtitleLanguage[];
  onAdd: (item: SubtitleLanguage) => void;
  onRemove: (item: SubtitleLanguage) => void;
  onReorder: (items: SubtitleLanguage[]) => void;
}

function LanguagePriorityList({
  title,
  description,
  icon,
  items,
  onAdd,
  onRemove,
  onReorder,
}: LanguagePriorityListProps) {
  const [selectedLang, setSelectedLang] = React.useState<SubtitleLanguage | "">(
    ""
  );

  // Get available languages (not already in the list)
  const availableLanguages = subtitleLanguages.filter(
    (lang) => !items.includes(lang)
  );

  const handleAdd = () => {
    if (!selectedLang || items.includes(selectedLang)) return;
    onAdd(selectedLang);
    setSelectedLang("");
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

      {/* Add Select */}
      <div className="flex gap-2">
        <select
          value={selectedLang}
          onChange={(e) =>
            setSelectedLang(e.target.value as SubtitleLanguage | "")
          }
          className="flex-1 rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50"
        >
          <option value="">选择字幕语种...</option>
          {availableLanguages.map((lang) => (
            <option key={lang} value={lang}>
              {subtitleLanguageLabels[lang]} ({lang})
            </option>
          ))}
        </select>
        <Button
          type="button"
          size="sm"
          onClick={handleAdd}
          disabled={!selectedLang}
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
              displayText={`${subtitleLanguageLabels[item]} (${item})`}
              onRemove={() => onRemove(item)}
            />
          ))}
        </Reorder.Group>
      )}

      {/* Empty state hint */}
      {items.length === 0 && (
        <p className="text-xs text-muted-foreground/60 italic">
          未配置任何字幕语种优先级
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

  // Helper to create handlers for subtitle languages
  const createLanguageHandlers = () => {
    return {
      onAdd: (item: SubtitleLanguage) => {
        const currentItems = form.getFieldValue(
          "priority.subtitle_languages"
        ) as SubtitleLanguage[];
        form.setFieldValue("priority.subtitle_languages", [
          ...currentItems,
          item,
        ]);
      },
      onRemove: (item: SubtitleLanguage) => {
        const currentItems = form.getFieldValue(
          "priority.subtitle_languages"
        ) as SubtitleLanguage[];
        form.setFieldValue(
          "priority.subtitle_languages",
          currentItems.filter((i) => i !== item)
        );
      },
      onReorder: (newItems: SubtitleLanguage[]) => {
        form.setFieldValue("priority.subtitle_languages", newItems);
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
          优先级固定顺序：字幕组 &gt; 字幕语种。未配置的属性视为最低优先级。
        </p>
      </div>

      {/* Subscribe to priority values for reactivity */}
      <form.Subscribe
        selector={(state) => ({
          subtitleGroups: state.values.priority.subtitle_groups,
          subtitleLanguages: state.values.priority.subtitle_languages,
        })}
      >
        {({ subtitleGroups, subtitleLanguages: langs }) => (
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

            {/* Subtitle Languages */}
            <LanguagePriorityList
              title="字幕语种优先级"
              description="越靠前优先级越高"
              icon="✨"
              items={langs}
              {...createLanguageHandlers()}
            />
          </>
        )}
      </form.Subscribe>
    </section>
  );
}
