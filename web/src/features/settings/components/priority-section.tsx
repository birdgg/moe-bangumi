import * as React from "react";
import { cn } from "@/lib/utils";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import {
  IconPlus,
  IconTrash,
  IconGripVertical,
} from "@tabler/icons-react";
import { type SettingsFormInstance } from "../hooks";
import { Reorder, useDragControls } from "framer-motion";
import {
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
  label?: string;
}

function CombinationItem({ combo, index, label }: CombinationItemProps) {
  const controls = useDragControls();
  // Create a stable key from the sorted combo
  const comboKey = [...combo].sort().join(",");

  return (
    <Reorder.Item
      value={comboKey}
      dragListener={false}
      dragControls={controls}
      className="flex items-center gap-2 rounded-lg border border-border/50 bg-muted/30 px-2 py-1.5 data-dragging:z-50 data-dragging:border-chart-1/50 data-dragging:bg-muted/80 data-dragging:shadow-lg"
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

      {/* Label or language badges */}
      <div className="flex flex-1 flex-wrap gap-1">
        {label ? (
          <span className="text-sm font-medium">{label}</span>
        ) : (
          combo.map((lang) => (
            <span
              key={lang}
              className="inline-flex items-center rounded-md bg-chart-1/10 px-2 py-0.5 text-xs font-medium text-chart-1 border border-chart-1/20"
            >
              {subtitleLanguageLabels[lang]}
            </span>
          ))
        )}
      </div>
    </Reorder.Item>
  );
}

// Preset language combinations
const LANGUAGE_PRESETS: { key: string; label: string; languages: SubtitleLanguage[] }[] = [
  { key: "chs", label: "简体", languages: ["CHS"] },
  { key: "chs_jpn", label: "简日", languages: ["CHS", "JPN"] },
  { key: "cht", label: "繁体", languages: ["CHT"] },
  { key: "cht_jpn", label: "繁日", languages: ["CHT", "JPN"] },
  { key: "chs_cht_jpn", label: "简繁日", languages: ["CHS", "CHT", "JPN"] },
];

interface LanguageCombinationListProps {
  title: string;
  description: string;
  icon: string;
  items: SubtitleLanguageSet[];
  onReorder: (items: SubtitleLanguageSet[]) => void;
}

function LanguageCombinationList({
  title,
  description,
  icon,
  items,
  onReorder,
}: LanguageCombinationListProps) {
  // Create stable keys for reordering
  const comboKeys = items.map((combo) => [...combo].sort().join(","));

  const handleReorder = (newKeys: string[]) => {
    const keyToCombo = new Map<string, SubtitleLanguageSet>();
    items.forEach((combo) => {
      keyToCombo.set([...combo].sort().join(","), combo);
    });
    const newItems = newKeys
      .map((key) => keyToCombo.get(key))
      .filter((c): c is SubtitleLanguageSet => c !== undefined);
    onReorder(newItems);
  };

  // Get display label for a combination
  const getComboLabel = (combo: SubtitleLanguageSet) => {
    const sortedCombo = [...combo].sort().join(",");
    const preset = LANGUAGE_PRESETS.find(
      (p) => [...p.languages].sort().join(",") === sortedCombo
    );
    return preset?.label;
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
              label={getComboLabel(combo)}
            />
          ))}
        </Reorder.Group>
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

  // Handler for language combination reordering
  const handleCombinationReorder = (newItems: SubtitleLanguageSet[]) => {
    form.setFieldValue("priority.subtitle_language_sets", newItems);
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
              description="拖拽调整优先级，越靠前优先级越高"
              icon="✨"
              items={languageSets}
              onReorder={handleCombinationReorder}
            />
          </>
        )}
      </form.Subscribe>
    </section>
  );
}
